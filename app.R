library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DBI)
library(RSQLite)
library(tibble)

options(shiny.sanitize.errors = FALSE)
options(shiny.fullstacktrace = TRUE)

# ------------------ app dir ------------------
get_app_dir <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) return(normalizePath(dirname(ofile)))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(normalizePath(dirname(p)))
  }
  normalizePath(getwd())
}

app_dir <- get_app_dir()
message("app_dir = ", app_dir)

# sources
source(file.path(app_dir, "R", "utils.R"))
source(file.path(app_dir, "R", "parsers.R"))
source(file.path(app_dir, "R", "db.R"))

db_path <- file.path(app_dir, "depenses.sqlite")

# JSON-safe
dt_safe <- function(df) {
  df %>%
    mutate(
      across(where(is.factor), as.character),
      across(where(is.Date), as.character),
      across(where(is.POSIXt), as.character)
    ) %>%
    mutate(across(where(is.list), ~ vapply(.x, function(z) paste(z, collapse=" "), character(1)))) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

# colonnes affichées dans Transactions
tx_cols <- c("id","date","description","amount","direction","nature","source",
             "bank_category_raw","category","subcategory","is_recurring","notes")

# ------------------ UI ------------------
ui <- page_navbar(
  title = "Gestion des dépenses",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel("Importer",
            layout_columns(
              col_widths = c(5, 7),
              card(
                card_header("Importer"),
                radioButtons("source", "Source", choices = c("Compte"="Compte", "Crédit"="Crédit"), inline = TRUE),
                numericInput("year", "Année", value = year(Sys.Date()), min = 2000, max = year(Sys.Date()) + 1),
                textAreaInput("paste", "Coller ici", rows = 10),
                actionButton("btn_parse", "Prévisualiser", class = "btn-primary"),
                actionButton("btn_save", "Enregistrer", class = "btn-success"),
                tags$hr(),
                verbatimTextOutput("db_info")
              ),
              card(card_header("Prévisualisation"), DTOutput("preview"))
            )
  ),
  
  nav_panel("Transactions",
            layout_columns(
              col_widths = c(3, 9),
              card(
                card_header("Filtres / actions"),
                dateRangeInput("range", "Période", start = Sys.Date() - 365, end = Sys.Date()),
                selectInput("f_source", "Source", choices = c("Toutes","Compte","Crédit")),
                checkboxInput("exclude_transfers", "Exclure les transferts", TRUE),
                checkboxInput("f_rec", "Seulement récurrentes", FALSE),
                actionButton("btn_refresh", "Rafraîchir"),
                actionButton("btn_classify", "Classer la sélection", class = "btn-primary"),
                actionButton("btn_clear_cat", "Effacer catégorie", class = "btn-outline-danger"),
                actionButton("btn_clear_selection", "Réinitialiser sélection", class = "btn-secondary"),
                actionButton("btn_delete_rows", "Supprimer ligne(s)", class = "btn-danger")
              ),
              card(card_header("Liste"), DTOutput("tbl"))
            )
  ),
  
  nav_panel("Catégories",
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header("Ajouter catégorie / sous-catégorie"),
                textInput("new_cat", "Catégorie"),
                textInput("new_sub", "Sous-catégorie"),
                selectInput("new_dir", "Direction", choices = c("Retrait","Dépôt","Toutes"), selected = "Retrait"),
                actionButton("btn_add_cat", "Ajouter", class = "btn-primary")
              ),
              card(card_header("Liste"), DTOutput("cats"))
            )
  ),
  
  nav_panel("Rapports",
            layout_columns(
              col_widths = c(3, 9),
              card(
                card_header("Options"),
                selectInput(
                  "report_mode", "Type de rapport",
                  choices = c(
                    "Retraits (dépenses)" = "out",
                    "Dépôts (revenus)" = "in",
                    "Net (Dépôts - Retraits)" = "net"
                  ),
                  selected = "out"
                ),
                
                # ✅ AJOUT ICI
                checkboxInput("report_include_transfers", "Inclure les transferts dans le rapport", FALSE),
                
                tags$small("Masque Direction/Catégorie/Sous-catégorie via « Colonnes… » : le tableau se regroupe automatiquement. Utilise « Réinitialiser colonnes » si besoin.")
              ),
              card(card_header("Tableau croisé"), DTOutput("report"))
            )
  )
)

# ------------------ SERVER ------------------
server <- function(input, output, session){
  
  con <- dbConnect(SQLite(), db_path)
  onStop(function() dbDisconnect(con))
  db_init(con)
  
  # petit “tick” pour rafraîchir partout
  db_tick <- reactiveVal(0)
  bump_db <- function() db_tick(isolate(db_tick()) + 1)
  
  # info DB visible
  output$db_info <- renderText({
    n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM transactions")$n
    paste0("DB = ", db_path, "\nTransactions = ", n)
  })
  
  # ------------------ Importer ------------------
  rv <- reactiveValues(preview = tibble())
  
  observeEvent(input$btn_parse, {
    req(input$paste)
    if (input$source == "Compte") {
      rv$preview <- parse_compte_paste(input$paste, year = input$year, source = "Compte")
    } else {
      rv$preview <- parse_credit_paste(input$paste, year = input$year, source = "Crédit")
    }
    
    # assurer colonnes cat_* (si ton db.R les attend)
    if (!"cat_source" %in% names(rv$preview)) rv$preview$cat_source <- "manual"
    if (!"cat_confidence" %in% names(rv$preview)) rv$preview$cat_confidence <- NA_real_
  })
  
  output$preview <- renderDT({
    datatable(dt_safe(rv$preview), options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)
  
  observeEvent(input$btn_save, {
    req(rv$preview)
    df <- rv$preview %>%
      mutate(date = as.character(date),
             is_recurring = as.integer(is_recurring))
    
    # défauts
    if (!"cat_source" %in% names(df)) df$cat_source <- "manual"
    if (!"cat_confidence" %in% names(df)) df$cat_confidence <- NA_real_
    
    db_upsert_transactions(con, df)
    bump_db()
    showNotification("Import enregistré.", type = "message", duration = 2)
  })
  
  # ------------------ Transactions ------------------
  proxy_tbl <- DT::dataTableProxy("tbl")
  
  base_tx <- reactive({
    db_tick()
    
    df <- db_get_transactions(con)
    
    # filtre période SAFE même si range absent (mais ici range existe)
    start_date <- input$range[1]
    end_date   <- input$range[2]
    df <- df %>% filter(date >= start_date, date <= end_date)
    
    # filtres
    if (input$f_source != "Toutes") df <- df %>% filter(source == input$f_source)
    if (isTRUE(input$exclude_transfers)) df <- df %>% filter(is.na(nature) | nature != "Transfert")
    if (isTRUE(input$f_rec)) df <- df %>% filter(is_recurring == 1)
    
    # non classées en haut
    df <- df %>%
      mutate(
        category = as.character(category),
        is_uncat = is.na(category) | category == ""
      ) %>%
      arrange(desc(is_uncat), desc(date), desc(id)) %>%
      select(-is_uncat)
    
    df
  })
  
  output$tbl <- renderDT({
    df <- base_tx() %>% select(any_of(tx_cols))
    df <- dt_safe(df)
    
    datatable(
      df,
      rownames = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        stateSave = TRUE,
        stateDuration = -1
      )
    )
  }, server = FALSE)
  
  observeEvent(input$btn_refresh, {
    bump_db()
  })
  
  observeEvent(input$btn_clear_selection, {
    DT::selectRows(proxy_tbl, NULL)
  })
  
  # Effacer catégorie (met category/subcategory à NULL)
  observeEvent(input$btn_clear_cat, {
    sel <- input$tbl_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Sélectionne au moins une ligne.", type = "warning")
      return()
    }
    df <- base_tx()
    ids <- df$id[sel]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) return()
    
    db_clear_transactions_category(con, ids)
    bump_db()
    DT::selectRows(proxy_tbl, NULL)
    showNotification("Catégorie effacée.", type = "message", duration = 2)
  })
  
  # Classer la sélection (modal)
  observeEvent(input$btn_classify, {
    sel <- input$tbl_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Sélectionne au moins une ligne.", type = "warning")
      return()
    }
    
    cats_df <- db_get_categories(con)
    showModal(modalDialog(
      title = "Classer la sélection",
      selectizeInput("pick_cat", "Catégorie", choices = sort(unique(cats_df$category))),
      uiOutput("pick_sub_ui"),
      checkboxInput("pick_rec", "Marquer récurrente", FALSE),
      easyClose = TRUE,
      footer = tagList(modalButton("Annuler"),
                       actionButton("btn_apply_classify", "Appliquer", class = "btn-success"))
    ))
  })
  
  observeEvent(input$btn_delete_rows, {
    sel <- input$tbl_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Sélectionne au moins une ligne à supprimer.", type = "warning")
      return()
    }
    
    # Nombre de lignes sélectionnées
    n_sel <- length(sel)
    
    showModal(modalDialog(
      title = "Supprimer des transactions",
      tags$p(
        paste0("Tu es sur le point de supprimer ", n_sel, " transaction(s)."),
        tags$br(),
        tags$strong("Action irréversible.")
      ),
      tags$p("Pour confirmer, tape ", tags$strong("SUPPRIMER"), " :"),
      textInput("delete_confirm_text", label = NULL, placeholder = "SUPPRIMER"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("btn_confirm_delete_rows", "Supprimer", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_confirm_delete_rows, {
    txt <- input$delete_confirm_text
    if (is.null(txt) || toupper(trimws(txt)) != "SUPPRIMER") {
      showNotification("Confirmation incorrecte. Tape SUPPRIMER.", type = "warning")
      return()
    }
    
    sel <- input$tbl_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Aucune sélection.", type = "warning")
      removeModal()
      return()
    }
    
    # IMPORTANT : mapper la sélection vers les ids DB
    df <- base_tx()
    ids <- df$id[sel]
    ids <- ids[!is.na(ids)]
    
    if (length(ids) == 0) {
      showNotification("Impossible de trouver les IDs à supprimer.", type = "error")
      removeModal()
      return()
    }
    
    # Suppression DB
    tryCatch({
      n_del <- db_delete_transactions(con, ids)
      
      bump_db()                  # rafraîchit les données réactives
      DT::selectRows(proxy_tbl, NULL)  # enlève la sélection
      removeModal()
      
      showNotification(paste0("Supprimé: ", n_del, " transaction(s)."), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Erreur suppression:", e$message), type = "error", duration = NULL)
    })
  })
  
  output$pick_sub_ui <- renderUI({
    req(input$pick_cat)
    cats_df <- db_get_categories(con)
    subs <- cats_df %>% filter(category == input$pick_cat) %>% pull(subcategory) %>% unique() %>% sort()
    selectizeInput("pick_sub", "Sous-catégorie", choices = c("—" = "", subs))
  })
  
  observeEvent(input$btn_apply_classify, {
    req(input$pick_cat)
    sel <- input$tbl_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    
    df <- base_tx()
    ids <- df$id[sel]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) return()
    
    subval <- input$pick_sub
    if (is.null(subval) || !nzchar(subval)) subval <- NA_character_
    
    db_set_transactions_category(con, ids, input$pick_cat, subval, as.integer(isTRUE(input$pick_rec)))
    bump_db()
    removeModal()
    DT::selectRows(proxy_tbl, NULL)
    showNotification("Catégorie appliquée.", type = "message", duration = 2)
  })
  
  # ------------------ Catégories ------------------
  cats <- reactiveVal(db_get_categories(con))
  
  observeEvent(input$btn_add_cat, {
    req(nchar(input$new_cat) > 0, nchar(input$new_sub) > 0)
    db_add_category(con, input$new_cat, input$new_sub, input$new_dir)
    cats(db_get_categories(con))
    showNotification("Catégorie ajoutée.", type = "message", duration = 2)
  })
  
  output$cats <- renderDT({
    datatable(dt_safe(cats()), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  }, server = FALSE)
  
  # ------------------ Rapports (collapsable 3 colonnes) ------------------
  output$report <- DT::renderDT({
    db_tick()
    
    # --- visibilité des 3 dimensions (valeurs envoyées par JS) ---
    dir_visible <- input$report_dir_visible
    if (is.null(dir_visible)) dir_visible <- TRUE
    
    cat_visible <- input$report_cat_visible
    if (is.null(cat_visible)) cat_visible <- TRUE
    
    sub_visible <- input$report_sub_visible
    if (is.null(sub_visible)) sub_visible <- TRUE
    
    # --- mode robuste ---
    mode <- input$report_mode
    if (is.null(mode) || !nzchar(mode)) mode <- "out"
    
    # --- charger transactions ---
    df <- db_get_transactions(con) %>%
      mutate(
        month = format(floor_date(date, "month"), "%Y-%m"),
        
        # conversion robuste amount -> amount_num
        amount_chr = as.character(amount),
        amount_chr = str_replace_all(amount_chr, "[^0-9,\\.\\-\\+\\(\\)]", ""),
        amount_chr = str_replace_all(amount_chr, "\\s+", ""),
        amount_chr = if_else(
          str_detect(amount_chr, "^\\(.*\\)$"),
          paste0("-", str_replace_all(amount_chr, "[\\(\\)]", "")),
          amount_chr
        ),
        amount_chr = str_replace_all(amount_chr, ",", "."),
        amount_num = suppressWarnings(as.numeric(amount_chr)),
        
        # libellés
        category = if_else(is.na(category) | category == "", "Non classé", as.character(category)),
        subcategory = if_else(is.na(subcategory) | subcategory == "", "(Sans sous-catégorie)", as.character(subcategory)),
        
        # flux basés sur amount_num
        outflow = if_else(!is.na(amount_num) & amount_num < 0, -amount_num, 0),
        inflow  = if_else(!is.na(amount_num) & amount_num > 0,  amount_num, 0),
        
        # direction standardisée (affichage)
        direction = case_when(
          !is.na(amount_num) & amount_num > 0 ~ "Dépôt",
          !is.na(amount_num) & amount_num < 0 ~ "Retrait",
          TRUE ~ "Neutre"
        )
      ) %>%
      # transferts exclus (comme ton app actuelle)
      filter(is.na(nature) | nature != "Transfert")
    
    # ✅ Filtrer selon mode (Retraits/Dépôts seulement)
    if (mode == "out") {
      df <- df %>% filter(!is.na(amount_num) & amount_num < 0)
    } else if (mode == "in") {
      df <- df %>% filter(!is.na(amount_num) & amount_num > 0)
    } else {
      # net: garde tous les mouvements non nuls
      df <- df %>% filter(!is.na(amount_num) & amount_num != 0)
    }
    
    # ✅ Calcul value robuste (pas de case_when avec scalaire)
    df$value <- if (mode == "out") {
      df$outflow
    } else if (mode == "in") {
      df$inflow
    } else {
      df$inflow - df$outflow
    }
    
    # --- Collapsable : on garde les colonnes mais met vide si cachées ---
    # (colonnes restent ré-affichables via ColVis)
    if (!isTRUE(dir_visible)) df <- df %>% mutate(direction = "")
    if (!isTRUE(cat_visible)) df <- df %>% mutate(category  = "")
    if (!isTRUE(sub_visible)) df <- df %>% mutate(subcategory = "")
    
    keys <- c("direction", "category", "subcategory")
    
    # --- agréger ---
    agg <- df %>%
      group_by(across(all_of(keys)), month) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    # --- pivot mois -> colonnes ---
    pivot <- agg %>%
      pivot_wider(names_from = month, values_from = total, values_fill = 0)
    
    if (nrow(pivot) == 0) {
      return(DT::datatable(data.frame(Message = "Aucune donnée à afficher."), rownames = FALSE))
    }
    
    month_cols <- setdiff(names(pivot), keys)
    
    # --- moyenne mensuelle ---
    pivot <- pivot %>%
      mutate(`Moyenne mensuelle` = rowMeans(across(all_of(month_cols)), na.rm = TRUE))
    
    out <- dt_safe(pivot)
    
    # --- tableau + boutons + callback visibilité colonnes ---
    dt <- DT::datatable(
      out,
      rownames = FALSE,
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list(
          # ✅ ColVis liste aussi les colonnes cachées -> permet de les remettre
          list(extend = "colvis", text = "Colonnes…", columns = ":not(.noVis)"),
          # ✅ reset rapide des colonnes
          list(extend = "colvisRestore", text = "Réinitialiser colonnes"),
          list(extend = "csv", text = "Télécharger CSV")
        )
      ),
      callback = JS("
      // indices stables: direction=0, category=1, subcategory=2
      table.off('column-visibility.dt._toggle3');
      table.on('column-visibility.dt._toggle3', function(e, settings, column, state){
        if (column === 0) Shiny.setInputValue('report_dir_visible', state, {priority:'event'});
        if (column === 1) Shiny.setInputValue('report_cat_visible', state, {priority:'event'});
        if (column === 2) Shiny.setInputValue('report_sub_visible', state, {priority:'event'});
      });
    ")
    )
    
    # --- format $ ---
    dt <- dt %>% DT::formatCurrency(
      columns = c(month_cols, "Moyenne mensuelle"),
      currency = "$", digits = 2, interval = 3, mark = " ", dec.mark = ","
    )
    
    # --- net: négatif en rouge ---
    if (mode == "net") {
      dt <- dt %>% DT::formatStyle(
        columns = c(month_cols, "Moyenne mensuelle"),
        color = DT::styleInterval(0, c("red", "black"))
      )
    }
    
    dt
  }, server = FALSE)
  
}

shinyApp(ui, server)
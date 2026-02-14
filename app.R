library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
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
source(file.path(app_dir, "R", "utils.R"))
source(file.path(app_dir, "R", "parsers.R"))
source(file.path(app_dir, "R", "db.R"))

db_path <- file.path(app_dir, "depenses.sqlite")

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

# ------------------ UI ------------------
ui <- page_navbar(
  title = "Gestion des dépenses",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel("Importer",
            layout_columns(
              col_widths = c(5, 7),
              card(
                card_header("Importer (stub)"),
                radioButtons("source", "Source", choices = c("Compte"="Compte", "Crédit"="Crédit"), inline = TRUE),
                numericInput("year", "Année", value = year(Sys.Date()), min = 2000, max = year(Sys.Date()) + 1),
                textAreaInput("paste", "Coller ici", rows = 10),
                actionButton("btn_parse", "Prévisualiser", class = "btn-primary"),
                actionButton("btn_save", "Enregistrer", class = "btn-success")
              ),
              card(card_header("Prévisualisation"), DTOutput("preview"))
            )
  ),
  
  nav_panel("Transactions",
            layout_columns(
              col_widths = c(3, 9),
              card(
                card_header("Stub"),
                tags$p("Garde ici ta version complète Transactions si tu en as une.")
              ),
              card(card_header("Liste"), DTOutput("tbl"))
            )
  ),
  
  nav_panel("Catégories",
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header("Stub"),
                tags$p("Garde ici ta version complète Catégories si tu en as une.")
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
                tags$small("Masque Direction/Catégorie/Sous-catégorie via « Colonnes… » : le tableau se regroupe automatiquement.")
              ),
              card(
                card_header("Tableau croisé"),
                DTOutput("report")
              )
            )
  )
)

# ------------------ SERVER ------------------
server <- function(input, output, session){
  
  con <- dbConnect(SQLite(), db_path)
  onStop(function() dbDisconnect(con))
  db_init(con)
  
  db_tick <- reactiveVal(0)
  bump_db <- function() db_tick(isolate(db_tick()) + 1)
  
  # -------- stubs minimal pour que l'app tourne --------
  rv <- reactiveValues(preview = tibble())
  observeEvent(input$btn_parse, {
    if (input$source == "Compte") {
      rv$preview <- parse_compte_paste(input$paste, year = input$year, source = "Compte")
    } else {
      rv$preview <- parse_credit_paste(input$paste, year = input$year, source = "Crédit")
    }
  })
  
  output$preview <- renderDT({
    datatable(dt_safe(rv$preview), options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)
  
  observeEvent(input$btn_save, {
    req(rv$preview)
    df <- rv$preview %>% mutate(date = as.character(date), is_recurring = as.integer(is_recurring))
    if (!"cat_source" %in% names(df)) df$cat_source <- "manual"
    if (!"cat_confidence" %in% names(df)) df$cat_confidence <- NA_real_
    db_upsert_transactions(con, df)
    bump_db()
  })
  
  cats <- reactiveVal(db_get_categories(con))
  output$cats <- renderDT({
    datatable(dt_safe(cats()), options = list(pageLength = 12, scrollX = TRUE))
  }, server = FALSE)
  
  output$tbl <- renderDT({
    datatable(data.frame(Message = "Stub table"), rownames = FALSE)
  }, server = FALSE)
  
  # ------------------ RAPPORTS (VRAI FIX) ------------------
  output$report <- DT::renderDT({
    db_tick()
    
    # Valeurs de visibilité envoyées par JS (par défaut TRUE)
    dir_visible <- input$report_dir_visible; if (is.null(dir_visible)) dir_visible <- TRUE
    cat_visible <- input$report_cat_visible; if (is.null(cat_visible)) cat_visible <- TRUE
    sub_visible <- input$report_sub_visible; if (is.null(sub_visible)) sub_visible <- TRUE
    
    mode <- input$report_mode
    if (is.null(mode) || !nzchar(mode)) mode <- "out"
    
    df <- db_get_transactions(con) %>%
      mutate(
        month = format(floor_date(date, "month"), "%Y-%m"),
        direction = if_else(is.na(direction) | direction == "",
                            if_else(amount < 0, "Retrait", "Dépôt"),
                            as.character(direction)),
        category = if_else(is.na(category) | category == "", "Non classé", as.character(category)),
        subcategory = if_else(is.na(subcategory) | subcategory == "", "(Sans sous-catégorie)", as.character(subcategory)),
        outflow = if_else(amount < 0, -amount, 0),
        inflow  = if_else(amount > 0,  amount, 0)
      ) %>%
      filter(is.na(nature) | nature != "Transfert")
    
    # Mode valeur
    if (mode == "out") {
      df <- df %>% filter(direction == "Retrait") %>% mutate(value = outflow)
    } else if (mode == "in") {
      df <- df %>% filter(direction == "Dépôt") %>% mutate(value = inflow)
    } else {
      df <- df %>% mutate(value = inflow - outflow)
    }
    
    # ✅ IMPORTANT: les colonnes restent toujours présentes.
    # Si cachées, on les remplace par une valeur constante => regroupement automatique.
    if (!isTRUE(dir_visible)) df <- df %>% mutate(direction = "(Total)")
    if (!isTRUE(cat_visible)) df <- df %>% mutate(category = "(Total)")
    if (!isTRUE(sub_visible)) df <- df %>% mutate(subcategory = "(Total)")
    
    keys <- c("direction", "category", "subcategory")
    
    agg <- df %>%
      group_by(across(all_of(keys)), month) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    pivot <- agg %>%
      pivot_wider(names_from = month, values_from = total, values_fill = 0)
    
    if (nrow(pivot) == 0) {
      return(datatable(data.frame(Message = "Aucune donnée à afficher."), rownames = FALSE))
    }
    
    month_cols <- setdiff(names(pivot), keys)
    
    pivot <- pivot %>%
      mutate(`Moyenne mensuelle` = rowMeans(across(all_of(month_cols)), na.rm = TRUE))
    
    out <- dt_safe(pivot)
    
    # Index 0-based stables: direction=0, category=1, subcategory=2
    dt <- DT::datatable(
      out,
      rownames = FALSE,
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "colvis", text = "Colonnes…"),
          list(extend = "csv", text = "Télécharger CSV")
        )
      ),
      callback = JS("
        table.off('column-visibility.dt._toggle3');
        table.on('column-visibility.dt._toggle3', function(e, settings, column, state){
          if (column === 0) Shiny.setInputValue('report_dir_visible', state, {priority:'event'});
          if (column === 1) Shiny.setInputValue('report_cat_visible', state, {priority:'event'});
          if (column === 2) Shiny.setInputValue('report_sub_visible', state, {priority:'event'});
        });
      ")
    )
    
    dt <- dt %>% DT::formatCurrency(
      columns = c(month_cols, "Moyenne mensuelle"),
      currency = "$", digits = 2, interval = 3, mark = " ", dec.mark = ","
    )
    
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
library(DBI)
library(RSQLite)
library(dplyr)
library(tibble)

db_init <- function(con){
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transactions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      date TEXT,
      description TEXT,
      amount REAL,
      direction TEXT,
      nature TEXT,
      source TEXT,
      bank_category_raw TEXT,
      category TEXT,
      subcategory TEXT,
      is_recurring INTEGER DEFAULT 0,
      notes TEXT,
      hash TEXT UNIQUE
    )
  ")
  
  # --- Migration transactions: cat_source / cat_confidence ---
  cols_tx <- DBI::dbGetQuery(con, "PRAGMA table_info(transactions)")$name
  if (!"cat_source" %in% cols_tx) {
    DBI::dbExecute(con, "ALTER TABLE transactions ADD COLUMN cat_source TEXT")
    DBI::dbExecute(con, "UPDATE transactions SET cat_source = 'manual' WHERE category IS NOT NULL AND cat_source IS NULL")
  }
  if (!"cat_confidence" %in% cols_tx) {
    DBI::dbExecute(con, "ALTER TABLE transactions ADD COLUMN cat_confidence REAL")
  }
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS categories (
      category TEXT,
      subcategory TEXT,
      direction TEXT
    )
  ")
  
  # migration categories: direction si absent
  cols_cat <- DBI::dbGetQuery(con, "PRAGMA table_info(categories)")$name
  if (!"direction" %in% cols_cat) {
    DBI::dbExecute(con, "ALTER TABLE categories ADD COLUMN direction TEXT")
    DBI::dbExecute(con, "UPDATE categories SET direction = 'Toutes' WHERE direction IS NULL")
  }
  DBI::dbExecute(con, "UPDATE categories SET direction = 'Toutes' WHERE direction IS NULL")
}

db_upsert_transactions <- function(con, df){
  if (nrow(df) == 0) return(invisible())
  
  stmt <- "
    INSERT OR IGNORE INTO transactions
    (date, description, amount, direction, nature, source, bank_category_raw,
     category, subcategory, is_recurring, notes, hash, cat_source, cat_confidence)
    VALUES
    (:date, :description, :amount, :direction, :nature, :source, :bank_category_raw,
     :category, :subcategory, :is_recurring, :notes, :hash, :cat_source, :cat_confidence)
  "
  
  cols <- c("date","description","amount","direction","nature","source","bank_category_raw",
            "category","subcategory","is_recurring","notes","hash","cat_source","cat_confidence")
  
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) stop("Colonnes manquantes dans df: ", paste(missing, collapse = ", "))
  
  for (i in seq_len(nrow(df))){
    row <- df[i, cols, drop = FALSE]
    params <- as.list(row) # named params OK avec :placeholders
    DBI::dbExecute(con, stmt, params = params)
  }
}

db_get_transactions <- function(con){
  DBI::dbReadTable(con, "transactions") |>
    mutate(date = as.Date(date)) |>
    arrange(desc(date), desc(id))
}

db_get_categories <- function(con){
  if(!DBI::dbExistsTable(con, "categories")){
    return(tibble(id = integer(), category = character(), subcategory = character(), direction = character()))
  }
  DBI::dbGetQuery(con, "
    SELECT rowid as id, category, subcategory, COALESCE(direction, 'Toutes') as direction
    FROM categories
    ORDER BY category, subcategory
  ")
}

db_add_category <- function(con, category, subcategory, direction = "Toutes"){
  DBI::dbWriteTable(
    con, "categories",
    data.frame(
      category = category,
      subcategory = subcategory,
      direction = direction,
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )
}

db_update_cell <- function(con, id, colname, value){
  allowed <- c("date","description","amount","category","subcategory","is_recurring","notes",
               "nature","direction","bank_category_raw","source","cat_source","cat_confidence")
  if(!colname %in% allowed) stop("Colonne non autorisée: ", colname)
  
  sql <- sprintf("UPDATE transactions SET %s = ? WHERE id = ?", colname)
  DBI::dbExecute(con, sql, params = unname(list(value, id)))
}

db_set_transactions_category <- function(con, ids, cat, subcat = NA_character_, is_rec = 0L){
  ids <- as.integer(ids)
  if (length(ids) == 0) return(invisible())
  
  if (length(cat) != 1) cat <- cat[1]
  cat <- as.character(cat)
  
  if (is.null(subcat) || length(subcat) == 0) subcat <- NA_character_
  if (length(subcat) != 1) subcat <- subcat[1]
  subcat <- as.character(subcat)
  
  sql <- sprintf(
    "UPDATE transactions
     SET category = ?, subcategory = ?, is_recurring = ?,
         cat_source = 'manual', cat_confidence = NULL
     WHERE id IN (%s)",
    paste(rep("?", length(ids)), collapse = ",")
  )
  
  params <- c(list(cat, subcat, as.integer(is_rec)), as.list(ids))
  DBI::dbExecute(con, sql, params = unname(params))
}

db_clear_transactions_category <- function(con, ids){
  ids <- as.integer(ids)
  if(length(ids) == 0) return(invisible())
  
  sql <- sprintf(
    "UPDATE transactions
     SET category = NULL, subcategory = NULL,
         cat_source = NULL, cat_confidence = NULL
     WHERE id IN (%s)",
    paste(rep("?", length(ids)), collapse = ",")
  )
  DBI::dbExecute(con, sql, params = unname(as.list(ids)))
}

db_delete_transactions <- function(con, ids){
  ids <- as.integer(ids)
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) return(invisible(0L))
  
  sql <- sprintf(
    "DELETE FROM transactions WHERE id IN (%s)",
    paste(rep("?", length(ids)), collapse = ",")
  )
  DBI::dbExecute(con, sql, params = unname(as.list(ids)))
  invisible(length(ids))
}

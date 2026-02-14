library(dplyr)
library(stringr)
library(lubridate)
library(digest)

# utils.R doit être chargé avant (parse_amount_fr, month_fr_to_num, etc.)

# ------------------------------
# Parseur COMPTE COURANT
# ------------------------------
parse_compte_paste <- function(txt, year = year(Sys.Date()), source = "Compte"){
  txt <- str_replace_all(txt, "\r", "\n")
  lines <- txt |> str_split("\n") |> unlist() |> str_squish()
  lines <- lines[lines != ""]
  
  # En-têtes / lignes à ignorer
  ignore_pat <- "^(Date\\s+Description\\s+Montant|Solde|lien)$"
  lines <- lines[!str_detect(lines, ignore_pat)]
  
  out <- list()
  cur_day <- NA_integer_
  cur_month <- NA_integer_
  buffer <- character()
  
  flush_tx <- function(desc, amount, bank_cat = NA_character_){
    direction <- ifelse(amount < 0, "Retrait", "Dépôt")
    date <- as.Date(sprintf("%04d-%02d-%02d", year, cur_month, cur_day))
    hash <- digest(paste(date, desc, amount, source, sep="|"), algo = "xxhash64")
    tibble(
      date = date,
      description = desc,
      amount = amount,
      direction = direction,
      nature = NA_character_,              # optionnel côté compte (peut rester NA)
      source = source,
      bank_category_raw = bank_cat,        # catégorie vue dans le site (si captée)
      category = NA_character_,
      subcategory = NA_character_,
      is_recurring = 0L,
      notes = NA_character_,
      hash = hash
    )
  }
  
  for (ln in lines){
    
    # 1) Si une ligne "résumé" existe, on l'utilise directement
    summ <- match_summary_line(ln)
    if (!is.null(summ)){
      cur_day <- summ$day
      cur_month <- month_fr_to_num(summ$month_name)
      if (is.na(cur_month)) next
      
      amount <- parse_amount_fr(summ$amount_token)
      out[[length(out)+1]] <- flush_tx(str_squish(summ$description), amount)
      buffer <- character()
      next
    }
    
    # 2) Ligne de date type "5 FÉV5 Février"
    md <- match_date_line(ln)
    if (!is.null(md)){
      cur_day <- md$day
      cur_month <- month_fr_to_num(md$month_name)
      buffer <- character()
      
      # si montant sur la même ligne que la date (rare côté compte)
      tok <- extract_amount_token(md$remainder)
      if (!is.na(tok)){
        amount <- parse_amount_fr(tok)
        desc <- str_squish(str_replace(md$remainder, "([\\+\\-\\u2212]?\\s*[0-9\\s]+[,\\.][0-9]{2})\\s*\\$", ""))
        desc <- ifelse(nchar(desc) > 0, desc, "Transaction")
        out[[length(out)+1]] <- flush_tx(desc, amount)
      }
      next
    }
    
    # Pas de date active => on ignore
    if (is.na(cur_day) || is.na(cur_month)) next
    
    # 3) Ligne avec montant => crée transaction à partir du buffer
    tok <- extract_amount_token(ln)
    if (!is.na(tok)){
      amount <- parse_amount_fr(tok)
      
      # Description : dernière ligne textuelle avant le montant
      bank_cat <- ifelse(length(buffer) >= 2, buffer[1], NA_character_)
      desc <- ifelse(length(buffer) >= 1, buffer[length(buffer)], "Transaction")
      
      out[[length(out)+1]] <- flush_tx(desc, amount, bank_cat)
      buffer <- character()
      next
    }
    
    # 4) Ligne texte : on l'ajoute au buffer
    buffer <- c(buffer, ln)
  }
  
  bind_rows(out)
}


# ------------------------------
# Parseur CARTE DE CRÉDIT
# ------------------------------
parse_credit_paste <- function(txt, year = year(Sys.Date()), source = "Crédit"){
  txt <- str_replace_all(txt, "\r", "\n")
  lines <- txt |> str_split("\n") |> unlist() |> str_squish()
  lines <- lines[lines != ""]
  
  # Ignore header
  lines <- lines[!str_detect(lines, "^Date\\s+Description")]
  
  out <- list()
  cur_day <- NA_integer_
  cur_month <- NA_integer_
  buffer <- character()
  
  flush_tx <- function(desc, amount, nature, bank_cat = NA_character_){
    direction <- ifelse(amount < 0, "Retrait", "Dépôt")
    date <- as.Date(sprintf("%04d-%02d-%02d", year, cur_month, cur_day))
    hash <- digest(paste(date, desc, amount, source, sep="|"), algo = "xxhash64")
    
    tibble(
      date = date,
      description = desc,
      amount = amount,
      direction = direction,
      nature = nature,                     # "Dépense" | "Crédit" | "Transfert"
      source = source,
      bank_category_raw = bank_cat,
      category = NA_character_,
      subcategory = NA_character_,
      is_recurring = 0L,
      notes = NA_character_,
      hash = hash
    )
  }
  
  for (ln in lines){
    
    # 1) Ligne résumé "29 Décembre Télus 85,08 $" si elle existe
    summ <- match_summary_line(ln)
    if (!is.null(summ)){
      cur_day <- summ$day
      cur_month <- month_fr_to_num(summ$month_name)
      if (is.na(cur_month)) next
      
      token <- summ$amount_token
      has_plus  <- str_detect(token, "^\\s*\\+")
      has_minus <- str_detect(token, "^\\s*[\\-\\u2212]")
      
      amt_abs <- abs(parse_amount_fr(token))
      # Convention carte :
      #  - + => inflow (paiement/crédit)
      #  - - => dépense
      #  - sans signe => achat => dépense (négatif)
      amount <- dplyr::case_when(
        has_plus  ~  amt_abs,
        has_minus ~ -amt_abs,
        TRUE      ~ -amt_abs
      )
      
      desc <- str_squish(summ$description)
      nature <- dplyr::case_when(
        str_detect(str_to_lower(desc), "\\bpaiement\\b") ~ "Transfert",
        amount > 0 ~ "Crédit",
        TRUE ~ "Dépense"
      )
      
      out[[length(out)+1]] <- flush_tx(desc, amount, nature)
      buffer <- character()
      next
    }
    
    # 2) Ligne de date "31 JAN31 Janvier ..." (parfois avec Paiement + montant dedans)
    md <- match_date_line(ln)
    if (!is.null(md)){
      cur_day <- md$day
      cur_month <- month_fr_to_num(md$month_name)
      buffer <- character()
      
      tok <- extract_amount_token(md$remainder)
      if (!is.na(tok)){
        has_plus  <- str_detect(tok, "^\\s*\\+")
        has_minus <- str_detect(tok, "^\\s*[\\-\\u2212]")
        
        amt_abs <- abs(parse_amount_fr(tok))
        amount <- dplyr::case_when(
          has_plus  ~  amt_abs,
          has_minus ~ -amt_abs,
          TRUE      ~ -amt_abs
        )
        
        # retire le montant et un % possible
        desc <- md$remainder |>
          str_replace("([\\+\\-\\u2212]?\\s*[0-9\\s]+[,\\.][0-9]{2})\\s*\\$", "") |>
          str_replace("\\b\\d+\\s*%\\b", "") |>
          str_squish()
        desc <- ifelse(nchar(desc) > 0, desc, "Transaction")
        
        nature <- dplyr::case_when(
          str_detect(str_to_lower(desc), "\\bpaiement\\b") ~ "Transfert",
          amount > 0 ~ "Crédit",
          TRUE ~ "Dépense"
        )
        
        out[[length(out)+1]] <- flush_tx(desc, amount, nature)
      }
      next
    }
    
    if (is.na(cur_day) || is.na(cur_month)) next
    
    # 3) Ligne montant (souvent "3 % 63,17 $" ou juste "+114,96 $")
    tok <- extract_amount_token(ln)
    if (!is.na(tok)){
      has_plus  <- str_detect(tok, "^\\s*\\+")
      has_minus <- str_detect(tok, "^\\s*[\\-\\u2212]")
      amt_abs <- abs(parse_amount_fr(tok))
      
      amount <- dplyr::case_when(
        has_plus  ~  amt_abs,
        has_minus ~ -amt_abs,
        TRUE      ~ -amt_abs
      )
      
      bank_cat <- ifelse(length(buffer) >= 2, buffer[1], NA_character_)
      desc <- ifelse(length(buffer) >= 1, buffer[length(buffer)], "Transaction")
      
      nature <- dplyr::case_when(
        str_detect(str_to_lower(desc), "\\bpaiement\\b") ~ "Transfert",
        amount > 0 ~ "Crédit",
        TRUE ~ "Dépense"
      )
      
      out[[length(out)+1]] <- flush_tx(desc, amount, nature, bank_cat)
      buffer <- character()
      next
    }
    
    # 4) Lignes texte : on accumule, mais ignore "1 %" "3 %"
    if (!str_detect(ln, "^\\d+\\s*%$")){
      buffer <- c(buffer, ln)
    }
  }
  
  bind_rows(out)
}
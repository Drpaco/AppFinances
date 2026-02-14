library(dplyr)
library(stringr)

merchant_key <- function(x){
  x %>%
    str_to_upper() %>%
    str_replace_all("[0-9]", " ") %>%
    str_replace_all("[^A-ZÀÂÄÇÉÈÊËÎÏÔÖÙÛÜŸ\\s]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

build_category_maps <- function(con){
  tx <- DBI::dbGetQuery(con, "
    SELECT description, direction, bank_category_raw, category, subcategory
    FROM transactions
    WHERE category IS NOT NULL AND category <> ''
  ")
  
  if (nrow(tx) == 0) {
    return(list(map_merchant = tibble(), map_bank = tibble()))
  }
  
  tx <- tx %>%
    mutate(
      description = as.character(description),
      direction = as.character(direction),
      bank_category_raw = as.character(bank_category_raw),
      category = as.character(category),
      subcategory = as.character(subcategory),
      merchant = merchant_key(description)
    )
  
  # A) Merchant mapping (merchant + direction)
  map_merchant <- tx %>%
    filter(!is.na(merchant), merchant != "") %>%
    count(direction, merchant, category, subcategory, name = "n") %>%
    group_by(direction, merchant) %>%
    mutate(total = sum(n), conf = n/total) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(direction, merchant, category, subcategory, conf, n, total)
  
  # B) bank_category_raw mapping (bank_category_raw + direction)
  map_bank <- tx %>%
    filter(!is.na(bank_category_raw), bank_category_raw != "") %>%
    count(direction, bank_category_raw, category, subcategory, name = "n") %>%
    group_by(direction, bank_category_raw) %>%
    mutate(total = sum(n), conf = n/total) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(direction, bank_category_raw, category, subcategory, conf, n, total)
  
  list(map_merchant = map_merchant, map_bank = map_bank)
}

apply_suggestions <- function(df_new, maps, min_conf = 0.60){
  if (nrow(df_new) == 0) return(df_new)
  
  df_new <- df_new %>%
    mutate(
      description = as.character(description),
      direction = as.character(direction),
      bank_category_raw = as.character(bank_category_raw),
      merchant = merchant_key(description)
    )
  
  # Join merchant suggestions first
  out <- df_new %>%
    left_join(maps$map_merchant %>% 
                select(direction, merchant, sug_cat = category, sug_sub = subcategory, sug_conf = conf),
              by = c("direction", "merchant"))
  
  # If no merchant suggestion, try bank_category_raw
  out <- out %>%
    left_join(maps$map_bank %>% 
                select(direction, bank_category_raw, sug_cat2 = category, sug_sub2 = subcategory, sug_conf2 = conf),
              by = c("direction", "bank_category_raw"))
  
  out <- out %>%
    mutate(
      cat_suggested = case_when(
        !is.na(sug_cat) & sug_conf >= min_conf ~ sug_cat,
        is.na(sug_cat) & !is.na(sug_cat2) & sug_conf2 >= min_conf ~ sug_cat2,
        TRUE ~ NA_character_
      ),
      sub_suggested = case_when(
        !is.na(sug_cat) & sug_conf >= min_conf ~ sug_sub,
        is.na(sug_cat) & !is.na(sug_cat2) & sug_conf2 >= min_conf ~ sug_sub2,
        TRUE ~ NA_character_
      ),
      conf_suggested = case_when(
        !is.na(sug_cat) & sug_conf >= min_conf ~ sug_conf,
        is.na(sug_cat) & !is.na(sug_cat2) & sug_conf2 >= min_conf ~ sug_conf2,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      # On ne remplit que si pas déjà catégorisé
      category = if_else(is.na(category) | category == "", cat_suggested, category),
      subcategory = if_else(is.na(subcategory) | subcategory == "", sub_suggested, subcategory),
      cat_source = if_else(!is.na(cat_suggested), "auto", coalesce(cat_source, NA_character_)),
      cat_confidence = if_else(!is.na(conf_suggested), conf_suggested, cat_confidence)
    ) %>%
    select(-merchant, -starts_with("sug_"), -starts_with("cat_"), -starts_with("sub_"), -conf_suggested)
  
  out
}
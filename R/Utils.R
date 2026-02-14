library(stringr)
library(lubridate)
library(digest)

# Montant FR -> numérique (gère espaces, $, virgules, signe unicode)
parse_amount_fr <- function(x){
  x <- str_replace_all(x, "\u2212", "-")     # '−' unicode -> '-'
  x <- str_replace_all(x, "\u00A0", " ")     # espace insécable -> espace
  x <- str_squish(x)
  x <- str_replace_all(x, "\\$", "")
  x <- str_replace_all(x, " ", "")           # enlève séparateur de milliers
  x <- str_replace_all(x, ",", ".")          # virgule -> point
  as.numeric(x)
}

# Mois FR -> numéro
month_fr_to_num <- function(m){
  m <- str_to_lower(m)
  m <- str_replace_all(m, c("é"="e","è"="e","ê"="e","ë"="e","à"="a","â"="a","î"="i","ï"="i","ô"="o","û"="u","ç"="c"))
  map <- c(
    "janvier"=1, "janv"=1,
    "fevrier"=2, "fevr"=2, "fev"=2,
    "mars"=3,
    "avril"=4, "avr"=4,
    "mai"=5,
    "juin"=6,
    "juillet"=7, "juil"=7,
    "aout"=8,
    "septembre"=9, "sept"=9,
    "octobre"=10, "oct"=10,
    "novembre"=11, "nov"=11,
    "decembre"=12, "dec"=12
  )
  unname(map[m])
}

# Extrait le premier montant "… 123,45 $" d'une ligne (avec signe optionnel)
extract_amount_token <- function(line){
  m <- str_match(line, "([\\+\\-\\u2212]?\\s*[0-9\\s]+[,\\.][0-9]{2})\\s*\\$")
  m[,2]
}

# Détecte une ligne de date type "31 JAN31 Janvier ..." ou "5 FÉV5 Février ..."
# Retourne: day, month_name, remainder (après le mois)
match_date_line <- function(line){
  # ex: "6 JAN6 Janvier   Paiement   +2 986,74 $"
  pat <- "^\\s*(\\d{1,2})\\s*[A-ZÉ]{3}\\s*\\d{1,2}\\s+([A-Za-zÉéÈèÊêËëÛûÂâÀàÎîÏïÔôÇç]+)\\s*(.*)\\s*$"
  m <- str_match(line, pat)
  if (is.na(m[1,1])) return(NULL)
  list(day = as.integer(m[1,2]), month_name = m[1,3], remainder = m[1,4])
}

# Détecte une ligne "résumé" type "5 Février .... −156,02 $"
match_summary_line <- function(line){
  pat <- "^\\s*(\\d{1,2})\\s+([A-Za-zÉéÈèÊêËëÛûÂâÀàÎîÏïÔôÇç]+)\\s+(.+?)\\s+([\\+\\-\\u2212]?\\s*[0-9\\s]+[,\\.][0-9]{2})\\s*\\$\\s*$"
  m <- str_match(line, pat)
  if (is.na(m[1,1])) return(NULL)
  list(day = as.integer(m[1,2]), month_name = m[1,3], description = m[1,4], amount_token = m[1,5])
}
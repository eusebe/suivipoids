library(googlesheets4)

# Authentifie l'accès en lecture seule au Sheet via le compte de service.
# En local : lit le fichier secrets/service-account.json.
# En production (Connect Cloud) : lit le JSON depuis la variable d'environnement
# GOOGLE_SERVICE_ACCOUNT_JSON (gargle accepte le JSON en texte brut comme `path`).
gs_auth <- function() {
  env_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
  gs4_auth(path = if (nzchar(env_json)) env_json else SERVICE_ACCOUNT_PATH)
}

# Lit les réponses brutes du Form (colonnes Timestamp / Poids (kg))
fetch_raw <- function() {
  read_sheet(SHEET_ID)
}

# Nettoie les réponses brutes en un data.frame date/poids trié,
# avec une seule ligne par jour (moyenne si plusieurs pesées le même jour).
# Utilise la position des colonnes (1 = horodatage, 2 = poids) plutôt que
# leur nom : le libellé exact de la question dans le Sheet peut varier.
clean_weight_data <- function(raw) {
  d <- data.frame(
    date = as.Date(raw[[1]], tz = "Europe/Paris"),
    poids = suppressWarnings(as.numeric(gsub(",", ".", as.character(raw[[2]]))))
  )
  d <- d[!is.na(d$date) & !is.na(d$poids), ]
  d <- aggregate(poids ~ date, data = d, FUN = mean)
  d[order(d$date), ]
}

# Point d'entrée utilisé par le dashboard
get_weight_data <- function() {
  clean_weight_data(fetch_raw())
}

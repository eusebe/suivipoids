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

# Classe une heure en moment de la journée : matin (< 10h), milieu de
# journée, ou soir (>= 20h)
moment_jour <- function(heure) {
  h <- as.numeric(format(heure, "%H")) + as.numeric(format(heure, "%M")) / 60
  factor(
    ifelse(h < 10, "Matin", ifelse(h >= 20, "Soir", "Milieu de journée")),
    levels = c("Matin", "Milieu de journée", "Soir")
  )
}

# Nettoie les réponses brutes : une ligne par pesée (date, heure, moment,
# poids), triée par horodatage. Utilise la position des colonnes (1 =
# horodatage, 2 = poids) plutôt que leur nom : le libellé exact de la
# question dans le Sheet peut varier.
clean_weight_data <- function(raw) {
  # googlesheets4 lit l'horodatage du Sheet en l'étiquetant "UTC", mais la
  # valeur est en réalité déjà l'heure murale Europe/Paris telle que saisie
  # par le Form (les serials Google Sheets ne portent pas de fuseau). Il
  # faut donc réinterpréter ces chiffres tels quels en Europe/Paris, pas les
  # convertir (as.POSIXct(x, tz=) sur un POSIXct décale l'instant plutôt que
  # de relabelliser, ce qui ajoutait à tort le décalage CEST/CET).
  d <- data.frame(
    heure = as.POSIXct(format(raw[[1]], tz = "UTC"), tz = "Europe/Paris"),
    poids = suppressWarnings(as.numeric(gsub(",", ".", as.character(raw[[2]]))))
  )
  d <- d[!is.na(d$heure) & !is.na(d$poids), ]
  d$date <- as.Date(d$heure, tz = "Europe/Paris")
  d$moment <- moment_jour(d$heure)
  d[order(d$heure), c("date", "heure", "moment", "poids")]
}

# Point d'entrée utilisé par le dashboard : une ligne par pesée
get_weight_data <- function() {
  clean_weight_data(fetch_raw())
}

# Agrège en une ligne par jour (moyenne) : c'est cette version qui alimente
# la tendance, la projection et le résumé, quel que soit le nombre de
# pesées par jour.
agg_daily <- function(df) {
  d <- aggregate(poids ~ date, data = df, FUN = mean)
  d[order(d$date), ]
}

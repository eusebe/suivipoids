# Seuils OMS (kg/m²) et libellés des catégories d'IMC
IMC_SEUILS <- c(maigreur = 18.5, normal = 25, surpoids = 30)
IMC_LABELS <- c("Maigreur", "Corpulence normale", "Surpoids", "Obésité")
IMC_COULEURS <- c("#5b8def", "#27ae60", "#f39c12", "#e74c3c")

imc <- function(poids, taille_m = TAILLE_M) {
  poids / taille_m^2
}

# Catégorie OMS d'un IMC donné
imc_categorie <- function(valeur_imc) {
  cut(
    valeur_imc,
    breaks = c(-Inf, IMC_SEUILS, Inf),
    labels = IMC_LABELS,
    right = FALSE
  )
}

# Traduit les seuils OMS (IMC) en seuils de poids (kg) pour une taille donnée
seuils_poids <- function(taille_m = TAILLE_M) {
  IMC_SEUILS * taille_m^2
}

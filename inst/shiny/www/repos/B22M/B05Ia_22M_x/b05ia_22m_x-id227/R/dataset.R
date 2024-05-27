# Importation des données
# Date : 2022-11-29

# Importation des packages
SciViews::R()

# Données importées depuis ZENODO
url <- "https://zenodo.org/record/4943286/files/Abies_bals_allom_data_Peart.csv?download=1"

# Importation des données
abies <- read$csv(url, cache_file = "data/raw/ab.csv")

# Renommer les variables
abies <- janitor::clean_names(abies)

# Ajout des labels et des unités
abies <- labelise(abies,
  label = list(
    height = "Hauteur",
    diameter = 'Diamètre à 1.37m du sol',
    elevation = "Altitude",
    canopyheight = "Hauteur du plus grand arbre proche"
  ),
  units = list(
    height = "m",
    diameter = "cm",
    elevation = "m",
    canopyheight = "m"
  ))

# Sauvegarde des données localement
write$rds(abies, "data/abies.rds", compress = "xz")

rm(abies, url)

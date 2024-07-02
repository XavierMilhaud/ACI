# Charger les bibliothèques nécessaires
library(httr)
library(zip)
library(data.table)
library(dplyr)

# URL du fichier à télécharger
url <- "https://psmsl.org/data/obtaining/rlr.monthly.data/rlr_monthly.zip"

# Chemin vers le répertoire de destination
destination_dir <- "../data/required_data"
zip_file_path <- file.path(destination_dir, "rlr_monthly.zip")
extract_path <- file.path(destination_dir, "rlr_monthly")

# Vérifie si le dossier rlr_monthly existe déjà
if (!dir.exists(extract_path)) {
  # Crée le répertoire de destination s'il n'existe pas
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Télécharge le fichier
  response <- GET(url, write_disk(zip_file_path, overwrite = TRUE))
  if (status_code(response) == 200) {
    # Décompresse le fichier
    unzip(zip_file_path, exdir = destination_dir)
    
    # Supprime le fichier zip après extraction
    file.remove(zip_file_path)
    cat("Téléchargement et décompression terminés.\n")
  } else {
    cat("Échec du téléchargement.\n")
    stop("Erreur de téléchargement")
  }
} else {
  cat("Le dossier rlr_monthly existe déjà. Aucune action nécessaire.\n")
}

# Charger le DataFrame A
# En supposant que A soit un fichier CSV, remplacez 'path_to_dataframe.csv' par le chemin réel de votre CSV
csv_path <- '../data/required_data/psmsl_data.csv'
if (file.exists(csv_path)) {
  df <- fread(csv_path)
  cat("DataFrame chargé avec succès.\n")
} else {
  cat("Fichier CSV non trouvé. Veuillez vérifier le chemin du fichier.\n")
  stop("Erreur de chargement du CSV")
}

# Répertoire contenant les fichiers .rlrdata
source_dir <- '../data/required_data/rlr_monthly/data'

# Fonction pour copier et renommer les fichiers en fonction de l'abréviation du pays
copy_and_rename_files_by_country <- function(abbreviation) {
  # Répertoire cible pour copier les fichiers
  target_dir <- file.path("../data", paste0("sealevel_data_", abbreviation))
  
  # S'assurer que le répertoire cible existe
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
    cat(paste("Créé le répertoire cible", target_dir, "\n"))
  }
  
  # Filtrer le DataFrame par l'abréviation du pays
  filtered_df <- df %>% filter(Country == abbreviation)
  if (nrow(filtered_df) == 0) {
    cat(paste("Aucune entrée trouvée pour l'abréviation du pays", abbreviation, "\n"))
    return(NULL)
  }
  
  # Parcourir chaque ID et copier le fichier associé, puis le renommer
  for (file_id in filtered_df$ID) {
    source_file <- file.path(source_dir, paste0(file_id, ".rlrdata"))
    if (file.exists(source_file)) {
      destination_file <- file.path(target_dir, paste0(file_id, ".rlrdata"))
      file.copy(source_file, destination_file)
      cat(paste("Copié", source_file, "vers", destination_file, "\n"))
      
      # Renommer le fichier copié en .txt
      new_filename <- file.path(target_dir, paste0(file_id, ".txt"))
      file.rename(destination_file, new_filename)
      cat(paste("Renommé", destination_file, "en", new_filename, "\n"))
    } else {
      cat(paste("Le fichier", source_file, "n'existe pas\n"))
    }
  }
}


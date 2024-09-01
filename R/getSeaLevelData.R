# Charger les bibliothèques nécessaires
library(httr)
library(zip)
library(data.table)
library(dplyr)

#' Télécharge et décompresse les données RLR mensuelles
#'
#' Télécharge un fichier ZIP contenant les données RLR mensuelles depuis une URL spécifiée,
#' le décompresse dans un répertoire de destination, et supprime le fichier ZIP après l'extraction.
#' Si le dossier cible existe déjà, aucune action n'est effectuée.
#'
#' @param url URL du fichier à télécharger
#' @param destination_dir Chemin vers le répertoire de destination
#' @param zip_file_path Chemin complet vers le fichier ZIP
#' @param extract_path Chemin complet vers le répertoire d'extraction
#'
#' @return Affiche un message indiquant si le téléchargement et la décompression sont terminés,
#' ou si le dossier existe déjà
#' @examples
#' # Exemple de chemin d'accès et d'utilisation
#' url <- "https://psmsl.org/data/obtaining/rlr.monthly.data/rlr_monthly.zip"
#' destination_dir <- "../data/required_data"
#' zip_file_path <- file.path(destination_dir, "rlr_monthly.zip")
#' extract_path <- file.path(destination_dir, "rlr_monthly")
#' # Exécute le téléchargement et l'extraction
#' # (Voir le code complet ci-dessous pour l'exécution)
#'
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

#' Charger le fichier CSV contenant les données PSMSL
#'
#' Vérifie si un fichier CSV spécifique existe dans le répertoire de données.
#' Si le fichier existe, il est chargé dans un DataFrame. Sinon, une erreur est levée.
#'
#' @param csv_path Chemin vers le fichier CSV à charger
#'
#' @return Affiche un message indiquant si le DataFrame a été chargé avec succès
#' ou si une erreur s'est produite
#' @examples
#' # Chemin du fichier CSV
#' csv_path <- '../data/required_data/psmsl_data.csv'
#' # Charger le fichier CSV
#' # (Voir le code complet ci-dessous pour l'exécution)
#'
csv_path <- '../data/required_data/psmsl_data.csv'
if (file.exists(csv_path)) {
  df <- fread(csv_path)
  cat("DataFrame chargé avec succès.\n")
} else {
  cat("Fichier CSV non trouvé. Veuillez vérifier le chemin du fichier.\n")
  stop("Erreur de chargement du CSV")
}

#' Copier et renommer les fichiers en fonction de l'abréviation du pays
#'
#' Copie les fichiers .rlrdata d'un répertoire source vers un répertoire cible basé
#' sur l'abréviation du pays fournie. Les fichiers copiés sont ensuite renommés en .txt.
#'
#' @param abbreviation Abréviation du pays pour filtrer et copier les fichiers associés
#' @param source_dir Répertoire source contenant les fichiers .rlrdata
#' @param target_dir Répertoire cible où les fichiers seront copiés et renommés
#' @param df DataFrame contenant les données PSMSL pour filtrer par abréviation de pays
#'
#' @return NULL Renvoie NULL si aucune entrée n'est trouvée pour l'abréviation du pays
#' @examples
#' # Exécution de la fonction pour une abréviation spécifique
#' copy_and_rename_files_by_country("FRA")
#' # (Voir le code complet ci-dessous pour l'exécution)
#'
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


#' Telecharge et decompresse les donnees RLR mensuelles
#'
#' Telecharge un fichier ZIP contenant les donnees RLR mensuelles depuis une URL specifiee,
#' le decompresse dans un repertoire de destination, et supprime le fichier ZIP apres l'extraction.
#' Si le dossier cible existe deja, aucune action n'est effectuee.
#'
#' @param url URL du fichier a telecharger.
#' @param destination_dir Chemin vers le repertoire de destination.
#'
#' @return Affiche un message indiquant si le telechargement et la decompression sont termines,
#'         ou si le dossier existe deja.
#'
#' @examples
#' # URL du fichier a telecharger:
#' #url <- "https://psmsl.org/data/obtaining/rlr.monthly.data/rlr_monthly.zip"
#' #destination_dir <- "./inst/externaldata/"
#' #download_monthlyRLRdata(url, destination_dir)
#'
#' @export

download_monthlyRLRdata <- function(url, destination_dir) {
  zip_file_path <- file.path(destination_dir, "rlr_monthly.zip")
  extract_path <- file.path(destination_dir, "rlr_monthly")
  # Verifie si le dossier rlr_monthly existe deja
  if (!dir.exists(extract_path)) {
    # Cree le repertoire de destination s'il n'existe pas
    if (!dir.exists(destination_dir)) {
      dir.create(destination_dir, showWarnings = FALSE, recursive = TRUE)
    }
    # Telecharge le fichier
    response <- httr::GET(url, httr::write_disk(zip_file_path, overwrite = TRUE))
    if (httr::status_code(response) == 200) {
      # Decompresse le fichier
      zip::unzip(zip_file_path, exdir = destination_dir)
      # Supprime le fichier zip apres extraction
      file.remove(zip_file_path)
      cat("Downloading and unzipping finished.\n")
    } else {
      cat("Failed to download.\n")
      stop("Download error")
    }
  } else {
    cat("Repository rlr_monthly already exists. Nothing to do.\n")
  }
}


#' Charger le fichier CSV contenant les donnees PSMSL
#'
#' Verifie si un fichier CSV specifique existe dans le repertoire de donnees.
#' Si le fichier existe, il est charge dans un DataFrame. Sinon, une erreur est levee.
#'
#' @param csv_path Chemin vers le fichier CSV a charger.
#'
#' @return Affiche un message indiquant si le DataFrame a ete charge avec succes
#'         ou si une erreur s'est produite.
#'
#' @examples
#' # Chemin du fichier CSV:
#' #csv_path <- './inst/externaldata/psmsl_data.csv'
#' #psmsl <- load_PSMSLdata(csv_path)
#'
#'@export

load_PSMSLdata <- function(csv_path) {
  if (file.exists(csv_path)) {
    df <- data.table::fread(csv_path)
    cat("DataFrame loaded successfully.\n")
  } else {
    cat("CSV file not found. Please check the file path.\n")
    stop("Error in loading the CSV file.")
  }
  return(df)
}

#' Copier et renommer les fichiers en fonction de l'abreviation du pays
#'
#' Copie les fichiers .rlrdata d'un repertoire source vers un repertoire cible base
#' sur l'abreviation du pays fournie. Les fichiers copies sont ensuite renommes en .txt.
#'
#' @param abbreviation Abreviation du pays pour filtrer et copier les fichiers associes
#' @param source_dir Repertoire source contenant les fichiers .rlrdata
#' @param df DataFrame contenant les donnees PSMSL pour filtrer par abreviation de pays
#'
#' @return NULL Renvoie NULL si aucune entree n'est trouvee pour l'abreviation du pays
#'
#' @examples
#' # Execution de la fonction pour une abreviation specifique
#' # Repertoire contenant les fichiers .rlrdata
#' #psmsl <- load_PSMSLdata('./inst/externaldata/psmsl_data.csv')
#' data(psmsl)
#' source_dir <- './inst/externaldata/rlr_monthly/data/'
#' copy_and_rename_files_by_country("FRA", source_dir, psmsl)
#'
#' @import dplyr
#' @export
#'
copy_and_rename_files_by_country <- function(abbreviation, source_dir, df) {
  # Repertoire cible pour copier les fichiers
  target_dir <- file.path("./inst/generateddata", paste0("sealevel_data_", abbreviation))
  # S'assurer que le repertoire cible existe
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
    cat(paste("Create the target reposirory", target_dir, "\n"))
  }

  # Filtrer le DataFrame par l'abreviation du pays
  filtered_df <- df |> dplyr::filter(Country == abbreviation)
  if (nrow(filtered_df) == 0) {
    cat(paste("No data for country ", abbreviation, "\n"))
    return(NULL)
  }

  # Parcourir chaque ID et copier le fichier associe, puis le renommer
  for (file_id in filtered_df$ID) {
    source_file <- file.path(source_dir, paste0(file_id, ".rlrdata"))
    if (file.exists(source_file)) {
      destination_file <- file.path(target_dir, paste0(file_id, ".rlrdata"))
      file.copy(source_file, destination_file)
      cat(paste("Copy ", source_file, " to ", destination_file, "\n"))

      # Renommer le fichier copie en .txt
      new_filename <- file.path(target_dir, paste0(file_id, ".txt"))
      file.rename(destination_file, new_filename)
      cat(paste("Rename ", destination_file, " as ", new_filename, "\n"))
    } else {
      cat(paste("File ", source_file, "does not exist\n"))
    }
  }
}

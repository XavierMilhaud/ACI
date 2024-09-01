# Charger les bibliothèques nécessaires
library(ecmwfr)
library(data.table)

#' Classe Era5var pour extraire des données ERA5
#'
#' Cette classe contient des méthodes pour extraire des données ERA5.
#'
#' @field area Nom de la zone pour laquelle les données sont nécessaires (par exemple, 'France', 'London').
#' @field coordinates Liste des coordonnées nord, ouest, sud et est qui délimitent la zone.
#' @field years Intervalle d'années pour l'extraction sous forme de chaîne (par exemple, '1961-1963') ou une seule année (par exemple, '1961').
#' @field variable_name Nom de la variable à extraire (par exemple, 'total_precipitation').
#' @field monthly Booléen, si TRUE, l'extraction se fait mois par mois puis fusionnée (utile pour les grandes données).
#'
#' @export
Era5var <- setRefClass(
  "Era5var",
  fields = list(
    area_name = "character",
    coordinates = "numeric",
    years_included = "character",
    variable_name = "character",
    monthly = "logical"
  ),
  methods = list(
    #' Initialiser un objet Era5var avec les paramètres spécifiés.
    #'
    #' @param area Nom de la zone.
    #' @param coordinates Coordonnées délimitant la zone.
    #' @param years Intervalle d'années pour l'extraction.
    #' @param variable_name Nom de la variable à extraire.
    #' @param monthly (booléen, optionnel) Si TRUE, extrait les données mois par mois. Par défaut, FALSE.
    #'
    #' @return Un nouvel objet Era5var.
    initialize = function(area, coordinates, years, variable_name = NULL, monthly = FALSE) {
      if (grepl("-", years)) { # Format 'YYYY-YYYY'
        start_year <- as.integer(substr(years, 1, 4))
        end_year <- as.integer(substr(years, 6, 9))
        .self$years_included <- as.character(start_year:end_year)
      } else { # Format 'YYYY'
        .self$years_included <- years
      }
      
      .self$area_name <- area
      .self$coordinates <- coordinates
      .self$variable_name <- variable_name
      .self$monthly <- monthly
    },
    
    #' Demander les données ERA5 en fonction des paramètres d'initialisation.
    request_data = function() {
      directory <- "../data/data0"
      if (!dir.exists(directory)) {
        dir.create(directory, recursive = TRUE)
      }
      
      wf_set_key(user = "your_user", key = "your_api_key", service = "cds")
      
      for (year in .self$years_included) {
        if (!.self$monthly) {
          wf_request(
            user = "your_user",
            request = list(
              dataset_short_name = "reanalysis-era5-single-levels",
              product_type = "reanalysis",
              variable = .self$variable_name,
              year = year,
              month = sprintf("%02d", 1:12),
              day = sprintf("%02d", 1:31),
              time = sprintf("%02d:00", 0:23),
              area = .self$coordinates,
              format = "netcdf"
            ),
            target = sprintf("%s/%s_%s_%s.nc", directory, .self$area_name, .self$variable_name, year)
          )
        } else {
          for (month in 1:12) {
            wf_request(
              user = "your_user",
              request = list(
                dataset_short_name = "reanalysis-era5-single-levels",
                product_type = "reanalysis",
                variable = .self$variable_name,
                year = year,
                month = sprintf("%02d", month),
                day = sprintf("%02d", 1:31),
                time = sprintf("%02d:00", 0:23),
                area = .self$coordinates,
                format = "netcdf"
              ),
              target = sprintf("%s/%s_%s_%s_%02d.nc", directory, .self$area_name, .self$variable_name, year, month)
            )
          }
        }
      }
      if (!.self$monthly) {
        .self$merge_files(directory)
      }
    },
    
    #' Demander les données ERA5 pour toutes les variables pertinentes.
    request_all_variables = function() {
      variables <- c('total_precipitation', '2m_temperature', '10m_u_component_of_wind', '10m_v_component_of_wind')
      for (variable in variables) {
        .self$variable_name <- variable
        cat(sprintf("Requesting data for variable: %s\n", variable))
        .self$request_data()
      }
    },
    
    #' Fusionner les fichiers NetCDF mensuels en un seul fichier et supprimer les fichiers mensuels individuels.
    #'
    #' @param directory Répertoire contenant les fichiers NetCDF.
    merge_files = function(directory) {
      setwd(directory)
      if (length(.self$years_included) == 1) {
        merged_filename <- sprintf("%s_%s_%s_complete.nc", .self$area_name, .self$variable_name, .self$years_included[1])
      } else {
        merged_filename <- sprintf("%s_%s_%s_%s.nc", .self$area_name, .self$variable_name, .self$years_included[1], .self$years_included[length(.self$years_included)])
      }
      merge_command <- sprintf("cdo -b F32 mergetime *.nc %s", merged_filename)
      system(merge_command)
      cat("Merge executed successfully\n")
      
      # Delete individual monthly files but keep the merged file
      file.remove(list.files(pattern = sprintf("^%s.*\\.nc$", .self$area_name)))
    }
  )
)

# Exemple d'utilisation
if (interactive()) {
  test <- Era5var$new(
    area = 'PartOfParis',
    coordinates = c(49, 1, 48, 3),
    years = '1983-2023'
  )
  test$request_all_variables()
}

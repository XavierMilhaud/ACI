
#' Classe Era5var pour extraire des donnees ERA5
#'
#' Cette classe contient des methodes pour extraire des donnees ERA5.
#'
#' @field area_name Nom de la zone pour laquelle les donnees sont necessaires (par exemple, 'France', 'London').
#' @field coordinates Liste des coordonnees nord, ouest, sud et est qui delimitent la zone.
#' @field years_included Intervalle d'annees pour l'extraction sous forme de chaîne (par exemple, '1961-1963') ou une seule annee (par exemple, '1961').
#' @field variable_name Nom de la variable a extraire (par exemple, 'total_precipitation').
#' @field monthly Booleen, si TRUE, l'extraction se fait mois par mois puis fusionnee (utile pour les grandes donnees).
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
    initialize = function(area, coordinates, years, variable_name = "", monthly = FALSE) {
      #' Initialiser un objet Era5var avec les parametres specifies.
      #'
      #' @param area Nom de la zone.
      #' @param coordinates Coordonnees delimitant la zone.
      #' @param years Intervalle d'annees pour l'extraction.
      #' @param variable_name Nom de la variable a extraire.
      #' @param monthly (booleen, optionnel) Si TRUE, extrait les donnees mois par mois. Par defaut, FALSE.
      #'
      #' @return Un nouvel objet Era5var.
      #'
      #' @examples
      #' #test <- Era5var$new(
      #' #           area_name = 'PartOfParis',
      #' #           coordinates = c(49, 1, 48, 3),
      #' #           years_included = '1983-2023'
      #' #           )
      #' #test$request_all_variables()
      #'
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

    request_data = function() {
      #' Demander les donnees ERA5 en fonction des parametres d'initialisation.
      #'
      directory <- "./data/ERA5"
      if (!dir.exists(directory)) {
        dir.create(directory, recursive = TRUE)
      }
      #ecmwfr::wf_set_key(user = "your_user", key = "your_api_key", service = "cds")
      ecmwfr::wf_set_key(user = "xavier.milhaud.research@gmail.com", key = "1e3d693e-36bf-4836-b831-a32e36670a6f")

      for (year in .self$years_included) {
        if (!.self$monthly) {
          ecmwfr::wf_request(
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
              format = "netcdf",
              target = sprintf("%s/%s_%s_%s.nc", directory, .self$area_name, .self$variable_name, year)
            )
          )
        } else {
          for (month in 1:12) {
            ecmwfr::wf_request(
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
                format = "netcdf",
                target = sprintf("%s/%s_%s_%s_%02d.nc", directory, .self$area_name, .self$variable_name, year, month)
              )
            )
          }
        }
      }
      if (!.self$monthly) {
        .self$merge_files(directory)
      }
    },

    request_all_variables = function() {
      #' Demander donnees ERA5 pour toutes les variables pertinentes.
      #'
      variables <- c('total_precipitation', '2m_temperature', '10m_u_component_of_wind', '10m_v_component_of_wind')
      for (variable in variables) {
        .self$variable_name <- variable
        cat(sprintf("Requesting data for variable: %s\n", variable))
        .self$request_data()
      }
    },

    merge_files = function(directory) {
      #' Fusionner les fichiers NetCDF mensuels en un seul fichier et supprimer les
      #' fichiers mensuels individuels.
      #'
      #' @param directory Repertoire contenant les fichiers NetCDF.
      #'
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

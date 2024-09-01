library(data.table)
library(lubridate)
library(zoo)

setwd("./R")
# Assurez-vous que les fichiers des composants sont chargés
source("temperaturecomponent.R")
source("precipitationcomponent.R")
source("droughtcomponent.R")
source("windcomponent.R")
source("sealevel.R")

#' Classe ActuarialClimateIndex
#'
#' Cette classe représente l'indice climatique actuariel, qui est calculé à partir de plusieurs composants climatiques tels que la température, les précipitations, la sécheresse, le vent, et le niveau de la mer.
#'
#' @field temperature_component Composant température de type ANY
#' @field precipitation_component Composant précipitation de type ANY
#' @field drought_component Composant sécheresse de type ANY
#' @field wind_component Composant vent de type ANY
#' @field sealevel_component Composant niveau de la mer de type ANY
#' @field study_period Période d'étude pour l'analyse (vecteur de deux dates)
#' @field reference_period Période de référence pour la standardisation (vecteur de deux dates)

ActuarialClimateIndex <- setRefClass(
  "ActuarialClimateIndex",
  fields = list(
    temperature_component = "ANY",  # Composant température
    precipitation_component = "ANY",  # Composant précipitation
    drought_component = "ANY",  # Composant sécheresse
    wind_component = "ANY",  # Composant vent
    sealevel_component = "ANY",  # Composant niveau de la mer
    study_period = "character",  # Période d'étude
    reference_period = "character"  # Période de référence
  ),
  methods = list(
    #' Initialize ActuarialClimateIndex object
    #'
    #' Cette méthode initialise un objet de la classe ActuarialClimateIndex avec les chemins des fichiers de données nécessaires et les périodes d'étude et de référence.
    #'
    #' @param temperature_data_path Chemin vers le fichier de données de température
    #' @param precipitation_data_path Chemin vers le fichier de données de précipitation
    #' @param wind_u10_data_path Chemin vers le fichier de données du composant U du vent
    #' @param wind_v10_data_path Chemin vers le fichier de données du composant V du vent
    #' @param country_abbrev Abréviation du pays pour le niveau de la mer
    #' @param mask_data_path Chemin vers le fichier de masque géographique
    #' @param study_period Période d'étude pour l'analyse (vecteur de deux dates)
    #' @param reference_period Période de référence pour la standardisation (vecteur de deux dates)
    #'
    #' @return Un nouvel objet ActuarialClimateIndex
    
    initialize = function(temperature_data_path, precipitation_data_path,
                          wind_u10_data_path, wind_v10_data_path,
                          country_abbrev, mask_data_path,
                          study_period, reference_period) {
      
      .self$study_period <- study_period
      .self$reference_period <- reference_period
      
      .self$temperature_component <- TemperatureComponent$new(
        temperature_file = temperature_data_path, 
        mask_file = mask_data_path, 
        reference_period = reference_period
      )
      
      .self$precipitation_component <- PrecipitationComponent$new(
        precipitation_file = precipitation_data_path, 
        mask_file = mask_data_path, 
        reference_period = reference_period
      )
      
      .self$drought_component <- DroughtComponent$new(
        precipitation_file = precipitation_data_path, 
        mask_file = mask_data_path, 
        reference_period = reference_period
      )
      
      .self$wind_component <- WindComponent$new(
        u10_file = wind_u10_data_path, 
        v10_file = wind_v10_data_path, 
        mask_file = mask_data_path
      )
      
      .self$sealevel_component <- SeaLevelComponent$new(
        country_abrev = country_abbrev, 
        study_period = study_period, 
        reference_period = reference_period
      )
    },
    
    #' Calculate Actuarial Climate Index
    #'
    #' Cette méthode calcule l'indice climatique actuariel (ACI) en utilisant les données des différents composants climatiques initialisés.
    #'
    #' @param factor Facteur d'ajustement pour le niveau de la mer
    #'
    #' @return Un data.table contenant les valeurs calculées de l'indice climatique actuariel (ACI)
    
    calculate_aci = function(factor = 1) {
      
      # Exécution des calculs nécessaires avec les composants initialisés
      preci_std <- .self$precipitation_component$calculate_monthly_max()
      p_dt <- preci_std[, .(year, month, latitude, longitude, precipitation = rx5day)]
      
      wind_std <- .self$wind_component$std_wind_exceedance_frequency(.self$reference_period)
      w_dt <- wind_std[, .(year_month, latitude, longitude, windpower = std_frequency)]
      w_dt[, `:=`(year = year(year_month), month = month(year_month))]
      
      .self$drought_component$max_consecutive_dry_days()
      .self$drought_component$standardize_metric()
      cdd_dt <- .self$drought_component$standardized_dry_days_dt[, .(year, month, latitude, longitude, drought = standardized_dry_days)]
      
      temp90_std <- .self$temperature_component$std_t90(.self$reference_period)
      t90_dt <- temp90_std[, .(month, T90 = standardized)]
      
      temp10_std <- .self$temperature_component$std_t10(.self$reference_period)
      t10_dt <- temp10_std[, .(month, T10 = standardized)]
      
      sea_lev <- .self$sealevel_component$process()
      
      # Sélectionner les colonnes Measurement_* et calculer leur moyenne
      measurement_cols <- grep("^Measurement_", colnames(sea_lev), value = TRUE)
      sea_lev[, sea_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = measurement_cols]
      sea_lev <- sea_lev[, .(Corrected_Date, sea_mean)]
      
      sea_lev[, `:=`(year = as.integer(year(Corrected_Date)), month = as.integer(month(Corrected_Date)))]
      w_dt[, `:=`(year = as.integer(year), month = as.integer(month))]
      p_dt[, `:=`(year = as.integer(year), month = as.integer(month))]
      cdd_dt[, `:=`(year = as.integer(year), month = as.integer(month))]
      t90_dt[, `:=`(year = as.integer(year(month)), month = as.integer(month(month)))]
      t10_dt[, `:=`(year = as.integer(year(month)), month = as.integer(month(month)))]
      
      # Merge data.tables by year, month, latitude, and longitude
      df1 <- merge(w_dt, p_dt, by = c("year", "month", "latitude", "longitude"), all = TRUE)
      df2 <- merge(df1, cdd_dt, by = c("year", "month", "latitude", "longitude"), all = TRUE)
      df3 <- merge(df2, sea_lev, by = c("year", "month"), all = TRUE)
      df4 <- merge(df3, t90_dt, by = c("year", "month"), all = TRUE)
      aci_composites <- merge(df4, t10_dt, by = c("year", "month"), all = TRUE)
      
      # Calculer l'ACI
      aci_composites[, ACI := (T90 - T10 + precipitation + drought + factor * sea_mean + windpower) / 6]
      aci_composites <- aci_composites[, .(year, month, latitude, longitude, windpower, precipitation, drought, T10, T90, sea_mean, ACI)]
      
      return(aci_composites)
    }
  )
)

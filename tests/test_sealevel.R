library(testthat)
library(data.table)
library(lubridate)
library(httr)
library(dplyr)

source("../R/sealevel.R")

create_test_data <- function(directory, filename, dates, values) {
  df <- data.table(Date = dates, Measurement_test = values)
  df$Date <- as.numeric(gsub("-", ".", sprintf("%d.%02d125", year(dates), month(dates))))
  fwrite(df, file.path(directory, filename), sep = ";", col.names = FALSE)
}

test_that("SeaLevelComponent: std_max_consecutive_dry_days method works correctly", {
  country_abrev <- "USA"
  study_period <- c("1960-01-01", "1969-12-31")
  reference_period <- c("1960-01-01", "1964-12-31")
  data_path <- "../data/sealevel_data_USA"
  
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  dates <- seq(as.Date("1960-01-01"), as.Date("1964-12-31"), by = "month")
  values <- runif(length(dates)) * 100
  create_test_data(data_path, "test_file.txt", dates, values)
  
  sea_level_component <- SeaLevelComponent$new(country_abrev, study_period, reference_period)
  
  data <- sea_level_component$load_data()
  expect_is(data, "data.table")
  expect_false(nrow(data) == 0)
  
  corrected_data <- sea_level_component$correct_date_format(data)
  expect_is(corrected_data$Corrected_Date, "Date")
  expect_false(nrow(corrected_data) == 0)
  
  corrected_data[1, Measurement_test := -99999.0]
  cleaned_data <- sea_level_component$clean_data(corrected_data)
  expect_true(is.na(cleaned_data[1, Measurement_test]))
  
  monthly_means <- sea_level_component$compute_monthly_stats(cleaned_data, reference_period, "means")
  monthly_std_devs <- sea_level_component$compute_monthly_stats(cleaned_data, reference_period, "std")
  expect_is(monthly_means, "data.table")
  expect_is(monthly_std_devs, "data.table")
  expect_equal(nrow(monthly_means), 12)
  expect_equal(nrow(monthly_std_devs), 12)
  
  standardized_data <- sea_level_component$standardize_data(cleaned_data, monthly_means, monthly_std_devs, study_period)
  expect_is(standardized_data, "data.table")
  expect_false(nrow(standardized_data) == 0)
  
  standardized_data <- sea_level_component$process()
  expect_is(standardized_data, "data.table")
  expect_false(nrow(standardized_data) == 0)
  
  # Clean up
  unlink(data_path, recursive = TRUE)
})

test_that("SeaLevelComponent: standardize_sealevel method works against precomputed reference anomalies", {
  # Liste des cas de test
  test_cases <- c('test1', 'test2', 'test3', 'test4')
  
  for (test_case in test_cases) {
    test_that(paste("Test case:", test_case), {
      sea_level_data_path <- paste0('../data/tests_data/tests_data_sealevel/', test_case, '_sea_level_test_data.csv')
      reference_anomalies_path <- paste0('../data/tests_data/tests_data_sealevel/', test_case, '_reference_anomalies.csv')
      
      # Lire les données de niveau de la mer
      sea_level_data <- fread(sea_level_data_path)
      reference_anomalies <- fread(reference_anomalies_path)
      
      # Vérification des colonnes
      if (!"Corrected_Date" %in% colnames(sea_level_data)) {
        if ("V1" %in% colnames(sea_level_data)) {
          setnames(sea_level_data, "V1", "Corrected_Date")
        } else {
          stop("La colonne 'Corrected_Date' ou 'V1' est absente dans le fichier de niveau de la mer.")
        }
      }
      
      if (!"Corrected_Date" %in% colnames(reference_anomalies)) {
        if ("V1" %in% colnames(reference_anomalies)) {
          setnames(reference_anomalies, "V1", "Corrected_Date")
        } else {
          stop("La colonne 'Corrected_Date' ou 'V1' est absente dans le fichier d'anomalies de référence.")
        }
      }
      
      sea_level_data$Corrected_Date <- as.Date(sea_level_data$Corrected_Date)
      reference_anomalies$Corrected_Date <- as.Date(reference_anomalies$Corrected_Date)
      
      # Initialiser le composant SeaLevel
      sea_level_component <- SeaLevelComponent$new(
        country_abrev = 'FRA',
        study_period = c("2000-01-01", "2001-12-31"),
        reference_period = c("2000-01-01", "2000-12-31")
      )
      
      # Calculer les anomalies
      calculated_anomalies <- sea_level_component$process()
      # Créer un tableau de comparaison des moyennes calculées et de référence
      calculated_mean <- rowMeans(calculated_anomalies[, grep("^Measurement_", colnames(calculated_anomalies)), with = FALSE], na.rm = TRUE)
      reference_mean <- rowMeans(reference_anomalies[, grep("^Measurement_", colnames(reference_anomalies)), with = FALSE], na.rm = TRUE)
      
      combined_df <- data.table(
        Corrected_Date = calculated_anomalies$Corrected_Date,
        calculated_mean = calculated_mean,
        reference_mean = reference_mean
      )
      
      # Filtrer les NA
      combined_df <- combined_df[!is.na(calculated_mean) & !is.na(reference_mean)]
      
      # Comparer les anomalies aux valeurs de référence
      expect_equal(
        combined_df$calculated_mean,
        combined_df$reference_mean,
        tolerance = 1e-6
      )
    })
  }
})
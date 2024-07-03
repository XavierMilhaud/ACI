library(testthat)
library(ncdf4)
library(data.table)
library(lubridate)
library(ggplot2)
library(RcppRoll)

source("../R/precipitationcomponent.R")

create_test_nc <- function(file_path, times, latitudes, longitudes, data_var) {
  # Créer un fichier NetCDF de test
  nc <- nc_create(file_path, list(
    dim = list(time = length(times), lat = length(latitudes), lon = length(longitudes)),
    var = list(tp = list(dim = c("time", "lat", "lon"), units = "mm", data = data_var))
  ))
  
  # Ajouter les coordonnées
  ncvar_put(nc, "time", times)
  ncvar_put(nc, "lat", latitudes)
  ncvar_put(nc, "lon", longitudes)
  
  # Fermer le fichier
  nc_close(nc)
}

create_test_mask_nc <- function(file_path, latitudes, longitudes) {
  # Créer un masque avec la variable 'country'
  mask_data <- matrix(1, nrow = length(latitudes), ncol = length(longitudes))
  nc <- nc_create(file_path, list(
    dim = list(lat = length(latitudes), lon = length(longitudes)),
    var = list(country = list(dim = c("lat", "lon"), units = "1", data = mask_data))
  ))
  
  # Ajouter les coordonnées
  ncvar_put(nc, "lat", latitudes)
  ncvar_put(nc, "lon", longitudes)
  
  # Fermer le fichier
  nc_close(nc)
}

test_that("monthly_max_anomaly method works correctly", {
  mask_path <- "test_mask.nc"
  times <- seq(ymd("2000-01-01"), ymd("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  precipitation_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), 
                              dim = c(length(times), length(latitudes), length(longitudes)))
  data_path <- "test_data.nc"
  
  create_test_nc(data_path, as.numeric(times), latitudes, longitudes, precipitation_data)
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  reference_period <- c("2000-01-01", "2009-12-31")
  precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period)
  
  anomalies <- precipitation$monthly_max_anomaly()
  
  expect_is(anomalies, "data.table")
  
  # Check the dimensions of the result
  expect_true("time" %in% colnames(anomalies))
  expect_true("latitude" %in% colnames(anomalies))
  expect_true("longitude" %in% colnames(anomalies))
  
  # Check that the result contains the correct time period
  expect_gte(min(anomalies$time), as.Date("2000-01-01"))
  expect_lte(max(anomalies$time), as.Date("2020-12-31"))
  
  # Ensure that the mean anomaly over the reference period is approximately zero
  ref_anomalies <- anomalies[time >= as.Date(reference_period[1]) & time <= as.Date(reference_period[2])]
  mean_anomaly <- mean(ref_anomalies$anomaly, na.rm = TRUE)
  expect_false(is.nan(mean_anomaly))
  expect_equal(mean_anomaly, 0, tolerance = 1)
  
  # Ensure that the standard deviation of anomalies over the reference period is approximately one
  std_anomaly <- sd(ref_anomalies$anomaly, na.rm = TRUE)
  expect_false(is.nan(std_anomaly))
  expect_equal(std_anomaly, 1, tolerance = 1)
})

test_that("monthly_max_anomaly method handles no precipitation", {
  mask_path <- "test_mask.nc"
  times <- seq(ymd("2000-01-01"), ymd("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  # All zeros for no precipitation
  precipitation_data <- array(0, dim = c(length(times), length(latitudes), length(longitudes)))
  data_path <- "test_data.nc"
  
  create_test_nc(data_path, as.numeric(times), latitudes, longitudes, precipitation_data)
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  reference_period <- c("2000-01-01", "2009-12-31")
  precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period)
  
  anomalies <- precipitation$monthly_max_anomaly()
  
  expect_true(all(is.na(anomalies$anomaly)))
})

test_that("monthly_max_anomaly method handles constant precipitation", {
  mask_path <- "test_mask.nc"
  times <- seq(ymd("2000-01-01"), ymd("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  # Constant precipitation value
  precipitation_data <- array(10, dim = c(length(times), length(latitudes), length(longitudes)))
  data_path <- "test_data.nc"
  
  create_test_nc(data_path, as.numeric(times), latitudes, longitudes, precipitation_data)
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  reference_period <- c("2000-01-01", "2009-12-31")
  precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period)
  
  anomalies <- precipitation$monthly_max_anomaly()
  
  expect_true(all(is.na(anomalies$anomaly)))
})

test_that("monthly_max_anomaly method works with multiple test cases", {
  test_cases <- c('test1', 'test2', 'test3', 'test4')
  data_dir <- '../data/tests_data/tests_data_prec_bis'
  
  for (test_case in test_cases) {
    data_path <- file.path(data_dir, paste0(test_case, '_data.nc'))
    mask_path <- file.path(data_dir, paste0(test_case, '_mask.nc'))
    reference_anomalies_path <- file.path(data_dir, paste0(test_case, '_reference_anomalies.nc'))
    
    reference_anomalies <- nc_open(reference_anomalies_path)
    reference_values <- ncvar_get(reference_anomalies, "tp")
    nc_close(reference_anomalies)
    
    precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period_bis)
    anomalies <- precipitation$monthly_max_anomaly()
    
    expect_equal(anomalies$anomaly, reference_values, tolerance = 1e-6)
  }
})

test_that("clean up test data files", {
  mask_path <- "test_mask.nc"
  data_path <- "test_data.nc"
  
  if (file.exists(data_path)) {
    file.remove(data_path)
  }
  
  if (file.exists(mask_path)) {
    file.remove(mask_path)
  }
})

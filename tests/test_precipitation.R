library(testthat)
library(ncdf4)
library(data.table)
library(lubridate)
library(ggplot2)
library(RcppRoll)

source("../R/precipitationcomponent.R")

create_test_nc <- function(file_path, times, latitudes, longitudes, data_var) {
  time_dim <- ncdim_def("time", "days since 1970-01-01", as.numeric(times))
  lat_dim <- ncdim_def("latitude", "degrees_north", latitudes)
  lon_dim <- ncdim_def("longitude", "degrees_east", longitudes)
  var_def <- ncvar_def("tp", "mm", list(lon_dim, lat_dim, time_dim), -9999, prec = "float")
  
  nc <- nc_create(file_path, var_def)
  ncvar_put(nc, var_def, data_var)
  nc_close(nc)
}

create_test_mask_nc <- function(file_path, latitudes, longitudes) {
  lat_dim <- ncdim_def("lat", "degrees_north", latitudes)
  lon_dim <- ncdim_def("lon", "degrees_east", longitudes)
  var_def <- ncvar_def("country", "1", list(lon_dim, lat_dim), -9999, prec = "integer")
  
  mask_data <- matrix(1, nrow = length(longitudes), ncol = length(latitudes))
  nc <- nc_create(file_path, var_def)
  ncvar_put(nc, var_def, mask_data)
  nc_close(nc)
}

test_that("monthly_max_anomaly method works correctly", {
  mask_path <- "test_mask.nc"
  times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  precipitation_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), 
                              dim = c(length(longitudes), length(latitudes), length(times)))
  data_path <- "test_data.nc"
  
  create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  reference_period <- c("2000-01-01", "2009-12-31")
  precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period)
  
  precipitation$calculate_monthly_max()
  anomalies <- precipitation$monthly_max
  
  expect_is(anomalies, "data.table")
  
  expect_true("year" %in% colnames(anomalies))
  expect_true("month" %in% colnames(anomalies))
  expect_true("longitude" %in% colnames(anomalies))
  expect_true("latitude" %in% colnames(anomalies))
  expect_true("rx5day" %in% colnames(anomalies))
  
  expect_gte(min(anomalies$year, na.rm = TRUE), 2000)
  expect_lte(max(anomalies$year, na.rm = TRUE), 2020)
  
  ref_anomalies <- anomalies[year >= 2000 & year <= 2009]
  mean_anomaly <- mean(ref_anomalies$rx5day, na.rm = TRUE)
  expect_false(is.nan(mean_anomaly))
  expect_equal(mean_anomaly, 0, tolerance = 1)
  
  std_anomaly <- sd(ref_anomalies$rx5day, na.rm = TRUE)
  expect_false(is.nan(std_anomaly))
  expect_equal(std_anomaly, 1, tolerance = 1)
})

test_that("monthly_max_anomaly method handles no precipitation", {
  mask_path <- "test_mask.nc"
  times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  # All zeros for no precipitation
  precipitation_data <- array(0, dim = c(length(longitudes), length(latitudes), length(times)))
  data_path <- "test_data.nc"
  
  create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  reference_period <- c("2000-01-01", "2009-12-31")
  precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period)
  
  precipitation$calculate_monthly_max()
  anomalies <- precipitation$monthly_max
  
  expect_true(all(is.na(anomalies$rx5day)))
})

test_that("monthly_max_anomaly method handles constant precipitation", {
  mask_path <- "test_mask.nc"
  times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  # Constant precipitation value
  precipitation_data <- array(10, dim = c(length(longitudes), length(latitudes), length(times)))
  data_path <- "test_data.nc"
  
  create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  reference_period <- c("2000-01-01", "2009-12-31")
  precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period)
  
  precipitation$calculate_monthly_max()
  anomalies <- precipitation$monthly_max
  
  expect_true(all(is.na(anomalies$rx5day)))
})


test_that("monthly_max_anomaly method works with multiple test cases", {
  test_cases <- c('test1', 'test2', 'test3', 'test4')
  reference_period_bis <- c("2000-01-01", "2000-12-31")
  data_dir <- '../data/tests_data/tests_data_prec_bis'
  
  for (test_case in test_cases) {
    data_path <- file.path(data_dir, paste0(test_case, '_data.nc'))
    mask_path <- file.path(data_dir, paste0(test_case, '_mask.nc'))
    reference_anomalies_path <- file.path(data_dir, paste0(test_case, '_reference_anomalies.nc'))
    
    if (!file.exists(data_path) || !file.exists(mask_path) || !file.exists(reference_anomalies_path)) {
      message(paste("Skipping test", test_case, "due to missing files"))
      skip(paste("Skipping test", test_case, "due to missing files"))
    }
    
    reference_nc <- nc_open(reference_anomalies_path)
    reference_tp <- ncvar_get(reference_nc, "tp")
    nc_close(reference_nc)
    reference_tp[is.nan(reference_tp)] <- NA
    reference_tp[is.infinite(reference_tp)] <- NA
    dims_reference_tp <- dim(reference_tp)
    reference_tp_vec <- as.vector(reference_tp)
    precipitation <- PrecipitationComponent$new(data_path, mask_path, reference_period_bis)
    precipitation$calculate_monthly_max()
    anomalies <- precipitation$monthly_max
    anomalies_vec <- as.vector(anomalies$rx5day)
    expect_equal(anomalies_vec, reference_tp_vec, tolerance = 1e-6)
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

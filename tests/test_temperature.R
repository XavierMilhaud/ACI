library(testthat)
library(ncdf4)
library(data.table)
library(lubridate)

source("../R/temperaturecomponent.R")

# Fonction auxiliaire pour créer des fichiers NetCDF de test
create_test_nc <- function(filepath, var_name, var_data, times, latitudes, longitudes) {
  time_dim <- ncdim_def(name = "time", units = "days since 1970-01-01", vals = as.numeric(times - as.Date("1970-01-01")))
  lat_dim <- ncdim_def(name = "latitude", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "longitude", units = "degrees_east", vals = longitudes)
  var_def <- ncvar_def(name = var_name, units = "K", dim = list(time_dim, lat_dim, lon_dim), missval = -999)
  
  nc <- nc_create(filepath, list(var_def))
  ncvar_put(nc, var_def, var_data)
  nc_close(nc)
}

test_that("TemperatureComponent: std_t90 method works correctly", {
  mask_path <- "test_mask.nc"
  temp_path <- "test_temp.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  temp_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(temp_path, "t2m", temp_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  lat_dim <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  country_var <- ncvar_def(name = "country", units = "", dim = list(lat_dim, lon_dim))
  nc <- nc_create(mask_path, list(country_var))
  ncvar_put(nc, country_var, mask_data)
  nc_close(nc)
  
  temp <- TemperatureComponent$new(temp_path, mask_path, reference_period)
  
  standardized_t90 <- temp$std_t90(reference_period)
  expect_is(standardized_t90, "data.table")
  expect_true(all(c("month", "standardized") %in% names(standardized_t90)))
  expect_true(min(standardized_t90$month) >= as.Date("2000-01-01"))
  expect_true(max(standardized_t90$month) <= as.Date("2020-12-31"))
  
  unlink(c(temp_path, mask_path))
})

test_that("TemperatureComponent: std_t10 method works correctly", {
  mask_path <- "test_mask.nc"
  temp_path <- "test_temp.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  temp_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(temp_path, "t2m", temp_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  lat_dim <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  country_var <- ncvar_def(name = "country", units = "", dim = list(lat_dim, lon_dim))
  nc <- nc_create(mask_path, list(country_var))
  ncvar_put(nc, country_var, mask_data)
  nc_close(nc)
  
  temp <- TemperatureComponent$new(temp_path, mask_path, reference_period)
  
  standardized_t10 <- temp$std_t10(reference_period)
  expect_is(standardized_t10, "data.table")
  expect_true(all(c("month", "standardized") %in% names(standardized_t10)))
  expect_true(min(standardized_t10$month) >= as.Date("2000-01-01"))
  expect_true(max(standardized_t10$month) <= as.Date("2020-12-31"))
  
  unlink(c(temp_path, mask_path))
})

test_that("TemperatureComponent: t90 and t10 methods work correctly", {
  mask_path <- "test_mask.nc"
  temp_path <- "test_temp.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  temp_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(temp_path, "t2m", temp_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  lat_dim <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  country_var <- ncvar_def(name = "country", units = "", dim = list(lat_dim, lon_dim))
  nc <- nc_create(mask_path, list(country_var))
  ncvar_put(nc, country_var, mask_data)
  nc_close(nc)
  
  temp <- TemperatureComponent$new(temp_path, mask_path, reference_period)
  
  t90_values <- temp$t90(reference_period)
  t10_values <- temp$t10(reference_period)
  
  expect_is(t90_values, "data.table")
  expect_is(t10_values, "data.table")
  expect_true(all(c("month", "frequency_day", "frequency_night", "t90") %in% names(t90_values)))
  expect_true(all(c("month", "frequency_day", "frequency_night", "t10") %in% names(t10_values)))
  expect_true(min(t90_values$month) >= as.Date("2000-01-01"))
  expect_true(max(t90_values$month) <= as.Date("2020-12-31"))
  expect_true(min(t10_values$month) >= as.Date("2000-01-01"))
  expect_true(max(t10_values$month) <= as.Date("2020-12-31"))
  
  unlink(c(temp_path, mask_path))
})

test_that("TemperatureComponent: standardize metric method works correctly with given reference period", {
  mask_path <- "test_mask.nc"
  temp_path <- "test_temp.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  temp_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(temp_path, "t2m", temp_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  lat_dim <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  country_var <- ncvar_def(name = "country", units = "", dim = list(lat_dim, lon_dim))
  nc <- nc_create(mask_path, list(country_var))
  ncvar_put(nc, country_var, mask_data)
  nc_close(nc)
  
  temp <- TemperatureComponent$new(temp_path, mask_path, reference_period)
  
  t90_values <- temp$t90(reference_period)
  standardized_t90 <- temp$standardize_metric(t90_values, reference_period)
  
  expect_is(standardized_t90, "data.table")
  expect_is(standardized_t90, "data.table")
  expect_true(all(c("month", "standardized") %in% names(standardized_t90)))
  expect_true(min(standardized_t90$month) >= as.Date("2000-01-01"))
  expect_true(max(standardized_t90$month) <= as.Date("2020-12-31"))
  
  unlink(c(temp_path, mask_path))
})

test_that("TemperatureComponent: days above and below threshold methods work correctly", {
  mask_path <- "test_mask.nc"
  temp_path <- "test_temp.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  temp_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(temp_path, "t2m", temp_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  lat_dim <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  country_var <- ncvar_def(name = "country", units = "", dim = list(lat_dim, lon_dim))
  nc <- nc_create(mask_path, list(country_var))
  ncvar_put(nc, country_var, mask_data)
  nc_close(nc)
  
  temp <- TemperatureComponent$new(temp_path, mask_path, reference_period)
  
  t90_values <- temp$t90(reference_period)
  t10_values <- temp$t10(reference_period)
  days_above_thresholds <- temp$days_above_thresholds(reference_period)
  days_below_thresholds <- temp$days_below_thresholds(reference_period)
  
  expect_is(days_above_thresholds, "data.table")
  expect_is(days_below_thresholds, "data.table")
  expect_true(all(c("longitude", "latitude", "time", "days_above") %in% names(days_above_thresholds)))
  expect_true(all(c("longitude", "latitude", "time", "days_below") %in% names(days_below_thresholds)))
  expect_true(min(days_above_thresholds$time) >= as.Date("2000-01-01"))
  expect_true(max(days_above_thresholds$time) <= as.Date("2020-12-31"))
  expect_true(min(days_below_thresholds$time) >= as.Date("2000-01-01"))
  expect_true(max(days_below_thresholds$time) <= as.Date("2020-12-31"))
  
  unlink(c(temp_path, mask_path))
})

test_that("TemperatureComponent: standardize metric method works correctly with given reference period", {
  mask_path <- "test_mask.nc"
  temp_path <- "test_temp.nc"
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date('2000-01-01'), as.Date('2020-12-31'), by = "days")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  set.seed(0)
  temp_data <- array(runif(length(times) * length(latitudes) * length(longitudes)), dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(temp_path, "t2m", temp_data, times, latitudes, longitudes)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  lat_dim <- ncdim_def(name = "lat", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def(name = "lon", units = "degrees_east", vals = longitudes)
  country_var <- ncvar_def(name = "country", units = "", dim = list(lat_dim, lon_dim))
  nc <- nc_create(mask_path, list(country_var))
  ncvar_put(nc, country_var, mask_data)
  nc_close(nc)
  
  temp <- TemperatureComponent$new(temp_path, mask_path, reference_period)
  
  t90_values <- temp$t90(reference_period)
  t10_values <- temp$t10(reference_period)
  standardized_t90 <- temp$standardize_metric(t90_values, reference_period)
  standardized_t10 <- temp$standardize_metric(t10_values, reference_period)
  
  expect_is(standardized_t90, "data.table")
  expect_is(standardized_t10, "data.table")
  expect_true(all(c("month", "standardized") %in% names(standardized_t90)))
  expect_true(all(c("month", "standardized") %in% names(standardized_t10)))
  expect_true(min(standardized_t90$month) >= as.Date("2000-01-01"))
  expect_true(max(standardized_t90$month) <= as.Date("2020-12-31"))
  expect_true(min(standardized_t10$month) >= as.Date("2000-01-01"))
  expect_true(max(standardized_t10$month) <= as.Date("2020-12-31"))
  
  unlink(c(temp_path, mask_path))
})


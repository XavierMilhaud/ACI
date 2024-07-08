library(testthat)
library(ncdf4)
library(data.table)
library(zoo)
library(lubridate)

source("../R/droughtcomponent.R")

create_test_nc <- function(file_path, times, latitudes, longitudes, data) {
  time_dim <- ncdim_def("time", units = "days since 2000-01-01", vals = as.numeric(times - as.Date("2000-01-01")))
  lat_dim <- ncdim_def("latitude", units = "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def("longitude", units = "degrees_east", vals = longitudes)
  
  var_def <- ncvar_def("tp", "mm", list(time_dim, lat_dim, lon_dim), -999, prec = "double")
  
  nc <- nc_create(file_path, list(var_def))
  ncvar_put(nc, var_def, data)
  nc_close(nc)
}

create_test_mask_nc <- function(file_path, latitudes, longitudes) {
  lat_dim <- ncdim_def("lat", "degrees_north", vals = latitudes)
  lon_dim <- ncdim_def("lon", "degrees_east", vals = longitudes)
  
  var_def <- ncvar_def("country", "1", list(lat_dim, lon_dim), -999, prec = "integer")
  
  nc <- nc_create(file_path, list(var_def))
  ncvar_put(nc, var_def, matrix(1, nrow = length(latitudes), ncol = length(longitudes)))
  nc_close(nc)
}

test_that("DroughtComponent: std_max_consecutive_dry_days method works correctly", {
  setUp <- function() {
    mask_path <- "test_mask.nc"
    data_path <- 'test_data.nc'
    reference_period <- c("2000-01-01", "2009-12-31")
    
    # Generating test precipitation data
    times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
    latitudes <- seq(48.80, 48.90, by = 0.1)
    longitudes <- seq(2.20, 2.30, by = 0.1)
    set.seed(0)
    precipitation_data <- array(runif(length(times) * length(latitudes) * length(longitudes)),
                                dim = c(length(times), length(latitudes), length(longitudes)))
    
    create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)
    
    # Generating test mask data
    mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
    create_test_mask_nc(mask_path, latitudes, longitudes)
    
    list(mask_path = mask_path, data_path = data_path, reference_period = reference_period)
  }
  
  tearDown <- function(env) {
    file.remove(env$data_path)
    file.remove(env$mask_path)
  }
  
  env <- setUp()
  
  on.exit(tearDown(env))
  drought <- DroughtComponent$new(env$data_path, env$mask_path)
  drought$max_consecutive_dry_days()
  drought$standardize_metric(env$reference_period)
  anomalies <- drought$standardized_dry_days_dt
  
  expect_is(anomalies, "data.table")
  
  # Check the dimensions of the result
  expect_true("date" %in% names(anomalies))  # Changed from "time_day" to "date"
  expect_true("latitude" %in% names(anomalies))
  expect_true("longitude" %in% names(anomalies))
  
  # Check that the result contains the correct time period
  expect_true(min(anomalies$date) >= as.Date("2000-01-01"))
  expect_true(max(anomalies$date) <= as.Date("2020-12-31"))
  
  # Ensure that the mean anomaly over the reference period is approximately zero
  ref_anomalies <- anomalies[date >= as.Date(env$reference_period[1]) & date <= as.Date(env$reference_period[2])]
  mean_anomaly <- mean(ref_anomalies$standardized_dry_days, na.rm = TRUE)
  expect_false(is.na(mean_anomaly), info = "Mean anomaly should not be NaN.")
  expect_equal(mean_anomaly, 0, tolerance = 0.1)
  
  # Ensure that the standard deviation of anomalies over the reference period is approximately one
  std_anomaly <- sd(ref_anomalies$standardized_dry_days, na.rm = TRUE)
  expect_false(is.na(std_anomaly), info = "Std anomaly should not be NaN.")
  expect_equal(std_anomaly, 1, tolerance = 0.1)
})

test_that("DroughtComponent: no precipitation case", {
  mask_path <- "test_mask.nc"
  data_path <- 'test_data.nc'
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  # All zeros for no precipitation
  precipitation_data <- array(0, dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  drought <- DroughtComponent$new(data_path, mask_path)
  drought$max_consecutive_dry_days()
  drought$standardize_metric(reference_period)
  
  anomalies <- drought$standardized_dry_days_dt
  
  expect_true(all(is.na(anomalies$standardized_dry_days)), info = "Anomalies should be NaN when there is no precipitation.")
})

test_that("DroughtComponent: constant precipitation case", {
  mask_path <- "test_mask.nc"
  data_path <- 'test_data.nc'
  reference_period <- c("2000-01-01", "2009-12-31")
  
  times <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "day")
  latitudes <- seq(48.80, 48.90, by = 0.1)
  longitudes <- seq(2.20, 2.30, by = 0.1)
  
  # Constant precipitation value (below the threshold to simulate dry days)
  precipitation_data <- array(0.0005, dim = c(length(times), length(latitudes), length(longitudes)))
  
  create_test_nc(data_path, times, latitudes, longitudes, precipitation_data)
  
  mask_data <- array(1, dim = c(length(latitudes), length(longitudes)))
  create_test_mask_nc(mask_path, latitudes, longitudes)
  
  drought <- DroughtComponent$new(data_path, mask_path)
  drought$max_consecutive_dry_days()
  drought$standardize_metric(reference_period)
  
  anomalies <- drought$standardized_dry_days_dt
  cal <- drought$max_dry_days_dt
  
  expect_true(all(is.na(anomalies$standardized_dry_days)), info = "Anomalies should be NaN when precipitation is constant below the threshold")
  expect_true(all(cal$max_dry_days == cal$max_dry_days[1]), info = "Max consecutive dry days should be the same when precipitation is constant and below the threshold.")
})

test_that("DroughtComponent: standardize_drought method works against precomputed reference anomalies", {
  test_cases <- c('test1', 'test2', 'test3', 'test4')
  reference_period_bis <- c("2000-01-01", "2000-12-31")
  study_period_bis <- c("2000-01-01", "2001-12-31")
  
  for (test_case in test_cases) {
    precipitation_path <- paste0('../data/tests_data/tests_data_drought/', test_case, '_precipitation_test_data.nc')
    mask_path <- paste0('../data/tests_data/tests_data_drought/', test_case, '_mask_test_data.nc')
    reference_anomalies_path <- paste0('../data/tests_data/tests_data_drought/', test_case, '_reference_anomalies.nc')
    
    drought_component <- DroughtComponent$new(precipitation_path, mask_path)
    drought_component$max_consecutive_dry_days()
    drought_component$standardize_metric(reference_period_bis)
    
    reference_nc <- nc_open(reference_anomalies_path)
    reference_data <- ncvar_get(reference_nc, "days_below_thresholds")
    reference_time <- ncvar_get(reference_nc, "time")
    reference_dates <- as.Date("1970-01-01") + reference_time
    
    calculated_df <- data.table(date = drought_component$standardized_dry_days_dt$date, calculated_mean = drought_component$standardized_dry_days_dt$standardized_dry_days)
    reference_df <- data.table(date = reference_dates, reference_mean = as.vector(reference_data))
    
    combined_df <- merge(calculated_df, reference_df, by = "date", all = FALSE)
    combined_df[is.infinite(calculated_mean), calculated_mean := 1e10]
    combined_df[is.infinite(reference_mean), reference_mean := 1e10]
    
    expect_equal(combined_df$calculated_mean, combined_df$reference_mean, tolerance = 0.1, check.attributes = FALSE)
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



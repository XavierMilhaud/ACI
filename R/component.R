library(raster)
library(zoo)

Component <- setRefClass(
  "Component",
  fields = list(
    array = "ANY",
    mask = "ANY",
    file_name = "character"
  ),
  methods = list(
    initialize = function(array, mask, file_name) {
      .self$array <- array
      .self$mask <- mask
      .self$file_name <- file_name
    },
    
    apply_mask = function(var_name, threshold = 0.8) {
      if (is.null(.self$array) || is.null(.self$mask)) {
        stop("Data not loaded. Please ensure precipitation and mask data are loaded.")
      }
      
      cat("Applying mask...\n")
      mask_layer <- .self$mask
      mask_layer[mask_layer < threshold] <- NA
      
      masked_data <- overlay(.self$array, mask_layer, fun = function(x, y) ifelse(y >= threshold, x, NA))
      return(masked_data)
    },
    
    standardize_metric = function(metric, reference_period, area = NULL) {
      start_date <- as.Date(reference_period[1])
      end_date <- as.Date(reference_period[2])
      
      reference <- subset(metric, getZ(metric) >= start_date & getZ(metric) <= end_date)
      cat("Reference period dates: ", getZ(reference), "\n")
      time_index <- as.numeric(format(getZ(reference), "%m"))
      cat("Time index: ", time_index, "\n")
      
      mean <- stackApply(reference, indices = time_index, fun = mean, na.rm = TRUE)
      std <- stackApply(reference, indices = time_index, fun = sd, na.rm = TRUE)
      
      standardized <- (metric - mean) / std
      if (!is.null(area)) {
        return(cellStats(standardized, stat = "mean"))
      } else {
        return(standardized)
      }
    },
    
    calculate_rolling_sum = function(var_name, window_size) {
      masked_data <- .self$apply_mask(var_name)
      cat("Masked data has", nlayers(masked_data), "layers.\n")
      rolling_sum <- calc(masked_data, fun = function(x) rollapply(x, width = window_size, FUN = sum, align = "right", fill = NA))
      cat("Rolling sum calculation complete.\n")
      return(rolling_sum)
    }
  )
)

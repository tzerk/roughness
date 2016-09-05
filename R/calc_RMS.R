#' Calculate Root Mean Square Height after Davidson et al. 2003
#' 
#' This function calculates the Root Mean Square (RMS) Height after Davidson et al. 2003.
#'
#' @param data \code{\link{data.frame}} (\bold{required}): 
#' A log file imported with \code{\link{import_Data}}.
#' 
#' @param ... Currently not used.
#'
#' @return A \code{\link{data.frame}} containing the RMS value.
#'
#' @examples
#' 
#' # Load example data
#' file <- system.file("extdata/raw.log", package = "roughness")
#' data <- import_Data(file)
#' 
#' # find and remove outliers
#' data_cleaned <- find_Outliers(data = data,
#'                               prefilter = TRUE,
#'                               method = c("iqr", "sd", "mad"),
#'                               remove = "mad",
#'                               window = FALSE,
#'                               width = 20,
#'                               step = 10, 
#'                               hist = TRUE,
#'                               plot = TRUE)
#'                               
#' # de-trend the data set
#' data_detrend <- detrend_Data(data_cleaned, plot = TRUE)
#' 
#' # calculate RMS
#' RMS <- calc_RMS(data_detrend)
#' print(RMS)
#' 
#' # note that the function returns a data frame
#' str(RMS)
#' 
#' @export
calc_RMS <- function(data, 
                     ...) {
  
  ## data preparation
  data <- data[ ,1:2]
  colnames(data) <- c("x", "y")
  
  ###################   RMS HEIGHT (Davidson et al. 2003)  ##########################
  
  #mean
  mean <- mean(data$y, na.rm = TRUE)
  
  # calculates root mean square (RMS)
  RMS <-  sqrt(sum((data$y - mean)^2, na.rm = TRUE) / (length(data$y)))
  
  # return values
  results <- data.frame(RMS)
  
  return(results)
}
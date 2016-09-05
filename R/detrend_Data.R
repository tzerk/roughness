#' Linear regression to detrend the data
#'
#' This function performs a linear regression to detrend the data set.
#'
#' @param data \code{\link{data.frame}} (\bold{required}): 
#' A log file imported with \code{\link{import_Data}}.
#' 
#' @param plot \code{\link{logical}} (optional): \code{TRUE} to plot
#' the data set before and after detrending.
#'
#' @return A \code{\link{data.frame}} with detrended input data.
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
#' @export
detrend_Data <- function(data,
                         plot = TRUE) {
  
  
  ## Detrend ----
  colnames(data) <- c("x", "y")
  newdata <- data
  
  
  lm <- lm(y ~ x, data, na.action = na.omit)
  
  newdata$y[!is.na(data$y)] <- newdata$y[!is.na(data$y)] - lm$fitted.values
  
  ## Plot ----
  if (plot) {
    
    ## save original plot parameters and restore them upon end or stop
    par.old.full <- par(no.readonly = TRUE)
    par(mfrow = c(2, 1))
    on.exit(par(par.old.full))
    
    # original data set
    plot(data, main = "Before")
    abline(lm, col = "red", lty = 2)
    
    plot(newdata, main = "After")
    abline(lm(y ~ x, newdata, na.action = na.omit), col = "red", lty = 2)
  }
  
  invisible(newdata)
}
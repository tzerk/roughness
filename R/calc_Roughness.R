#' Calculates various roughness parameters
#'
#' @param data input data
#' @param method which roughness values should be calculated
#' @param ... currently not used
#'
#' @return console output
#' @export
#'
#' @examples
#' # none
calc_Roughness <- function(data, 
                           method = c("rms"), 
                           ...) {
  
  ## data preparation
  data <- data[ ,1:2]
  colnames(data) <- c("x", "y")
  
  ###################   RMS HEIGHT (Davidson et al. 2003)  ##########################
  
  # linear regression
  lm <- lm(y ~ x, data = data)
  
  # detrend the y values
  y.new <- data$y[which(!is.na(data$y))] - as.numeric(lm$fitted.values)
  
  # calculates root mean square (RMS)
  RMS <- sqrt(sum(y.new^2) / (length(y.new)))
  
  
  # return values
  results <- data.frame(RMS = RMS)
  
  return(results)
}
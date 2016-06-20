#' Calculates various roughness parameters
#'
#' @param data input data
#' @param ... currently not used
#'
#' @return console output
#'
#' @examples
#' # none
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
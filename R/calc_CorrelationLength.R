#' Calculate the Correlation length
#'
#' <placeholder>
#' 
#' <placeholder>
#'
#' @param data input data
#' @param plot TRUE or FALSE
#'
#' @return console output
#'
#' @examples
#' 
#' # none available yet
#' 
#' @export
calc_CorrelationLength <- function(data, plot = TRUE) {
  
  colnames(data) <- c("x", "y")
  
  acf <- acf(data$y, lag.max = length(data$y) / 4, na.action = na.pass, plot = plot)$acf
  
  euler <- 1 / exp(1)
  
  # we want to find the very first occurence of euler; we discard all values
  # after the first negative value and onward
  neg <- which(acf < 0)[1]
  if (!is.na(neg))
    acf <- acf[1:neg] 
    
  # determine the closest index and value to 1/e
  l.index <- which.min(abs(acf - euler))
  l <- data$x[l.index]
  
  # add this point to the plot for visual confirmation
  if (plot) {
    points(l.index, euler, col = "red", pch = 16)
    legend("topright", legend = "Value closest to 1/e", pch = 16, col = "red")
  }
  
  return(l)
}
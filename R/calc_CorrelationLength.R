#' Calculate the Correlation length
#'
#' Function to calculate the correlation length.
#' 
#' @param data \code{\link{data.frame}} (\bold{required}): 
#' A log file imported with \code{\link{import_Data}}.
#' 
#' @param plot \code{\link{logical}} (optional): \code{TRUE} to plot
#' the graphs produced by \code{\link{acf}}.
#'
#' @return A plot and a \code{\link{numeric}} value of the correlation length.
#'
#' @examples
#' 
#' # none
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
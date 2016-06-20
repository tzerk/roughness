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
  
  acf <- acf(data$y, lag.max = length(data$y) / 4, na.action = na.pass)
  
  euler <- 1 / exp(1)
  
  
  
  l.index <- which.min(abs(acf$acf - euler))
  l <- data$x[l.index]
  
  return(l)
}
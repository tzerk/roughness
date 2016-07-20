#' Linear regression to detrend the data
#'
#' @param data input data
#' @param plot TRUE or FALSE
#'
#' @return console output
#'
#' @examples
#' 
#' # none availble yet
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
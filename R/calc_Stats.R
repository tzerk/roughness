#' Calculates basic statistics
#'
#' @param data input data
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # none
calc_Stats <- function(data) {
  
  # only take the x and y values of the data set
  df <- data[ ,1:2]
  colnames(df) <- c("x", "y")
  
  # only y values
  y <- na.omit(df[ ,2])
  
  # standard statistical parameters
  mean <- mean(y)
  median <- median(y)
  sd <- sd(y)
  rsd <- sd / mean * 100
  var <- sd^2
  range <- max(y) - min(y)
  min <- min(y)
  max <- max(y)
  
  # linear regression
  lm <- lm(y ~ x, data = df)
  slope <- abs(coef(lm)[2])
  
  # create results object
  res <- data.frame(mean = mean,
                    median = median,
                    sd = sd,
                    rsd = rsd,
                    var = var,
                    range = range,
                    min = min,
                    max = max,
                    slope = slope)
  
  return(res)
}
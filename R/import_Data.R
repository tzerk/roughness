#' Function to import .log files
#'
#' This function imports a .log file to R and produces a \code{data.frame} to 
#' be used for further processing.
#'
#' @param file \code{\link{character}} (\bold{required}): Full path to a log file.
#' 
#' @param warnings \code{\link{logical}} (optional): 
#' \code{TRUE} to show warnings produced by \code{\link{readLines}}.
#'
#' @return Returns a \code{\link{data.frame}}.
#' 
#' @note 
#' The error code \code{65534} is converted to \code{\link{NA}}.
#' 
#' @examples
#' 
#' \dontrun{
#' # not run
#' data <- import_Data("~/PATH/TO/file.log", warnings = FALSE)
#' }
#' 
#' # An example data set can be loaded via
#' file <- system.file("extdata/raw.log", package = "roughness")
#' data <- import_Data(file)
#' 
#' # show first entries of example data set
#' head(data)
#' 
#' @export
import_Data <- function(file, warnings = FALSE) {
  
  # import data
  if (!warnings) 
    options(warn=-1)
  file_string <- paste(readLines(file), collapse=" ")
  if (!warnings) 
    options(warn=0)
  file_split <- strsplit(file_string, split = "#\\*")[[1]]
  
  # remove unwanted characters -> # * \ " 
  file_split <- gsub(file_split, pattern = "\\#", replacement = "")
  file_split <- gsub(file_split, pattern = "\\*", replacement = "")
  file_split <- gsub(file_split, pattern = '"', replacement = "")
  
  file_numeric <- as.numeric(file_split)
  
  # replace error code value 65534 with NA
  file_numeric[file_numeric == 65535] <- NA
  
  # remove first and last NA values
  NA_index <- which(!is.na(file_numeric))
  file_numeric <- file_numeric[min(NA_index) : max(NA_index)]
  
  #converts into centimeters 
  datacmfinal <- as.data.frame(file_numeric) / 100
  
  #adds a new column "ID" to data.frame
  datacmfinal["ID"] <- 1:length(file_numeric)
  datacmfinal["X"] <- datacmfinal$ID * 100 / length(datacmfinal$ID)
  
  # assign proper column names
  datacmfinal <- data.frame(x = datacmfinal$X,
                            y = datacmfinal$file_numeric,
                            id = datacmfinal$ID,
                            boxplot = 0,
                            sd = 0,
                            mad = 0)
  
  return(datacmfinal)
  
}
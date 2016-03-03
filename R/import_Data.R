#' Function to import .log files
#'
#' Insert valuable information here
#'
#' @param file full path to the file
#'
#' @return a data.frame
#' 
#' @examples
#' # none
#' 
#' @export
import_Data <- function(file) {
  
  # import data
  file_string <- paste(readLines(file), collapse=" ")
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
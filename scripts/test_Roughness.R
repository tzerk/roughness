library(roughness)

FOLDERS <- list.files(path = "./inst/extdata/", include.dirs = TRUE, pattern = "^Feldmessung_")[1]


for (i in 1:length(FOLDERS)) { 
  
  FILES <- list.files(path = paste0("./inst/extdata/", FOLDERS[i]), pattern="*.log")
  
  for (j in 1:length(FILES)) { 
    
    # import data
    data <- import_Data(paste0("./inst/extdata/", FOLDERS[i], "/", FILES[j])) 
    
    # find outliers
    data.new <- find_Outliers(data, method = "mad", remove = "mad", window = FALSE,
                              width = 100, step = 50, auto.decide = FALSE, 
                              hist = TRUE, plot = TRUE)
    
    
    
    # calculate roughness parameters
    print(paste("before:", round(calc_Roughness(data), 2)))
    print(paste("after:", round(calc_Roughness(data.new), 2)))
    cat("\n")
    
  }
  
}
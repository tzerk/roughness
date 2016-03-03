#' Function to detect outliers in the roughness data
#' 
#' This function applies three different methods to detect outliers 
#'
#' @param data the .log file
#' 
#' @param method currently implemented: "iqr", "sd", "mad"
#' 
#' @param width width of moving window
#' 
#' @param step step size of moving window
#' 
#' @param plot should the results be plotted?
#' 
#' @param ... currently unused
#'
#' @return A plot is returned
#' 
#' @export
#'
#' @examples
#' # none
find_Outliers <- function(data, 
                          method = c("iqr", "sd", "mad"), 
                          width = 100, 
                          step = 50, 
                          plot = TRUE, 
                          ...) {
    
    datacm <- data
    chunk.indices <- .chunk_Indices(data = datacm, WIDTH = 100, STEP = 50)
    
    for (k in 1:nrow(chunk.indices)) {
      
      datacmfinal <- datacm[chunk.indices[k,1]:chunk.indices[k,2], ]
      
      #adss a new column "log" to data.frame
      #datacm1["log"] <- log(numericdata)
      
      
      ###################  REMOVE OUTLIER BY BOXPLOT  #########################
      
      #plots boxplot, removes outliers according to boxplot and creates new data.frame "no" without outliers
      bp <- boxplot(datacmfinal$y, plot = FALSE)$out
      
      # this removes the outliers of the lower part of the distribution
      # may need to be changed!!!
      bp <- bp[bp < median(datacmfinal$y, na.rm = TRUE)]
      
      temp <- datacmfinal
      temp[which(temp$y %in% bp), "y"] <- NA
      
      # increase outlier-counter in original data.frame
      datacm[which(datacm$y %in% bp), "boxplot"] <- datacm[which(datacm$y %in% bp), "boxplot"] + 1
      
      
      ###################  OUTLIER: THREE TIMES STD DEV  #########################
      
      #removes outliers higher/lower 2*sd
      temp <- datacmfinal
      
      mean <- mean(temp$y, na.rm = TRUE)
      sd <- sd(temp$y, na.rm = TRUE)
      outliers <- na.omit(temp[temp$y < mean - 3 * sd, ])
      temp <- temp[temp$y > mean - 3 * sd, ]
      
      # increase outlier-counter in original data.frame
      datacm[outliers$id, "sd"] <- datacm[outliers$id, "sd"] + 1
      
      
      ###################  OUTLIER: MEDIAN ABSOLUTE DEVIATION  #########################
      
      temp <- datacmfinal
      
      #removes outliers higher/lower 3*MAD
      # b <- 1/quantile(datacmfinal$y, c(0.75), na.rm = TRUE)
      median <- median(temp$y, na.rm = TRUE)
      mad <- mad(x = datacmfinal$y,
                 center = median(datacmfinal$y, na.rm = TRUE),
                 constant = 1.4826,
                 na.rm = TRUE,
                 low = FALSE,
                 high = FALSE)
      
      outliers <- na.omit(temp[temp$y < median - 3 * mad, ])
      temp <- temp[temp$y > median - 3 * mad, ]
      
      # increase outlier-counter in original data.frame
      datacm[outliers$id, "mad"] <- datacm[outliers$id, "mad"] + 1
      
    }# End of loop: chunks
    
    # HERE WE HAVE TO PLOT with datacm
    
    #### PLOT ALL
    
    # set graphical parameters, 2 columns, 4 rows
    par(mfrow = c(4, 2))
    
    #plots "numericdata" as a scatter plot without outliers
    .plot_Outlier(x = datacm$x, y = datacm$y, main = "",
                  mtext = deparse(substitute(data)))
    
    
    # plot histogram
    hist(datacm$y, main = "")
    
    #### PLOT BOXPLOT-METHOD
    temp <- datacm[which(datacm$boxplot == 0), ]
    outliers <- datacm[which(datacm$boxplot != 0), ]
    
    
    .plot_Outlier(x = temp$x, y = temp$y, main = "Boxplot method",
                  mtext = paste("Removed", nrow(outliers), "outliers"))
    
    points(outliers,
           pch = 20, col = "red", cex = 0.3)
    
    hist(temp$y, main = "")
    
    #### PLOT SD METHOD
    temp <- datacm[which(datacm$sd == 0), ]
    outliers <- datacm[which(datacm$sd != 0), ]
    
    .plot_Outlier(x = temp$x, y = temp$y, main = "> 3 SD method",
                  mtext = paste("Removed", nrow(outliers), "outliers"))
    
    points(outliers,
           pch = 20, col = "red", cex = 0.3)
    
    hist(temp$y, main = "")
    
    #### PLOT MAD METHOD
    temp <- datacm[which(datacm$mad == 0), ]
    outliers <- datacm[which(datacm$mad != 0), ]
    
    .plot_Outlier(x = temp$x, y = temp$y, main = "> 3 MAD method",
                  mtext = paste("Removed", nrow(outliers), "outliers"))
    
    points(outliers,
           pch = 20, col = "red", cex = 0.3)
    
    hist(temp$y, main = "")
    
  
}


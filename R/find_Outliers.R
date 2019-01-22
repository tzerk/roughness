#' Function to detect outliers in the roughness data
#' 
#' This function applies three different methods to detect outliers.
#'
#' @param data \code{\link{data.frame}} (\bold{required}): 
#' A log file imported with \code{\link{import_Data}}.
#' 
#' @param prefilter \code{\link{logical}} (optional): 
#' \code{TRUE} to remove values that are +- 10 cm of the median
#' 
#' @param method \code{\link{character}} (optional): 
#' Vector of methods applied to detect outliers.
#' Currently implemented: \code{"iqr"} for Interquartile Range (see \code{\link{IQR}}),
#'  \code{"sd"} for Standard Deviation or \code{"mad"} for
#' Median Absolut Deviation.
#' 
#' @param remove \code{\link{character}} (optional):
#' Returns the original data set with ourliers removed by the
#' specified method (same options as for argument \code{method}).
#' 
#' @param window \code{\link{logical}} (optional):
#' If \code{TRUE} the data set is split in chunks of size specified by \code{width}
#' before applying any of the detection methods.
#' 
#' @param width \code{\link{integer}} (optional): Width of moving window.
#' 
#' @param step \code{\link{integer}} (optional): Step size of moving window.
#' 
#' @param hist \code{\link{logical}} (optional): Should histograms be plotted?
#' 
#' @param plot \code{\link{logical}} (optional): Should the results be plotted?
#' 
#' @param plot.single \code{\link{logical}} (optional):
#' Should the plots be combined or individually presented?
#' 
#' @param ... Currently unused.
#'
#' @return The data set with outliers removed is returned. Optionally, a plot
#' is created
#' 
#' @export
#'
#' @examples
#' 
#' # Load example data
#' file <- system.file("extdata/raw.log", package = "roughness")
#' data <- import_Data(file)
#' 
#' # find outliers
#' data_cleaned <- find_Outliers(data = data,
#'                               prefilter = TRUE,
#'                               method = c("iqr", "sd", "mad"),
#'                               remove = "mad",
#'                               window = FALSE,
#'                               width = 20,
#'                               step = 10, 
#'                               hist = TRUE,
#'                               plot = TRUE)
#' 
#' head(data_cleaned)
#' 
#' # find outliers with window technique
#' data_cleaned2 <- find_Outliers(data = data,
#'                                prefilter = TRUE,
#'                                method = "mad",
#'                                remove = "mad",
#'                                window = FALSE,
#'                                width = 30,
#'                                step = 15, 
#'                                hist = TRUE,
#'                                plot = TRUE)
#'                                
#' head(data_cleaned2)
#' 
find_Outliers <- function(data, 
                          prefilter = TRUE,
                          method = c("iqr", "sd", "mad"),
                          remove = "mad",
                          window = FALSE,
                          width = 20, 
                          step = 10, 
                          hist = TRUE,
                          plot = TRUE,
                          plot.single = FALSE,
                          ...) {
  
  ## rewrite data (backwards compatibility)
  datacm <- data
  
  ## Settings ----
  settings <- list(main = "",
                   mtext = deparse(substitute(data)))
  
  settings <- modifyList(settings, list(...))
  
  ## PREFILTER ----
  if (prefilter) {
    median <- median(data[ ,2], na.rm = TRUE)
    datacm[which(datacm[ ,2] < median - 10), 2] <- NA
    datacm[which(datacm[ ,2] > median + 10), 2] <- NA
  }
  
  if (window)
    chunk.indices <- chunk_Indices(data = datacm, WIDTH = width, STEP = step)
  else
    chunk.indices <- data.frame(start = 1,
                                end = nrow(datacm))

  
  for (k in 1:nrow(chunk.indices)) {
    
    datacmfinal <- datacm[chunk.indices[k,1]:chunk.indices[k,2], ]
    
    ###################  REMOVE OUTLIER BY BOXPLOT  #########################
    
    #creates boxplot and assigns all(upper AND lower) outliers(out) to bp
    bp <- boxplot(datacmfinal$y, plot = FALSE)$out
    
    #overwrites bp only with upper outliers
    #bp <- bp[bp < median(datacmfinal$y, na.rm = TRUE)]
    
    temp <- datacmfinal
    temp[which(temp$y %in% bp), "y"] <- NA
    
    # increase outlier-counter in original data.frame
    datacm[which(datacm$y %in% bp), "boxplot"] <- datacm[which(datacm$y %in% bp), "boxplot"] + 1
    
    
    ###################  OUTLIER: THREE TIMES STD DEV  #########################
    
    #removes outliers higher/lower 3*sd
    temp <- datacmfinal
    
    mean <- mean(temp$y, na.rm = TRUE)
    sd <- sd(temp$y, na.rm = TRUE)
    outliers <- na.omit(temp[(temp$y < mean - 3 * sd)|(temp$y > mean + 3 * sd),])
    temp <- temp[temp$y > mean - 3 * sd, ]
    
    # increase outlier-counter in original data.frame
    datacm[outliers$id, "sd"] <- datacm[outliers$id, "sd"] + 1
    
    
    ###################  OUTLIER: MEDIAN ABSOLUTE DEVIATION  #########################
    
    temp <- datacmfinal
    
    #removes outliers higher/lower 4*MAD
    # b <- 1/quantile(datacmfinal$y, c(0.75), na.rm = TRUE)
    median <- median(temp$y, na.rm = TRUE)
    mad <- mad(x = datacmfinal$y,
               center = median(datacmfinal$y, na.rm = TRUE),
               constant = 1.4826,
               na.rm = TRUE,
               low = FALSE,
               high = FALSE)
    
    outliers <- na.omit(temp[(temp$y < median - 4 * mad)|(temp$y > median + 4 * mad),])
    temp <- temp[temp$y > median - 4 * mad, ]
    
    # increase outlier-counter in original data.frame
    datacm[outliers$id, "mad"] <- datacm[outliers$id, "mad"] + 1
    
  }# End of loop: chunks
  
  # HERE WE HAVE TO PLOT with datacm
  
  #### PLOT ALL
  if (plot) {
    
    
    # set graphical parameters, 2 columns, 4 rows
    if (plot.single)
      par(mfrow = c(length(method) + 1, ifelse(hist, 2, 1)))
    else
      par(mfrow = c(1, ifelse(hist, 2, 1)))
      
    
    #plots "numericdata" as a scatter plot without outliers
    plot_Outlier(x = datacm$x, y = datacm$y, main = settings$main,
                 mtext = settings$mtext)
    
    
    # plot histogram
    if (hist)
      hist(datacm$y, main = "")
    
    #### PLOT BOXPLOT-METHOD
    if ("iqr" %in% method) { 
      temp <- datacm[which(datacm$boxplot == 0), ]
      outliers <- datacm[which(datacm$boxplot != 0), ]
      
      
      plot_Outlier(x = temp$x, y = temp$y, main = "Boxplot method",
                   mtext = paste("Removed", nrow(outliers), "outliers"))
      
      points(outliers,
             pch = 20, col = "red", cex = 0.3)
      
      if (hist)
        hist(temp$y, main = "")
    }
    
    #### PLOT SD METHOD
    if ("sd" %in% method) {
      temp <- datacm[which(datacm$sd == 0), ]
      outliers <- datacm[which(datacm$sd != 0), ]
      
      plot_Outlier(x = temp$x, y = temp$y, main = "> 3 SD method",
                   mtext = paste("Removed", nrow(outliers), "outliers"))
      
      points(outliers,
             pch = 20, col = "red", cex = 0.3)
      
      if (hist)
        hist(temp$y, main = "")
    }
    
    #### PLOT MAD METHOD
    if ("mad" %in% method) {
      temp <- datacm[which(datacm$mad == 0), ]
      outliers <- datacm[which(datacm$mad != 0), ]
      
      plot_Outlier(x = temp$x, y = temp$y, main = "> 4 MAD method",
                   mtext = paste("Removed", nrow(outliers), "outliers"))
      
      points(outliers,
             pch = 20, col = "red", cex = 0.3)
      
      if (hist)
        hist(temp$y, main = "")
    }
    par(mfrow = c(1, 1))
  }
  
  # REMOVE OUTLIERS FROM THE DATA SET AND RETURN TO USER
  if (remove == "mad")
    datacm$y[which(datacm$mad != 0)] <- NA
  if (remove == "sd")
    datacm$y[which(datacm$sd != 0)] <- NA
  if (remove == "iqr")
    datacm$y[which(datacm$boxplot != 0)] <- NA
  
  new.data <- datacm[ ,1:2]
  
  invisible(new.data)
}


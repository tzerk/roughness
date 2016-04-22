###################  PLOT  #################
plot_Outlier <- function(x, y, main, mtext = "") {
  
  plot(x = x, 
       y = y, 
       type = "l", 
       xlim = c(0,100),
       ylim = c(60,30),
       main = main, 
       xlab = "Distanz (cm)",
       ylab = "Hoehe (cm)",
       col = "grey")
  
  points(x, y, pch = 20, cex = 0.2)
  
  mtext(mtext, cex = 0.75)
}


###################  CALCULATE CHUNK INDICES  #################
chunk_Indices <- function(data, WIDTH = 10, STEP = 1) {
  
  LENGTH <- nrow(data)
  START <- seq(1, LENGTH - WIDTH + 1, STEP)
  END <- START + WIDTH - 1
  
  if (END[length(END)] != LENGTH) {
    END[length(END)] <- LENGTH
  }

  DIFF <- END[length(END)] - START[length(START)] + 1
  
  if (DIFF > WIDTH & DIFF - WIDTH >= WIDTH / 2) {
    END[length(END)] <- START[length(START)] + floor(DIFF / 2)
    START <- c(START, START[length(START)] + STEP)
    END <- c(END, LENGTH)
  }
  return(data.frame(start = START, end = END))
}

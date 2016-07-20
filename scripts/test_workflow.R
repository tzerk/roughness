library(roughness)
library(ggplot2)

FOLDERS <- list.files(path = "./inst/extdata/", include.dirs = TRUE, full.names = TRUE, pattern = "^Feldmessung_")
FILES <- as.character(do.call(c, sapply(FOLDERS, function(FOLDER) { 
  list.files(FOLDER, full.names = TRUE, pattern = "*.log") 
})))

# import data set
d <- lapply(FILES, function(x) import_Data(x, warnings = FALSE)) 

# detect outliers
d2 <- lapply(d, function(x) { 
  find_Outliers(x, 
                prefilter = TRUE, # remove values above and below 10 cm of the median
                method = c("iqr", "mad", "sd"), 
                remove = "mad", 
                window = TRUE, 
                width = 20, step = 10, 
                hist = TRUE, plot = FALSE)
})

# detrend data
d3 <- lapply(d2, function(x) detrend_Data(x, plot = FALSE))

# calculate RMS
rms <- as.numeric(unlist(sapply(d3, function(x) calc_RMS(x))))

# calculate correlation length
acl <- as.numeric(unlist(sapply(d3, function(x) calc_CorrelationLength(x, plot = TRUE))))

## combine results
df <- data.frame(
  folder = as.character(sapply(FILES, function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]]) - 1])),
  file = as.character(sapply(FILES, function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])])),
  rms = rms, 
  acl = acl)


## PLOT ----
gg <- ggplot(df, aes(x = rms, y = acl)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ folder)

print(gg)

ggsave("./scripts/ggplot_RMS.vs.ACL_by_Area.pdf")

gg1 <- ggplot(df, aes(x = rms, y = acl)) +
  geom_point(aes(color = folder)) +
  stat_smooth(method = "lm")

print(gg1)

ggsave("./scripts/ggplot_RMS.vs.ACL.pdf")
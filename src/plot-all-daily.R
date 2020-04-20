# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Read records 
meta_data = read_meta_data("meta-data.txt")

# Just a couple examples
record = meta_data[3,]

# I you want to inspect what's in WD
# WD  = load_output(record, "WD")
# head(WD)

plot_variables_by_day(record, c("WeightGain", "CosSinAmplitude", "BCDepth"), dmy("8/9/2017"), 14)
plot_variables(record, c("DetrendedWeight", "CosSinWeight"), dmy("11/9/2017"), 1)
plot_variables(record, c("DetrendedWeight", "CosSinWeight"), dmy("11/9/2017"), 3)

# Plot all data
plots = alply(meta_data, 1, plot_variables_by_day, 
              variables=c("WeightGain", "CosSinAmplitude", "NightSlope", "BCDepth"))

# Write plots to PDF file
pdf("../output/plot-all-daily.pdf", width=13, height=7.5)
l_ply(plots, grid.draw)
dev.off()

# Define function to filter out outliers
no_outliers = function(WD) {
  outliers = find_outliers(WD$NightSlope, 3.4)
  WD$NightSlope[outliers$IsOutlier] = NA
  WD$BCDepth[outliers$IsOutlier] = NA
  WD
}

# Plot all data but without outliers
plots = alply(meta_data, 1, plot_variables_by_day, 
              variables=c("WeightGain", "CosSinAmplitude", "NightSlope", "BCDepth"),
              filter_func=no_outliers)

# Write plots to PDF file
pdf("../output/plot-all-daily-no-outliers.pdf", width=13, height=7.5)
l_ply(plots, grid.draw)
dev.off()


# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Read records 
meta_data = read_meta_data("meta-data.txt")

# Just a couple examples
record = meta_data[3,]
plot_variables_by_day(record, c("WeightGain", "CosSinAmplitude"), dmy("8/9/2017"), 14)
plot_variables(record, c("DetrendedWeight", "CosSinWeight"), dmy("11/9/2017"), 1)

# Plot all data
plots = alply(meta_data, 1, plot_variables_by_day, variables=c("WeightGain", "CosSinAmplitude"))

# Write plots to PDF file
pdf("../output/plot-all-daily.pdf", width=13, height=7.5)
l_ply(plots, grid.draw)
dev.off()



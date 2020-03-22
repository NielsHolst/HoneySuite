# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Get records of interest
meta_data = read_meta_data("meta-data.txt")
az2014 = meta_data[1,]
az2015 = meta_data[2,]

# Show a single plot
variables = c("Weight", "DetrendedWeight", "MaDetrendedWeight", "CosSinWeight", "MaCosSinWeight")
plot_variables(az2014, variables, dmy("1/7/2014"), 4)

# Create a list of lots
daily = c("WeightGain", "CosSinR2", "MaCosSinR2", "CosSinAmplitude", "MaCosSinAmplitude")
plots = list(
  plot_variables(az2014, variables, dmy("1/7/2014"), 7),
  plot_variables(az2014, variables, dmy("1/8/2014"), 7),
  plot_variables_by_day(az2014, daily, dmy("1/7/2014"), 30)
)

# Write plots to PDF file 
pdf("../output/plot-az-mov-avg.pdf", paper="US")
l_ply(plots, grid.draw)
dev.off()



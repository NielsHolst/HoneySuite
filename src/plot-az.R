# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Get records of interest
meta_data = read_meta_data("meta-data.txt")
az2014 = meta_data[1,]
az2015 = meta_data[2,]

# Show a single plot
plot_variables(az2014, c("Weight", "DetrendedWeight", "CosSinWeight"), dmy("1/7/2014"), 1)

# Create a list of lots
plots = list(
  plot_variables(az2014, c("Weight", "DetrendedWeight", "CosSinWeight"), dmy("1/7/2014"), 1),
  plot_variables(az2014, c("Weight", "DetrendedWeight", "CosSinWeight"), dmy("1/7/2014"), 7),
  plot_variables_by_day(az2014, c("WeightGain", "CosSinR2"), dmy("1/7/2014"), 14),

  plot_variables(az2015, c("Weight", "DetrendedWeight", "CosSinWeight"), dmy("1/7/2015"), 1),
  plot_variables(az2015, c("Weight", "DetrendedWeight", "CosSinWeight"), dmy("1/7/2015"), 7),
  plot_variables_by_day(az2015, c("WeightGain", "CosSinR2"), dmy("1/7/2015"), 14)
)

pdf("../output/plot-az.pdf", paper="USr")
l_ply(plots, grid.draw)
dev.off()



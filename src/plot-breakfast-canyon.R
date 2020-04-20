# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Read records 
meta_data = read_meta_data("meta-data.txt")

# Use az-2017 data set as an example
record = meta_data[3,]

# Plot one date with one hive
plot_bc_slr(record, dmy("16/6/2017"), "hive225")

# Plot one date with all hives
plot_bc_slr(record, dmy("16/6/2017"))

# Same on PDF
pdf("../output/plot-breakfast-canyon.pdf", width=10, height=7.5)
plot_bc_slr(record, dmy("16/6/2017"))
dev.off()

# Now do several days
days = as.matrix(0:2)
plots = alply(days, 1, function(day) { plot_bc_slr(record, dmy("16/6/2017")+day) } )

# Same on PDF
pdf("../output/plot-breakfast-canyon-many.pdf", width=10, height=7.5)
l_ply(plots, function(plot) { grid.newpage(); grid.draw(plot) } )
dev.off()





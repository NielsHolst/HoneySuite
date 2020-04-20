# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Process all input data
run("process_input_files.R")
run("append-mov-avg-detrended.R")
run("append-breakfast-canyon.R")


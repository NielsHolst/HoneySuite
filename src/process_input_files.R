## Set source folder and configure
## Only needed if you run this script alone
# setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
# source("lib/configure.R")

# Produce W and WD files 
meta_data = read_meta_data("meta-data.txt")
a_ply(meta_data, 1, process_meta_record)



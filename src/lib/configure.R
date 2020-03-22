# Load all libraries needed
# Make certain that you have all the following packages installed
library(assertthat)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(plyr)
library(reshape2)
library(scales)
library(segmented)
library(stringr)

# Clean up environment
rm(list=ls(all=TRUE))
graphics.off()

# Set the seed of the random number generator (you can change this)
set.seed(131161)

# Prevent automatic conversions to local time zone
Sys.setenv(TZ = "UTC") 

# Establish output folder
if (!dir.exists("../output")) {
  dir.create("../output")
}

# Set up ggplot defaults
# See https://data-se.netlify.com/2018/12/12/changing-the-default-color-scheme-in-ggplot2/
hs_colours = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#999999') 

hs_colour = function(i) { 
  strtoi(c(
    paste0("0X", substr(hs_colours[i], 2,3)),
    paste0("0X", substr(hs_colours[i], 4,5)),
    paste0("0X", substr(hs_colours[i], 6,7))
  ))
}

scale_colour_discrete = function(...) {
  scale_colour_manual(..., values = rep(hs_colours, 10))
}

scale_linetype_discrete = function(...) {
  n = length(hs_colours)
  scale_linetype_manual(..., values = rep(c(rep(1, n), rep(7, n)), 5))
}

theme_set(theme_minimal())

# Run an R script
run = function(name, timed = FALSE) {
  if (timed) print(paste(Sys.time(), "started"))
  source(name)
  if (timed) print(paste(Sys.time(), "finished"))
}

# Define all functions
run("lib/define-misc-functions.R")
run("lib/define-input-functions.R")
run("lib/define-output-functions.R")
run("lib/define-astronomy-functions.R")
run("lib/define-plot-functions.R")

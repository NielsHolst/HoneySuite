## Set source folder and configure
## Only needed if you run this script alone
# setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
# source("lib/configure.R")

append_breakfast_canyon = function(record) {
  print(paste("Appending Breakfast Canyon for", record$FileName))

  # Load the weight data
  W = load_output(record, "W")

  # Fit a segmented model for every Hive*Date instance (time consuming!)
  slr_models = dlply(W, .(Hive,Date), fit_canyon, record=record)
  
  # Remove unsuccesful models
  slr_models = remove_null(slr_models)

  # Save the segmented models
  write_output(record, slr_models)
  # slr_models = load_output(record, "slr_models")  # activate for test purposes
 
  # Extract and save the line segments
  slr_segments = ldply(slr_models, extract_segments, record=record)[,-1]
  write_output(record, slr_segments)
  # slr_segments = load_output(record, "slr_segments")  # activate for test purposes

  # Extract breakfast canyons
  canyons = ddply(slr_segments, .(Hive,Date), categorize)
  canyons$BCCategory = factor(canyons$BCCategory)
  canyons$BCSegment  = factor(canyons$BCSegment)

  # Load and merge with daily weight data
  WD = load_output(record, "WD")
  WD = join(WD, canyons, c("Hive", "Date"))
  
  # Save updated data frame
  write_output(record, WD)
}


# Process all metadata records
meta_data = read_meta_data("meta-data.txt")
a_ply(meta_data, 1, append_breakfast_canyon)

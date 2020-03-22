# Set source folder and configure
setwd("C:/Users/au152367/Documents/RDev/HoneySuite/src")
source("lib/configure.R")

# Compute 24-h moving average and subtract it to detrend weights
ma_detrend = function (w) {
  ma = {}
  for (i in 1:nrow(w)) {
    bounds = w$DateTime[i] + 12*60*60*c(-1,1)
    w_window = subset(w, bounds[1] <= DateTime & DateTime < bounds[2])
    ma = c(ma, w$Weight[i] - mean(w_window$Weight))
  }
  data.frame(
    MaDetrendedWeight = ma
  )
}

# Append weight detrended by moving area and fitted cos-sin wave to W 
# Append cos-sin wave parameters to WD
append_ma_detrend = function(record) {
  print(paste("Appending moving average for", record$FileName))
  # Fetch saves data
  W  = load_output(record, "W")
  WD = load_output(record, "WD")
  # Calculate detrended weight
  ma_detrended = ddply(W, .(Hive), ma_detrend)[,2]
  W = cbind(W, MaDetrendedWeight = ma_detrended)
  # Fit cos-sin wave
  cos_sin_models = ddply(W, .(Hive, Date), cos_sin_model, variable="MaDetrendedWeight")
  parameters = cos_sin_models[c("Hive","Date","CosSinR2","CosSinAmplitude")]
  # Add cos-sin wave parameters to WD
  colnames(parameters)[3:4] = c("MaCosSinR2","MaCosSinAmplitude")
  WD = join(WD, parameters)
  # Add fitted cos-sin wave to W 
  curves = ddply(W, .(Hive, Date), predict_cos_sin_model, cos_sin_models=cos_sin_models)
  W = cbind(W, MaCosSinWeight=curves$CosSinWeight)
  # Save updated data frames
  write_output(record, W)
  write_output(record, WD)
}

# Process all metadata records
meta_data = read_meta_data("meta-data.txt")
a_ply(meta_data, 1, append_ma_detrend)


# Construct path to input file
input_file_path = function(file_name) {
  paste("../input", file_name, sep="/")
}

# Read meta data file
read_meta_data = function(file_name) {
  read.table(input_file_path(file_name), TRUE, "\t", stringsAsFactors=FALSE)
}

# Read exclude file
read_exclude_file = function(record, W) {
  # Construct file path
  file_name = split_file_name(record$FileName)[1]
  file_name = paste0(file_name, "-exclude.txt")
  file_path = input_file_path(file_name)
  assert_that(file.exists(file_path))
  
  # Read file
  E = read.table(file_path, TRUE, "\t", stringsAsFactors=FALSE)
  colnames(E)[1:5] = c("Date","Comment","FromTime","ToTime","Hives")
  
  # Parse date-times
  formats = c("d!/m!/Y! H!:M!:S!",
              "m!/d!/Y! H!:M!:S!",
              "Y!/m!/d! H!:M!:S!")
  E$FromDateTime = parse_date_time(paste(E$Date,E$FromTime), formats, truncated=1)
  E$ToDateTime   = parse_date_time(paste(E$Date,E$ToTime),   formats, truncated=1)
  
  # Parse dates
  formats = c("d!/m!/Y!",
              "m!/d!/Y!",
              "Y!/m!/d!")
  E$Date = parse_date_time(E$Date, formats)

  # Split string at comma and trim away any spaces
  split = function(x) {
    y = strsplit(x, ",")[[1]]
    trimws(y)
  }
  # Expand jokers to all hives
  hive_names = levels(W$Hive)
  ddply(E, .(Date, FromDateTime, ToDateTime), function(x) { data.frame(Hive = if (x$Hives=="*") hive_names else split(x$Hives)) })
}

# Read treatments data frame from file, or else return NULL
read_treatments_file = function(record) {
  # Construct file path
  file_name = split_file_name(record$FileName)[1]
  file_name = paste0(file_name, "-treatments.txt")
  file_path = input_file_path(file_name)
  # Read file if available
  if (file.exists(file_path)) {
    read.table(file_path, TRUE, "\t")
  } else {
    NULL
  }
}

# Takes same-length vectors of weight and weight change rate (kg/h);
# returns a same-length vector with values that should be dropped (TRUE) or not (FALSE);
# will detect singular or double outliers
noise_drop = function(weight, weight_rate) {
  n = length(weight)
  drop = rep(FALSE, n)
  # 3-points
  i = 1
  while (i<n-2) {
    # print(paste(i,n))
    r = weight_rate[i:(i+2)]
    w = weight[i:(i+2)]
    dw13 = abs(w[1] - w[3])
    dw12 = abs(w[1] - w[2])
    if (!any(is.na(c(r,w))) & abs(r[2]) > 1.2  &  dw13 < 0.25*dw12 ) {
      drop[i+1] = TRUE
      i = i+2
    } else {
      i = i+1
    }
  }
  # 4-points
  i = 1
  while (i<n-3) {
    # print(paste(i,n))
    r = weight_rate[i:(i+3)]
    w = weight[i:(i+3)]
    dw14 = abs(w[1] - w[4])
    dw12 = max(abs(w[1] - w[2]), abs(w[1] - w[3]))
    if (!any(is.na(c(r,w))) & (abs(r[2]) > 1.2 | abs(r[3]) > 1.2) & dw14 < 0.20*dw12 ) {
      drop[i+1] = TRUE
      drop[i+2] = TRUE
      i = i+3
    } else {
      i = i+1
    }
  }
  drop
}

# Read one record (i.e., row) of meta data and write data frames with processed hive data
read_and_amend_data = function(record) {
  # Read data file
  W = read.table(input_file_path(record$FileName), TRUE, "\t", stringsAsFactors=FALSE)
  colnames(W)[1] = "Timestamp"
  formats = c("d!/m!/Y! H!:M!:S!",
              "m!/d!/Y! H!:M!:S!",
              "Y!/m!/d! H!:M!:S!")
  W$Timestamp  = parse_date_time(W$Timestamp, formats, truncated=1)
  
  # Convert to long format and keep only complete records
  W = melt(W, id.vars="Timestamp", variable.name="Hive", value.name="Weight" )

  # Read exclude file
  E = read_exclude_file(record, W)
 
  # Replace excluded values with NA
  for (i in 1:nrow(E)) {
    inside_time_interval = (W$Timestamp > E$FromDateTime[i] & W$Timestamp < E$ToDateTime[i])
    hive = as.character(E$Hive[i]) 
    W$Weight[inside_time_interval & W$Hive==hive] = NA
  }
  
  # Only keep complete rows
  W = subset(W, complete.cases(W))

  # Join with treatment codes, if present
  T = read_treatments_file(record)
  if (!is.null(T)) W = join(W, T)

  # Re-order hive names alphabetically
  W$Hive = reorder_levels(W$Hive, order(levels(W$Hive)))

  # Create solar time column
  T = solar_time(record$Latitude, record$Longitude, record$TimeZone, W$Timestamp)
  T = T$SolarDateTime

  # Add time columns
  W = data.frame(
    DateTime = T,
    Date = date(T),
    Hour = hours_since_midnight(T),
    Hive = W$Hive,
    Treatment = W$Treatment,
    Weight = W$Weight
  )
  
  # Add columns for weight and time steps (in hours)
  W = mutate(W,
    WeightStep = na_diff(Weight),
    TimeStep   = na_diff(DateTime)/60, 
    WeightRate = WeightStep/TimeStep
  )
  
  # Mark rows to drop due to weight noise
  W = ddply(W, .(Hive), transform, NoiseDrop = noise_drop(Weight, WeightRate))

  # Write dropped rows to file then drop them
  noise_dropped = subset(W, NoiseDrop)
  print(paste("Dropped", nrow(noise_dropped), "observations due to weight noise"))
  write_output(record, noise_dropped)
  W = subset(W, !NoiseDrop)
  
  # Remove columns after use
  W$NoiseDrop   = 
  W$WeightStep  =
  W$TimeStep    =  
  W$WeightRate  =  {}

  # Re-number rows
  rownames(W) = {}
  W
}


# Estimate weight at midnight using a parabolic fit to the 3 closest observations
# Returns NA if less than 3 observations between 23:00 and 1:00
weight_at_midnight = function(W, hive, date) {
  # Find observations at midnight plus/minus 1 hour
  midnight = as.POSIXct(date)
  from = midnight - 3600
  to = midnight + 3600
  w = subset(W, Hive==hive & DateTime>from & DateTime<to)
  w = w[,c("Hour", "Weight")]
  w = w[complete.cases(w),]
  
  if (nrow(w)>2) {
    # Center Hour at midnight
    ix = w$Hour>12
    w$Hour[ix] = w$Hour[ix] - 24
    # Sort by distance to midnight
    w = w[order(abs(w$Hour)),]
    # Pick the three closest
    w = w[1:3,]
    # The intercept is the estimated weight at x=0=midnight
    model = lm(Weight ~ Hour + I(Hour^2), data=w)
    intercept = coef(model)[1]
  } else {
    intercept = NA
  }
  names(intercept) = {}
  intercept
}

summarize_daily = function(record, W) {
  # Estimate consecutive midnight weights
  WD = ddply(W, .(Hive, Date),  
              function(w) { 
                c(MidnightWeight1 = weight_at_midnight(W, unique(w$Hive), unique(w$Date)),
                  MidnightWeight2 = weight_at_midnight(W, unique(w$Hive), unique(w$Date)+1)
                 ) 
              }
  )
  # Join with treatment codes, if present
  T = read_treatments_file(record)
  if (!is.null(T)) WD = join(WD, T)
  # Compute weight gain
  WD$WeightGain = WD$MidnightWeight2 - WD$MidnightWeight1
  # Return
  WD
}

# Remove excluded hive*date instances 
remove_excluded = function(record, M) {
  E = read_exclude_file(record, M)
  # Reduce data frame
  for (i in 1:nrow(E)) {
    keep = (M$Hive != E$Hive[i]) | (M$Date != E$Date[i])
    M = M[keep,]
  }
  M
}

# Detrends the weight for a hive within one day;
# subtracts the line going from the first midnight to the second
detrend_weight = function(w, WD) {
  wd = subset(WD, Hive==unique(w$Hive) & Date==unique(w$Date))
  weight1 = wd$MidnightWeight1
  weight2 = wd$MidnightWeight2
  x = w$Hour
  y = rep(NA, length(x))
  if (nrow(wd) > 0 && !is.na(weight1) && !is.na(weight2)) {
    y0 = weight1
    y1 = weight2
    x0 = 0
    x1 = 24
    trend = y0 + (x-x0)/(x1-x0)*(y1-y0)
    assert_that(length(w$Weight) == length(trend))
    y = w$Weight - trend
  }
  data.frame(DetrendedWeight=y)
}

# Remove first and last date observations from each hive
remove_ends = function(w) {
  first_date = min(w$Date)
  last_date  = max(w$Date)
  subset(w, Date > first_date & Date < last_date)
}

# Regresses a cosine-sine curve on hour and detrended weight;
# Returns the lm model, or NULL if there are too few observations
cos_sin_model = function(w, variable) {
  # Data from one day only allowed
  assert_that(length(unique(w$Date)) == 1)
  # Pick data to model
  M = data.frame(
    x = w$Hour/24*2*pi,
    y = w[,variable]
  )
  M = M[complete.cases(M),]
  # Require at least 30 data points for the regression
  if (nrow(M)>=30) {
    model = lm(y~cos(x) + sin(x), data=M) 
    co = coef(model)
    r2 = summary(model)$r.squared
    data.frame(Intercept=co[1], Cos=co[2], Sin=co[3], CosSinR2=r2, CosSinAmplitude = 2*sqrt(co[2]^2 + co[3]^2)) 
  } else {
    data.frame(Intercept=NA, Cos=NA, Sin=NA, CosSinR2=NA, CosSinAmplitude=NA)
  }
}

predict_cos_sin_model = function(w, cos_sin_models) {
  co = subset(cos_sin_models, Hive==unique(w$Hive) & Date==unique(w$Date))
  y = rep(NA, nrow(w))
  if (nrow(co) > 0) {
    x = w$Hour/24*2*pi
    icept = co$Intercept
    a = co$Cos
    b = co$Sin
    y = icept + a*cos(x) + b*sin(x)
  }
  data.frame(CosSinWeight = y)
}

process_meta_record = function(record) {
  print(paste("Processing", record$FileName))
  W  = read_and_amend_data(record)
  WD = summarize_daily(record, W)
  
  W  = remove_excluded(record, W)
  WD = remove_excluded(record, WD)
  
  W  = ddply(W, .(Hive), remove_ends)

  M = ddply(W, .(Hive, Date), function(w) { detrend_weight(w, WD) })
  W = cbind(W, DetrendedWeight=M$DetrendedWeight)

  cos_sin_models = ddply(W, .(Hive, Date), cos_sin_model, variable="DetrendedWeight")
  WD = join(WD, cos_sin_models[c("Hive","Date","CosSinR2","CosSinAmplitude")])
  
  curves = ddply(W, .(Hive, Date), predict_cos_sin_model, cos_sin_models=cos_sin_models)
  W = cbind(W, CosSinWeight=curves$CosSinWeight)
  
  maW = W
  maWD = WD
  write_output(record, maW)
  write_output(record, maWD)
}


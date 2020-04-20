# CONFIGURATION BEGIN #

# Analyse data from midnight until sunrise plus these hours
until_hours_after_sunrise = 4

# CONFIGURATION END #

# Global variable used indirectly(!) by segmented.lm
fit_model_data = NULL    

# Fit segmented linear regression until this time
time_limit = function(record, date) {
  sun(record, date)$Rise  + until_hours_after_sunrise 
}

# Fits segmented model n_max times with shifting initial breaks starting at guess_breaks;
# for a 3-break model, breaks slide from (a,b,c) to (a-max_width, b, c+max_width);
# for a 2-break model, breaks slide from (a,c) to (a-max_width, c+max_width);
# for a 1-break model, break slides from (c) to (c+max_width);
fit_canyon_iteratively = function(lin_model, guess_breaks, max_width=2, n_max=50) {
  # Function that checks if the model was successfull
  success = function(model) {all(class(model)!="try-error") && any(str_detect(as.character(model$call), "segmented.lm"))}
  # Set up breaks
  k = max_width/n_max
  breaks_incr = list(k, k*c(-1,1), k*c(-1, 0, 1))
  breaks_incr = breaks_incr[[length(guess_breaks)]]
  cur_breaks = guess_breaks
  # List to keep all models
  models = list()
  # Fit model iteratively
  for (i in 1:n_max) {
    model = try(segmented.lm(lin_model, seg.Z=~Hour, psi=list(Hour=cur_breaks)), silent=TRUE)
    if (success(model)) models = c(models, list(model))
    cur_breaks = cur_breaks + breaks_incr
  }
  
  # Select best model (highest r2)
  best_model = NULL
  if (length(models)>0) {
    r2 = laply(models, function(x) summary(x)$r.squared)
    ix = which(r2==max(r2))
    best_model = models[ix][[1]]
  } 
  # Search for a canyon model some more
  if (is.null(best_model) & n_max<=800) {
    fit_canyon_iteratively(lin_model, guess_breaks, max_width, 2*n_max)
  }
  # Return best model and simple statistics
  list(BestModel=best_model, FitRatio=length(models)/n_max)
}

# Fits segmented model for one hive and date;
# includes data before time_limit()
fit_canyon = function(w, record) {
  hive = unique(w$Hive)
  date = unique(w$Date)
  print(paste(hive, date))
  # Select data in time windows
  w = subset(w, Hour < time_limit(record, date))
  if (nrow(w) < 30) return(NULL)
  # Put data for regression in global variable
  fit_model_data <<- w  
  
  # Fit linear model as a basis for segmented regression
  lin_model= lm(Weight~Hour, data=fit_model_data)
  # Initial guesses for breaks are relative to sunrise
  sunrise = sun(record, date)$Rise
  # Fit 3-break model
  model = fit_canyon_iteratively(lin_model, c(sunrise+0.5,sunrise+2,sunrise+3)) 
  # Or else, fit 2-break model 
  if (is.null(model$BestModel))
    model = fit_canyon_iteratively(lin_model, c(sunrise+0.5,sunrise+2)) 
  # Or else, fit 1-break model 
  if (is.null(model$BestModel))
    model = fit_canyon_iteratively(lin_model, sunrise+0.5)
  # Return the model
  c(Hive=list(hive), Date=list(date), Model=model)
}

# Extract breaks from segmented model
get_breaks = function(model) {
  model$psi[,2]
}
  
# Extract slopes from segmented model
get_slopes = function(model) {
  slope(model)[[1]][,1]
}

# Extracts the lines from a segmented model;
# returns the number of segments and three data frames (or NULL):
# - breaks with SE
# - slopes with SE and intercepts
# - (x,y) data to plot each line
# Note that the segments do not connect even when gap is set false;
# this is corrected through a calculation of the displacement
extract_lines = function(model) {
  if (is.null(model)) return(NULL)
  breaks = get_breaks(model)
  slopes = get_slopes(model)
  # intercepts = intercept(model, gap=FALSE)[[1]][,1]
  intercepts = intercept(model)[[1]][,1]
  x = model$model$Hour
  pred = predict(model)
  endings = c(min(x), breaks, max(x))
  n = length(slopes)
  line_plot = data.frame()
  # Loop through segments
  for (i in 1:n) {
    a = slopes[i]
    b = intercepts[i]
    x1 = endings[i]
    x2 = endings[i+1]
    y1 = a*x1+b
    y2 = a*x2+b
    ix_pred = min(which(x>x1))
    displacement = a*x[ix_pred] + b - pred[ix_pred]
    line_plot = rbind(line_plot, data.frame(Line=paste0("L",i), X=x1, Y=y1-displacement))
    line_plot = rbind(line_plot, data.frame(Line=paste0("L",i), X=x2, Y=y2-displacement))
  }
  rownames(line_plot) = NULL
  list(
    NumSegments = n,
    Breaks = data.frame(Value=breaks, 
               SE=model$psi[,3],
               row.names=1:(n-1)),
    Lines = data.frame(Slope=slopes,
               SlopeSE=slope(model)[[1]][,2],
               Intercept=intercepts,
               row.names=1:n),
    LinePlot = line_plot
  )
}

# Extracts the line segments of a model
extract_segments = function(model, record) {
  best_model = model$Model.BestModel 
  if (is.null(best_model)) return(NULL)
  lines = extract_lines(best_model)
  breaks = lines$Breaks$Value
  slope_values = lines$Lines$Slope
  from = 0
  segments = data.frame()
  n = length(breaks)

  # Loop through segments
  for (i in 1:n) {
    to = breaks[i] 
    segments = rbind(segments, c(i, from, to, slope_values[i]))
    from = to
  }

  segments = rbind(segments, c(n+1, from, time_limit(record,model$Date), slope_values[n+1]))
  segments = data.frame(segments)
  colnames(segments) = c("Segment", "From", "To", "Slope")
  
  mutate(segments,   
    R2=summary(best_model)$r.squared,
    Hive=model$Hive, 
    Date=model$Date, 
    NumBreaks = n
  )
}


# Finds the night segment, i.e. the segment of the longest duration before canyon rim;
# returns a data frame with info on that segment
extract_night = function(slps, category) {
  # Put slopes in order of decreasing slope
  slps = mutate(slps, Duration=To-From)
  slps = slps[order(slps$Duration,decreasing=TRUE),]
  
  # Locate canyon rim
  rim = category$BCHourBegin
  M = data.frame(
        NightFrom  = NA,
        NightTo    = NA,
        NightSlope = NA
      )
  if (is.na(rim)) return(M)
  
  # Pick the first (and longest segment ending before the rim)
  ix = 1
  while (slps$To[ix]>rim) ix=ix+1
  M = data.frame(
    NightFrom = slps$From[ix],
    NightTo = slps$To[ix],
    NightSlope = slps$Slope[ix]
  )
  M$NightDuration = M$NightTo - M$NightFrom
  M
}

# Data frame for mishappen categories
na_category =
  data.frame(
    BCSegment   = NA,
    BCHour      = NA,
    BCHourBegin = NA,
    BCHourEnd   = NA,
    BCSlope     = NA,
    BCSlopeUp   = NA,
    BCDepth     = NA
  )                   

# Extracts canyon information from segments where ix is the segment leading into the canyon
# returns a single-row data frame
extract_category = function(segments, ix) {
  # Last segment cannot be chosen
  if (nrow(segments) == ix) return(na_category) 
  M = data.frame(              
    BCSegment = ix,
    BCHour = segments$To[ix],         # Canyon bottom 
    BCHourBegin = segments$From[ix],  # Canyon left rim
    BCHourEnd = segments$To[ix+1],    # Canyon right rim
    BCSlope = segments$Slope[ix],
    BCSlopeUp = segments$Slope[ix+1],
    BCDepth = -segments$Slope[ix]*(segments$To[ix]-segments$From[ix]),
    BCJoined_2_3 = FALSE
  )   
  # Join segment 2 and 3, if slope2 < slope3
  if (ix==3 & segments$Slope[2] < segments$Slope[3]) {
    M2 = data.frame(              
      BCSegment = 2,
      BCHour = M$BCHour,
      BCHourBegin = segments$From[2],
      BCHourEnd = M$BCHourEnd,
      BCSlopeUp = M$BCSlopeUp,
      BCDepth = -segments$Slope[2]*(segments$To[2]-segments$From[2]) + M$BCDepth,
      BCJoined_2_3 = TRUE
    )   
    M2$BCSlope = with(M2, -BCDepth/(BCHour - BCHourBegin))
    M = M2
  }
  M
}

# Categorizes the segment slopes for a Hive*Date instance;
# returns a data frame with one row
categorize = function(segments) {
  # Which slopes are negative?
  neg = which(segments$Slope<0)
  # Turn vector into an integer number with as many digits as there are negative slopes;
  # each digit means the corresponding slope is negative
  course = if (length(neg)>0) as.numeric(paste(neg,collapse="")) else 0
  # Branch out according to the course;
  # cat will become a data frame with one row containing canyon information (segment number, hour, slope, depth)
  cat = if (course==0) {
    na_category
  } else if (course==1) {
    na_category
  } else if (course==2) {
    extract_category(segments, 2)
  } else if (course==3) {
    extract_category(segments, 3)
  } else if (course==4) {
    na_category
  } else if (course==12) {
    extract_category(segments, 2)
  } else if (course==13) {
    extract_category(segments, 3)
  } else if (course==14) {
    na_category
  } else if (course==23) {
    extract_category(segments, 3)
  } else if (course==24) {
    extract_category(segments, 2)
  } else if (course==34) {
    na_category
  } else if (course==123) {
    extract_category(segments, 3)
  } else if (course==124) {
    extract_category(segments, 2)
  } else if (course==134) {
    na_category
  } else if (course==234) {
    na_category
  } else if (course==1234) {
    na_category
  } else {
    stop(paste("Unknown course:", course))
  }
  cat = mutate(cat, BCCategory = course, BCNumSegments = unique(segments$NumBreaks) + 1, BCR2=unique(segments$R2))
  cbind(cat, extract_night(segments, cat))
}

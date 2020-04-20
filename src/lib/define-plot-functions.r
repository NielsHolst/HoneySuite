# Create a plot title from record
title_from_record = function(record) {
  file_name = split_file_name(record$FileName)[1]
  paste("File:", file_name)
}

# Plot variables for one day
plot_variables_1_day = function(record, variables, W, from_date) {
  # Get sunrise and sunset hours
  sun_ = sun(record, from_date)
  # Pick variables and dates
  id_variables = c("Hour", "Hive", "Treatment")
  w = subset(W, Date==from_date)
  w = w[c(id_variables, variables)]
  w = melt(w, id.vars=id_variables, variable.name="Variable", value.name="Value")
  # Plot
  ggplot(w) +
    geom_rect(xmin=0, xmax=sun_$Rise, ymin=-Inf, ymax=Inf, fill="grey80", colour=NA) +
    geom_rect(xmin=sun_$Set, xmax=24, ymin=-Inf, ymax=Inf, fill="grey80", colour=NA) +
    geom_line(aes(x=Hour, y=Value, colour=Treatment, group=Hive)) +
    scale_x_continuous(breaks=4*(0:6)) +
    # guides(colour = guide_legend(reverse=TRUE)) +
    labs(title=paste(title_from_record(record), "~ Date:", from_date)) +
    facet_wrap(~Variable, scales="free_y", ncol=1)
}

# Plot variables for many days
plot_variables_n_days = function(record, variables, W, from_date, num_days) {
  # Create data frame to discern day from night
  T = solar_time(record$Latitude, record$Longitude, record$TimeZone, as.POSIXct(from_date) - 60*60*12 + (0:(num_days+1))*24*60*60)
  Tbefore = T[1,]
  Tafter = T[num_days+2,]
  T = T[(1:num_days)+1,]
  sun = data.frame(
    NightBegin = c(Tbefore$SunsetSolarTime, T$SunsetSolarTime),
    NightEnd   = c(T$SunriseSolarTime, Tafter$SunriseSolarTime)
  )
  # Pick variables and dates
  id_variables = c("DateTime", "Hive", "Treatment")
  the_dates = date(T$SolarDateTime)
  w = subset(W, Date %in% the_dates)
  w = w[c(id_variables, variables)]
  w = melt(w, id.vars=id_variables, variable.name="Variable", value.name="Value")

  # Insert missing values for missing dates
  ix = which(diff(date(w$DateTime))>1)
  if (length(ix) > 0) {
    for (i in 1:length(ix)) {
      p = ix[i]
      w = rbind(
        w[1:p,],
        w[p,],
        w[p+1,],
        w[(p+1):nrow(w),]
      )
      w$DateTime[p+1] = w$DateTime[p+1] + 1
      w$DateTime[p+2] = w$DateTime[p+2] - 1
      w$Value[p+1] = NA
      w$Value[p+2] = NA
      ix = ix + 2
    }
  }
  # Plot
  ggplot(w) +
    geom_rect(aes(xmin=NightBegin, xmax=NightEnd), ymin=-Inf, ymax=Inf, fill="grey80", colour=NA, data=sun) +
    geom_line(aes(x=DateTime, y=Value, colour=Treatment, group=Hive)) +
    scale_x_datetime(breaks = T$SolarDateTime,  labels = date_format("%e %b")) +
    labs(title=title_from_record(record), x="Date") +
    facet_wrap(~Variable, scales="free_y", ncol=1)
}

# Plot variables from W data frame
plot_variables = function(record, variables, from_date, num_days=1, filter_func=NULL) {
  # Load data
  W  = load_output(record, "W")
  # Filter data
  if (!is.null(filter_func)) W = filter_func(W)
  # Construct title
  file_name = split_file_name(record$FileName)[1]
  title = paste("File:", file_name)
  # Make a special plot for only one day
  if (num_days==1) 
    plot_variables_1_day(record, variables, W, from_date) 
  else
    plot_variables_n_days(record, variables, W, from_date, num_days)
}

# Plot variables from WD data frame
plot_variables_by_day = function(record, variables, from_date=NA, num_days=NA, filter_func=NULL) {
  # Load daily data
  WD  = load_output(record, "WD")
  # Filter data
  if (!is.null(filter_func)) WD = filter_func(WD)
  # Set up time period
  if (is.na(from_date)) from_date = min(WD$Date)
  if (is.na(num_days)) num_days = as.numeric(max(WD$Date) - from_date) + 1
  # Pick variables and dates
  id_variables = c("Date", "Hive", "Treatment")
  w = subset(WD, Date>=from_date & Date<from_date+num_days)
  w = w[c(id_variables, variables)]
  w = melt(w, id.vars=id_variables, variable.name="Variable", value.name="Value")
  w$Date = as.POSIXct(w$Date)
  
  # Set up vertical line for treatment date
  treatment_date = record$TreatmentDate
  treatment_line = if (treatment_date < from_date | treatment_date > from_date+num_days) NULL else 
                   geom_vline(xintercept=treatment_date, size=2, alpha=0.3, colour="brown")
  
  # Insert missing values for dates missing
  jumps = diff(w$Date)
  ix = which(jumps > 1)
  if (length(ix) > 0) {
    jumps = jumps[ix]-1
    blanks = {}
    for (i in 1:length(ix)) {
      blank = w[ix[i],]
      blank$Value = NA
      for (j in 1:jumps[i]) {
        blank$Date = blank$Date+24*60*60
        blanks = rbind(blanks, blank)
      }
    }
    w = rbind(w, blanks)
  }
  # Plot
  breaks = if (num_days>250) "months" else if (num_days>100) "2 weeks" else if (num_days>14) "weeks" else "days"
  ggplot(w, aes(x=Date, y=Value, colour=Treatment, group=Hive)) +
    treatment_line +
    geom_line() +
    geom_point(size=0.5) +
    facet_wrap(~Variable, scales="free_y", ncol=1) +
    labs(title=title_from_record(record)) +
    scale_x_datetime(breaks=breaks, minor_breaks=NULL, labels = date_format("%e %b")) 
}

# Plot one Breakfast Canyon segmented linear regression model
plot_bc_slr_model = function(record, slr_model, n) {
  # Load data needed for plot
  W  = load_output(record, "W")
  WD = load_output(record, "WD")

  # Extract info
  date = slr_model$Date
  hive = slr_model$Hive
  wd = subset(WD, Hive==hive & Date==date)
  r2 = round(wd$BCR2,2)
  
  # Get sunrise and sunset hours
  sun_ = sun(record, date)
  # Get observed weights
  M = subset(W, Hive==hive & Date==date)
  # Get SLR lines
  slr_lines = extract_lines(slr_model$Model.BestModel)
  slr_data = slr_lines$LinePlot

  # The plot
  slr_line_size = if (n==1) 2 else 2.5
  P = ggplot(M, aes(x=Hour, y=Weight)) + 
    geom_rect(xmin=0, xmax=sun_$Rise, ymin=-Inf, ymax=Inf, fill="grey80", colour=NA) +
    geom_rect(xmin=sun_$Set, xmax=24, ymin=-Inf, ymax=Inf, fill="grey80", colour=NA) +
    guides(colour=FALSE) +
    geom_line(aes(X, Y, colour=Line), data=slr_data, size=slr_line_size) +
    geom_line() +
    scale_x_continuous(breaks=4*(0:6), limits=c(0,24)) 
    
    if (n==1) {
      P = P + 
        labs(title=paste(date, hive, paste0("r2=",r2), sep=" ~ "))
    } else {
      P = P +
        theme(plot.title = element_text(size=10)) +
        labs(title=paste(hive, r2, sep=" ~ "), x="", y="")
    }
    P
}

plot_bc_slr = function(record, date, hive=NULL) {
  # Load SLR models and subset by date and hive
  slr_models = load_output(record, "slr_models")
  slr_models = find_slr_models(slr_models, date, hive)  
  n = length(slr_models)
  # Create one or more plots
  plots = llply(slr_models, function(slr_model) {plot_bc_slr_model(record, slr_model, n)})
  # Return one or more plots
  if (n==1) {
    plots[[1]]
  } else {
    grid.arrange(grobs=plots, top=as.character(date))
  }
}
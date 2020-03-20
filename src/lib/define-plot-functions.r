# Create a plot title from record
title_from_record = function(record) {
  file_name = split_file_name(record$FileName)[1]
  paste("File:", file_name)
}

# Plot variables for one day
plot_variables_1_day = function(record, variables, W, from_date) {
  # Find today's sunrise and sunset
  T = solar_time(record$Latitude, record$Longitude, record$TimeZone, as.POSIXct(from_date) + 60*60*12)
  # Pick variables and dates
  id_variables = c("Hour", "Hive", "Treatment")
  the_date = date(T$SolarDateTime)
  w = subset(W, Date==the_date)
  w = w[c(id_variables, variables)]
  w = melt(w, id.vars=id_variables, variable.name="Variable", value.name="Value")
  # Plot
  ggplot(w) +
    geom_rect(xmin=0, xmax=hours_since_midnight(T$SunriseSolarTime), ymin=-Inf, ymax=Inf, fill="grey80", colour=NA) +
    geom_rect(xmin=hours_since_midnight(T$SunsetSolarTime), xmax=24, ymin=-Inf, ymax=Inf, fill="grey80", colour=NA) +
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
    # guides(colour = guide_legend(reverse=TRUE)) +
    scale_x_datetime(breaks = T$SolarDateTime,  labels = date_format("%e %b")) +
    labs(title=title_from_record(record), x="Date") +
    facet_wrap(~Variable, scales="free_y", ncol=1)
}

# Plot variables from W data frame
plot_variables = function(record, variables, from_date, num_days) {
  # Load data
  W = load_output(record, "W")
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
plot_variables_by_day = function(record, variables, from_date, num_days) {
  # Load daily data
  WD  = load_output(record, "WD")
  # Pick variables and dates
  id_variables = c("Date", "Hive", "Treatment")
  w = subset(WD, Date>=from_date & Date<from_date+num_days)
  w = w[c(id_variables, variables)]
  w = melt(w, id.vars=id_variables, variable.name="Variable", value.name="Value")
  w$Date = as.POSIXct(w$Date)
  
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
  G = ggplot(w, aes(x=Date, y=Value, colour=Treatment, group=Hive)) +
    geom_line() +
    geom_point() +
    # guides(colour = guide_legend(reverse=TRUE)) +
    facet_wrap(~Variable, scales="free_y", ncol=1) +
    labs(title=title_from_record(record))
  if (num_days>15) G else G + scale_x_datetime(breaks = "days", labels = date_format("%e %b"))
}

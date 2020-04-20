# Differences between values in vector headed by an NA
na_diff = function(x) {
  c(NA, diff(x))
}

# Lag 2-differences between values in vector headed by an 2 NAs
na_diff2 = function(x) {
  c(NA, NA, diff(x, lag=2))
}

# Re-order levels of factor. Example:
# x = factor(sample(letters[1:5],100, replace=TRUE))
# x = reorder_levels(x, c(4,5,1:3))
# x
reorder_levels = function(the_factor, new_order) {
  factor(the_factor,levels(the_factor)[new_order])
}

# Split file name into file name proper and suffix
# split_file_name("def.abc.txt") gives "def.abc" "txt"
split_file_name = function(s) {
  dots = strsplit(s, "\\.")[[1]]
  n = length(dots)
  c(
    paste(dots[1:(n-1)], collapse="."),
    dots[n]
  )
}

# Returns hours since midnight
hours_since_midnight = function(date_time) {
  as.numeric(difftime(date_time, date(date_time), units="hours"))
}

# Returns list with NULL values removed
remove_null = function(lst) {
  ix = which(sapply(lst, is.null))
  if (length(ix)==0) lst else lst[-ix]
}

# Return a data frame pointing out those values in vector x, that lie outside sd_max standard deviations
find_outliers = function(x, sd_max) {
  M = data.frame(Value=x)
  M$Std = NA
  M$IsOutlier = NA
  ix = !is.na(x)
  M$Std[ix] = scale(x[ix])
  M$IsOutlier[ix] = abs(M$Std[ix]) > sd_max
  M$IsOutlier[is.na(M$IsOutlier)] = FALSE
  M
}

# Find the SLR model for date and hive; returns NULL if not found
find_slr_models = function(slr_models, date, hive=NULL) {
  ix_date = which(sapply(slr_models, function(x) x$Date==date))
  # Find by both date and hive
  if (!is.null(hive)) {
    ix_hive = which(sapply(slr_models, function(x) x$Hive==hive))
    ix = intersect(ix_hive, ix_date) 
  # Find by date only
  } else {
    ix = ix_date
  }
  if (length(ix)==0) NULL else slr_models[ix]
}

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


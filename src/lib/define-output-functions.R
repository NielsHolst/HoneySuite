# Construct path to output file
output_file_path = function(record, name) {
  file_name = split_file_name(record$FileName)[1] 
  variable_file_name = paste0(file_name, "-", name, ".Rdata")
  paste0("../output/", variable_file_name)
 }

# Save variable to output folder
write_output = function(record, variable) {
  # Get the name of the variable used by the caller
  variable_name = deparse(substitute(variable)) 
  path = output_file_path(record, variable_name)
  save(variable, file = path)
  print(paste(variable_name, "written to", path))
}

# Load variable from output folder
load_output = function(record, variable_name) {
  load(output_file_path(record, variable_name))
  variable
}

# create function to update values of a variable in a df
update_variable_values <- function(data, variable, old_values, new_values){
  stopifnot(is.data.frame(data))
  stopifnot(is_character(variable))
  stopifnot(length(old_values) == length(new_values))
  
  df <- data
  
  for (i in seq_along(old_values)) {
    df[ df[ , variable] == old_values[i], variable] <- new_values[i]
  }
  
  df
}
# create function to add mean and sd for 1 variable/column
add_mean_sd_helper <- function(data,
                               var,
                               grouping_arguments) {
  
  stopifnot(is.data.frame(data))
  stopifnot(is_character(var))
  stopifnot(is_character(grouping_arguments))
  stopifnot(!(colnames(data) %in% "temp_mean"))
  stopifnot(!(colnames(data) %in% "temp_sd"))
  
  # calculare mean and sd of desired variable based on grouping arguments
  
  mean_sd <- data %>%
    dplyr::group_by(!!!dplyr::syms(grouping_arguments)) %>% # see https://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions
    dplyr::summarise(temp_mean = mean(!!dplyr::sym(var), na.rm = TRUE),
              temp_sd = sd(!!sym(var), na.rm = TRUE))
  
  # merge mean and sd values with initial df
  df <- left_join(data, mean_sd)
  
  # change mean and sd column names
  colnames(df)[colnames(df) == "temp_mean"] <- paste0(var, "_mean")
  colnames(df)[colnames(df) == "temp_sd"] <- paste0(var, "_sd")
  
  # output df
  df
}

# create function to add mean and sd for as many variables/columns as desired
add_mean_sd <- function(data,
                        vars,
                        grouping_arguments) {
  
  stopifnot(is_character(vars))
  
  # loop to add mean and sd for each var indicated
  for (i in seq_along(vars)) {
    
    suppressMessages( # prevents many summarise and left_join messages to console 
      data <- add_mean_sd_helper(data,
                                 var = vars[i],
                                 grouping_arguments)
    )
  }
  
  # output df
  data
}
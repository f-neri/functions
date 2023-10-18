# create function to add foldchange for 1 variable/column
add_fold_change_helper <- function(data,
                                   var_to_normalize,
                                   grouping_arguments,
                                   normalization_variable,
                                   normalization_value) {
  
  stopifnot(is_character(var_to_normalize))
  stopifnot(is_character(grouping_arguments))
  stopifnot(is_character(normalization_variable))
  stopifnot(!(colnames(data) %in% "normalization_mean"))
  stopifnot(!(colnames(data) %in% "normalization_mean_fc"))
  
  # get normalization df, which contains mean values based on grouping arguments and
  # filtered for normalization variable = normalization value
  
  norm_df <- data %>%
    group_by(!!!syms(grouping_arguments)) %>% # see https://stackoverflow.com/questions/27975124/pass-arguments-to-dplyr-functions
    summarise(normalization_mean = mean(!!sym(var_to_normalize))) %>%
    filter(!!sym(normalization_variable) == normalization_value) %>%
    .[ , colnames(.) != normalization_variable]
  
  # merge norm_df with data to add the normalization mean values
  df <- left_join(data, norm_df)
  
  # calculate fold change & drop normalization_mean column
  df <- df %>%
    mutate(normalization_mean_fc = !!sym(var_to_normalize) / normalization_mean) %>%
    .[ , colnames(.) != "normalization_mean"]
  
  colnames(df) <- colnames(df) %>%
    str_replace(pattern = "normalization_mean_fc", replacement = paste0(var_to_normalize, "_fc"))
  
  df
}

# create function to add fold change for as many variables/columns as desired
add_fold_change <- function(data,
                            vars_to_normalize,
                            grouping_arguments,
                            normalization_variable,
                            normalization_value) {
  
  stopifnot(is_character(vars_to_normalize))
  
  for (i in seq_along(vars_to_normalize)) {
    
    suppressMessages( # prevents many summarise and left_join messages to console 
      data <- add_fold_change_helper(data,
                                     var_to_normalize = vars_to_normalize[i],
                                     grouping_arguments,
                                     normalization_variable,
                                     normalization_value)
    )
  }
  
  data
  
}
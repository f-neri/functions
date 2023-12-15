# function to easily source multiple custom functions
load_functions <- function(f_names, file_path = "C:/Users/ffran/AppData/Local/R/functions") {
  stopifnot(is_character(f_names))
  
  # add .R if absent
  new_names <- f_names
  
  for (i in seq_along(new_names)) {
    
    if ( !(grepl("\\.R$", new_names[i])) ) {
      new_names[i] <- paste0(new_names[i], ".R")
    }
    
  }
  
  # generate full paths
  full_paths <- file.path(file_path, new_names)
  
  # create function to source an .R file and handle errors
  source_with_error_handling <- function(path) {
    # Attempt to source the file
    tryCatch(
      {
        source(path)
      },
      error = function(e) {
        # Handle the error and display a custom error message
        cat(paste("'", path, "' does not exist.\n", sep = ""), file = stderr())
      }
    )
  }
  
  # source all paths 
  for (i in seq_along(full_paths)) {
    source_with_error_handling(full_paths[i])
  }
  
}

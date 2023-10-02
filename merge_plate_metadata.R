function() {
  # importing metadata regarding conditions of each well (IR or CTL, full serum or serum-starved, different drug concentrations etc.)
  
  ## plater
  file_path <- str_c(getwd(),"/",plate_template_name,".csv", sep = "") # gets string with full path
  
  plate_metadata <- read_plate(
    file = file_path,             # full path to the .csv file
    well_ids_column = "well",    # name to give column of well IDs (optional)
    sep = ","                     # separator used in the csv file (optional)
  )
  
  ## change variable names to lower case, removing parenthesis (if present)
  colnames(plate_metadata) <- colnames(plate_metadata) %>%
    tolower() %>%
    str_replace_all(., "[()]", "")
  
  ## check that plate-template contains Condition variable
  if (any(colnames(plate_metadata) %in% "condition") == TRUE) {} else {
    beep(1)
    Sys.sleep(2)
    stop(
      "The metadata entered in plate-template must contain the \"Condition\" variable"
    )}
  
  ## check that the variables contained in plate-template are limited to
  ## condition, and up to 2 additional variable (e.g. serum and/or drug treatment)
  additional_variables <- colnames(plate_metadata) %>%
    .[-grep(pattern = "well|condition", .)]
  
  if (length(additional_variables) > 2) {
    beep(1)
    Sys.sleep(2)
    stop(
      "The metadata entered in plate-template is not acceptable.

The only metadata that can be entered in the plate-template file are
\"Condition\" and up to TWO more variable (e.g. \"Serum\" and/or \"Drug_concentration\")")
  }
  
  additional_variables_check <-  if (length(additional_variables) > 0) {TRUE} else {FALSE}
  multiple_additional_variables_check <- if (length(additional_variables) == 2) {TRUE} else {FALSE}
  
  # add metadata info (conditions & serum) to table with observations (signal intensities for each cell)
  
  ## mutating join with left_join()
  
  plate_metadata_variables <- colnames(plate_metadata)[-1]
  
  tidy_data6 <- tidy_data5 %>%
    left_join(plate_metadata, by = "well") %>%
    select(well, cell_ID, all_of(plate_metadata_variables), everything())
}
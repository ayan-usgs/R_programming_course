CSV_PATH <- 'C:\\Users\\ayan\\Downloads\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv'


column_validation <- function(column_names, substring) {
  col_list <- list(heart_attack = 13,
                   heart_failure = 19,
                   pneumonia = 25
                   )
  column_names_lower <- tolower(column_names)
  substring_exists <- grepl(substring, column_names_lower)
  substring_exists_sum <- sum(substring_exists)
  if (substring_exists_sum > 0) {
    substring_exists_logical <- TRUE
    return(substring_exists_logical)
  }
  else {
    stop('invalid outcome')
  }
}


state_validation <- function(states, state_abbrev) {
  unique_states <- unique(states)
  state_valid <- any(state_abbrev == states)
  if (state_valid) {
    return(TRUE)
  }
  else {
    stop('invalid state')
  }
}


na_replace <- function(dataframe, old_str, new_str = NA) {
  dataframe[dataframe == old_str] = new_str
  return(dataframe)
}


clean_str <- function(string, old_str= ' ', new_str= '.') {
  lower_str <- tolower(string)
  new_str <- gsub(old_str, new_str, lower_str)
}


filter_by_state <- function(dataframe, state_abbrev, na_string = 'Not Available') {
  df_state <- dataframe[dataframe$State == state_abbrev, ]
  df_state_na <- na_replace(df_state, na_string)
  df_complete <- complete.cases(df_state_na)
  df_state_complete <- df_state_na[df_complete, ]
}


col_num_assign <- function(outcome_string) {
  col_list <- list(heart_attack = 13,
                   heart_failure = 19,
                   pneumonia = 25)
  if (outcome_string == 'heart attack') {
    col_num <- col_list$heart_attack
  }
  else if (outcome_string == 'heart failure') {
    col_num <- col_list$heart_failure
  }
  else if (outcome_string == 'pneumonia') {
    col_num <- col_list$pneumonia
  }
  return(col_num)
}


best <- function(state, outcome, csv_path = CSV_PATH) {
  outcome_data <- read.csv(csv_path, colClasses = 'character')
  states <- outcome_data[, 7]
  state_valid <- state_validation(states, state)
  column_names <- colnames(outcome_data)
  outcome_str <- clean_str(outcome)
  outcome_valid <- column_validation(column_names, outcome_str)
  if (state_valid && outcome_valid) {
    df_state_complete <- filter_by_state(outcome_data, state)
    outcome_lower <- tolower(outcome)
    col_num <- col_num_assign(outcome_lower)
    df_state_complete[, col_num] <- as.numeric(df_state_complete[, col_num])
    outcome_min <- min(df_state_complete[, col_num])
    df_min <- df_state_complete[, col_num] == outcome_min
    df_min_records <- df_state_complete[df_min, ]
    hospital_names <- df_min_records$Hospital.Name
    sorted_names <- sort(hospital_names)
    first_hospital <- sorted_names[[1]]
    return(first_hospital)
  }
}
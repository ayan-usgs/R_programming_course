source('utils_3.R')

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
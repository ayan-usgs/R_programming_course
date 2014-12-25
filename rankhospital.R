source('utils_3.R')

rankhospital <- function(state, outcome, num, csv_path = CSV_PATH) {
  outcome_lower <- tolower(outcome)
  outcome_data <- read.csv(csv_path, colClasses = 'character')
  states <- outcome_data[, 7]
  state_valid <- state_validation(states, state)
  column_names <- colnames(outcome_data)
  outcome_str <- clean_str(outcome)
  outcome_valid <- column_validation(column_names, outcome_str)
  if (state_valid && outcome_valid) {
    df_state_complete <- filter_by_state(outcome_data, state)
    if (num == 'best') {
      num <- 1
    }
    else if (num == 'worst') {
      num <- nrow(df_state_complete)
    }
    else {
      num <- num
    }
    col_num <- col_num_assign(outcome_lower)
    df_state_complete[, col_num] <- as.numeric(df_state_complete[, col_num])
    df_ordered <- df_state_complete[with(df_state_complete, order(df_state_complete[, col_num], 
                                                                  df_state_complete[, 'Hospital.Name']
                                                                  )
                                         ), 
                                    ]
    hospital_name_num <- df_ordered[num, 'Hospital.Name']
  }
  return(hospital_name_num)
}
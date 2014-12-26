source('utils_3.R')


rankall_1 <- function(outcome, num, csv_path = CSV_PATH) {
  outcome_data <- read.csv(csv_path, colClasses = 'character')
  outcome_lower <- tolower(outcome)
  column_names <- colnames(outcome_data)
  outcome_str <- clean_str(outcome)
  outcome_valid <- column_validation(column_names, outcome_str)
  hosptial <- c()
  state <- c()
  if (outcome_valid) {
    states <- outcome_data[, 7]
    unique_states <- unique(states)
    for (unique_state in unique_states) {
      hospital_name <- rankhospital(unique_state, num)
      state_abbrev <- unique_state
      hospital <- append(hosptial, hospital_name)
      state <- append(state, state_abbrev)
    }
  }
  rank_df <- data.frame(hospital, state)
}


rankall <- function(outcome, num, csv_path = CSV_PATH) {
  outcome_lower <- tolower(outcome)
  col_num <- col_num_assign(outcome_lower)
  outcome_data <- read.csv(csv_path, colClasses = 'character')
  outcome_data <- na_replace(outcome_data, 'Not Available')
  outcome_data[, col_num] <- as.numeric(outcome_data[, col_num])
  states <- outcome_data[, 7]
  unique_states <- unique(states)
  states_sorted <- sort(unique_states)
  outcome_complete <- complete.cases(outcome_data)
  complete <- outcome_data[outcome_complete, ]
  hospital <- c()
  state <- c()
  complete_ordered <- complete[with(complete, order(complete[, 7],
                                                    complete[, col_num],
                                                    complete[, 'Hospital.Name']
                                                    )
                                    ),
                               ]
  for (state_abbrev in states_sorted) {
    state_df <- filter_by_state(complete_ordered, state_abbrev)
    if (num == 'best') {
      rank_num <- 1
    }
    else if (num == 'worst') {
      number_of_rows <- nrow(state_df)
      rank_num <- number_of_rows
    }
    else {
      rank_num <- num
    }
    # print(paste(state_abbrev, rank_num, sep=': '))
    hospital_name <- state_df[rank_num, 'Hospital.Name']
    hospital <- append(hospital, hospital_name)
    state <- append(state, state_abbrev)
    if (state_abbrev == 'HI') {
      return(state_df)
    }
  }
  hospital_rank <- data.frame(hospital, state)
}
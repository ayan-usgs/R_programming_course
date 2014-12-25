source('utils_3.R')
source('rankhospital.R')


rankall <- function(outcome, num, csv_path = CSV_PATH) {
  outcome_data <- read.csv(csv_path, colClasses = 'character')
  outcome_lower <- tolower(outcome)
  column_names <- colnames(outcome_data)
  outcome_str <- clean_str(outcome)
  outcome_valid <- column_validation(column_names, outcome_str)
  if (outcome_valid) {
    states <- outcome_data[, 7]
  }
}
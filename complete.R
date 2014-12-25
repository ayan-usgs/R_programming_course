complete <- function(csv_directory, id=1:332) {
  csv_filenames <- sprintf('%03d.csv', id)
  csv_paths <- sprintf('%s/%s', csv_directory, csv_filenames)
  nobs <- c()
  for (csv_path in csv_paths) {
    normalized_path <- normalizePath(csv_path)
    dataframe <- read.csv(normalized_path, header=TRUE)
    df_complete_cases <- complete.cases(dataframe)
    complete_cases_sum <- sum(df_complete_cases)
    nobs <- append(nobs, complete_cases_sum)
  }
  df <- data.frame(id, nobs)
  return(df)
}
source('complete.R')

corr <- function(csv_directory, threshold=0) {
  df_complete_count <- complete(csv_directory)
  complete_logical <- df_complete_count[[2]] > threshold
  filtered_df <- df_complete_count[complete_logical,]
  monitor_ids <-filtered_df[[1]]
  cor_vector <- c()
  for (monitor_id in monitor_ids) {
    csv_name <- sprintf('%03d.csv', monitor_id)
    csv_path <- sprintf('%s/%s', csv_directory, csv_name)
    normalized_path <- normalizePath(csv_path)
    df <- read.csv(normalized_path, header=TRUE)
    complete_cases <- complete.cases(df)
    df_complete <- df[complete_cases,]
    sulfate <- df_complete[['sulfate']]
    nitrate <- df_complete[['nitrate']]
    correlation <- cor(sulfate, nitrate)
    cor_vector <- append(cor_vector, correlation)
  }
  return(cor_vector)
}
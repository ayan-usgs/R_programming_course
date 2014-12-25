pollutantmean <- function(csv_directory, pollutant_name, id=1:332) {
  csv_filenames <- sprintf('%03d.csv', id)
  csv_paths <- sprintf('%s/%s', csv_directory, csv_filenames)
  for (csv_path in csv_paths) {
   dataframe_exists <- exists('dataframe')
   normalized_path = normalizePath(csv_path)
   if (dataframe_exists) {
     read_data <- read.csv(normalized_path, header=TRUE)
     dataframe <- rbind(dataframe, read_data)
     rm(read_data)
   }
   else {
     dataframe <- read.csv(normalized_path, header=TRUE)
   }
  }
  pollutant_vector <- dataframe[[pollutant_name]]
  pollutant_mean <- mean(pollutant_vector, na.rm=TRUE)
  return(pollutant_mean)
}
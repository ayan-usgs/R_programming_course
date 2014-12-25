csv_path <- 'C:\\Users\\ayan\\Downloads\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv'
outcome <- read.csv(csv_path, colClasses = 'character')
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

pollutantmean <- function(directory, pollutant = "sulfate", doc_range = 1:2) {
  my_list <- vector()
  for (i in doc_range) {
    if (i < 10)
      path = paste(directory, "/00", as.character(i), ".csv", sep="")  
    else if (i < 100)
      path = paste(directory, "/0", as.character(i), ".csv", sep="")  
    else
      path = paste(directory, "/", as.character(i), ".csv", sep="")
    df <- read.csv(path, header = TRUE)
    my_list <- c(my_list, df[[pollutant]])
  }
  print(mean(my_list, na.rm = TRUE))
}

pollutantmean("specdata", "nitrate", 23)


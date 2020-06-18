
pollutantmean <- function(directory, pollutant = "sulfate", doc_range = 1:332) {
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

round(pollutantmean("specdata", "nitrate"), 3)
  
complete <- function(directory = "specdata", range = 3:100) {
  m <- vector()
  for (i in range) {
    if (i < 10)
      path = paste(directory, "/00", as.character(i), ".csv", sep="")  
    else if (i < 100)
      path = paste(directory, "/0", as.character(i), ".csv", sep="")  
    else
      path = paste(directory, "/", as.character(i), ".csv", sep="")  
    df <- read.csv(path, header = TRUE)
    vector1 <- c(i, length(df$nitrate[complete.cases(df$nitrate) & complete.cases(df$sulfate)]))
    m <- rbind(m, vector1, deparse.level = 0)
  }
  m1 <- as.data.frame(m)
  names(m1) <- c("id", "nobs")
  m1
}
complete()

corr <- function(directory= "specdata", threshold = 0) {
  t1 <- complete(range = 1:332)
  ind <- t1$id[t1$nobs > threshold]
  cor_i <- vector()
  for (i in ind) {
    if (i < 10)
      path = paste(directory, "/00", as.character(i), ".csv", sep="")  
    else if (i < 100)
      path = paste(directory, "/0", as.character(i), ".csv", sep="")  
    else
      path = paste(directory, "/", as.character(i), ".csv", sep="")  
    df <- read.csv(path, header = TRUE)
    cor_i <- append(cor_i, cor(df$nitrate, df$sulfate, use = "pairwise.complete.obs"))
  }
  cor_i
}

corr(threshold = 150)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

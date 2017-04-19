library(dplyr)

train.data <- read.csv('data/train.csv')
test.data <- read.csv('data/test.csv')

preprocess.parseCabin <- function(data) {
  data$Cabin <- gsub('^.+? .+$', '', data$Cabin)
  
  data$Cabin.category <- factor(substr(data$Cabin, 1, 1))
  
  data$Cabin.number <- strtoi(substr(data$Cabin, 2, 10))
  data$Cabin.number[is.na(data$Cabin.number)] <- 0
  
  return(data)
}

show_all <- function(data) {
  data$Survived <- factor(data$Survived)
  data$PassengerId <- NULL
  data$Name <- NULL
  data$Ticket <- NULL
  data$Cabin <- NULL
  ggpairs(data, aes_string(colour='Survived', alpha=0.5))
}

train.data <- preprocess.parseCabin(train.data)
show_all(train.data)

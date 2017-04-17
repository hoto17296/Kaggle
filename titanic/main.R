library(dplyr)
library(kernlab)
library(ranger)
library(caret)

# Preprocessing

## Estimate missing age data
preprocess.fillMissingAge <- function(data) {
  # Extract titles
  data$Name.Title <- factor(gsub('^.+ (.+?)\\. .+$', '\\1', data$Name))
  
  # Multiple regression analysis
  ageFilledData <- data[!is.na(data$Age),]
  model <- lm(Age ~ Survived + Sex + Pclass + SibSp + Parch + Name.Title, data = ageFilledData)
  
  # Relative mean error
  ans <- ageFilledData$Age
  print(mean(abs((predict(model) - ans) / ans)))
  
  # Estimate
  data$Age[is.na(data$Age)] <- predict(model, data[is.na(data$Age),])
  
  return(data)
}

# Analysis

## Logistic regression
method.glm <- function(train, test) {
  model <- glm(Survived ~ Pclass + Sex + Age, data = train, family = binomial)
  pred <- predict(model, test, type = 'response')
  return(ifelse(pred > 0.5, 1, 0))
}

## SVM
method.svm <- function(train, test) {
  model <- ksvm(Survived ~ Pclass + Sex + Age, data = train)
  pred <- predict(model, test, type = 'response')
  return(ifelse(pred > 0.5, 1, 0))
}

## Random forest
method.ranger <- function(train, test) {
  model <- ranger(Survived ~ Pclass + Sex + Age, data = train, write.forest = TRUE)
  pred <- predict(model, test, type = 'response')
  return(ifelse(pred$predictions > 0.5, 1, 0))
}

# Validation

## Validation using training data
test.self <- function(method, data) {
  pred <- method(data, data)
  tbl <- table(data$Survived, pred)
  return(sum(diag(tbl)) / sum(tbl))
}

## K-fold cross validation
test.cv <- function(method, data, K=10) {
  folds <- cut(seq(1,nrow(data)), breaks=K, labels=FALSE)
  precisions <- c()
  for (i in 1:K) {
    test <- data[which(folds==i), ]
    train <- data[-which(folds==i), ]
    pred <- method(train, test)
    tbl <- table(test$Survived, pred)
    precision <- sum(diag(tbl)) / sum(tbl)
    precisions <- c(precisions, precision)
  }
  return(mean(precisions))
}

# Main process

data <- read.csv('data/train.csv')

data <- preprocess.fillMissingAge(data)

results <- data.frame(
  method = c('glm', 'svm', 'ranger'),
  self = c(
    test.self(method.glm, data),
    test.self(method.svm, data),
    test.self(method.ranger, data)
  ),
  cv = c(
    test.cv(method.glm, data),
    test.cv(method.svm, data),
    test.cv(method.ranger, data)
  )
)
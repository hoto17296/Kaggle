library(dplyr)
library(kernlab)
library(ranger)
library(caret)

# Preprocessing

## Estimate missing age data
preprocess.fillMissingAge <- function(train, test) {
  # Extract titles
  train$Name.Title <- factor(gsub('^.+ (.+?)\\. .+$', '\\1', train$Name))
  test$Name.Title <- factor(gsub('^.+ (.+?)\\. .+$', '\\1', test$Name), levels = levels(train$Name.Title))
  
  # Multiple regression analysis
  ageFilledData <- train[!is.na(train$Age),]
  model <- lm(Age ~ Sex + Pclass + SibSp + Parch + Name.Title, data = ageFilledData)
  model$xlevels[['Name.Title']] <- levels(train$Name.Title)
  
  # Relative mean error
  ans <- ageFilledData$Age
  print(mean(abs((predict(model) - ans) / ans)))
  
  # Estimate
  test$Age[is.na(test$Age)] <- predict(model, test[is.na(test$Age),])
  
  return(test)
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
train.data <- read.csv('data/train.csv')
test.data <- read.csv('data/test.csv')
all.data <- merge(train.data, test.data, all=T)

## Training
train.data <- preprocess.fillMissingAge(all.data, train.data)

train.results <- data.frame(
  method = c('glm', 'svm', 'ranger'),
  self = c(
    test.self(method.glm, train.data),
    test.self(method.svm, train.data),
    test.self(method.ranger, train.data)
  ),
  cv = c(
    test.cv(method.glm, train.data),
    test.cv(method.svm, train.data),
    test.cv(method.ranger, train.data)
  )
)

# Prediction
test.data <- preprocess.fillMissingAge(all.data, test.data)

test.data$Survived <- method.ranger(train.data, test.data)

write.csv(test.data[,c('PassengerId', 'Survived')], 'submission.csv', row.names = FALSE)

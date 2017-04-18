library(dplyr)
library(ranger)

preprocess <- function(data) {
  # 欠損値を埋める
  
  ## NA が少ない かつ ほとんどの値が RL のため
  data[is.na(data$MSZoning), 'MSZoning'] <- 'RL'
  
  ## 中央値が 68 のため
  data[is.na(data$LotFrontage), 'LotFrontage'] <- 68
  
  ## ほとんどの値が NA のため
  data[is.na(data$Alley), 'Alley'] <- NULL
  
  ## NA 以外全て 'AllPub' なため
  data[is.na(data$Utilities), 'Utilities'] <- 'AllPub'
  
  return(data)
}

preprocess.util <- function(data, column) {
  print(paste('NA count:', nrow(data[is.na(data[, column]),])))
  table(data[, column])
  #plot(data[, column])
}

# Root Mean Squared Logarithmic Error / 標準二乗対数誤差
rmsle <- function(data, pred) {
  return(sqrt(1/length(pred)*sum((log(pred +1)-log(data +1))^2)))
}

train.data <- preprocess(read.csv('data/train.csv', stringsAsFactors=FALSE))
test.data <- preprocess(read.csv('data/test.csv', stringsAsFactors=FALSE))

# Columns that contains NA
colnames(test.data)[colSums(is.na(test.data)) > 0]

model <- ranger(SalePrice ~ . - LotFrontage - Alley - MasVnrType - MasVnrArea - BsmtQual
                - BsmtCond - BsmtExposure - BsmtFinType1 - BsmtFinType2 - Electrical
                - FireplaceQu - GarageType - GarageYrBlt - GarageFinish - GarageQual
                - GarageCond - PoolQC - Fence - MiscFeature, data = train.data, write.forest = TRUE)

pred <- predict(model, train.data, type = 'response')
rmsle(train.data$SalePrice, pred$predictions)

test.data$pred <- predict(model, test.data, type = 'response')

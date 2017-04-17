library(dplyr)
library(ranger)

train.data <- read.csv('data/train.csv')

model <- ranger(SalePrice ~ . - LotFrontage - Alley - MasVnrType - MasVnrArea - BsmtQual
                - BsmtCond - BsmtExposure - BsmtFinType1 - BsmtFinType2 - Electrical
                - FireplaceQu - GarageType - GarageYrBlt - GarageFinish - GarageQual
                - GarageCond - PoolQC - Fence - MiscFeature, data = train.data, write.forest = TRUE)
pred <- predict(model, train.data, type = 'response')

rmsle(train.data$SalePrice, pred$predictions)

# Root Mean Squared Logarithmic Error / 標準二乗対数誤差
rmsle <- function(data, pred) {
  return(sqrt(1/length(pred)*sum((log(pred +1)-log(data +1))^2)))
}

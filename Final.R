setwd("")
read.csv("train.csv", stringsAsFactors = FALSE)
train<-read.csv("train.csv", stringsAsFactors = FALSE)
read.csv("test.csv", stringsAsFactors = FALSE)
test<-read.csv("test.csv", stringsAsFactors = FALSE)
describe(train)
summary(train)
str(train)
library(ggplot2)
## Save the ID column so that we can drop it from merged dataset (combine)
train_ID = train$Id
test_ID = test$Id

## test doesn't have SalePrice column, so add it.
test$SalePrice = NA
qplot(train$GrLivArea, train$SalePrice, main = "With Outliers")
train <- train[-which(train$GrLivArea > 4000 & train$SalePrice < 3e+05)                ]
## Check again after removal.
qplot(train$GrLivArea, train$SalePrice, main = "Without Outliers")
## Plot histogram of SalePrice Variable - Right skewed
qplot(SalePrice, data = train, bins = 50, main = "Right skewed distribution")
## Log transformation of the target variable
train$SalePrice <- log(train$SalePrice + 1)

## Normal distribution after transformation
qplot(SalePrice, data = train, bins = 50, main = "Normal distribution after log transformation")
## Combine train and test
combine<-rbind(train, test)

## Dropping Id as it is unnecessary for the prediction process.
combine<-combine[, -1]
#check for missing data
colSums(is.na(combine))
#For most of the categorical features, NA values will be imputed as ‘None’, because referring to the data_description.txt #file, the NA of these variables represent values such as ‘No Garage’,‘No Basement’, etc.
#For most of the numerical features, NA values will be replaced by 0, for variables like GarageArea, GarageCars, etc.
#For some categorical features like Functional and Electrical, the NA values will be replaced by the most frequently occuring value for that variable.

## For some variables, fill NA with 'None'
for (x in c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "GarageCond", "BsmtQual", "BsmtCond", 
            "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "MasVnrType")) {
  combine[is.na(combine[, x]), x] = "None"
}

# Group by neighborhood and fill in missing value by the median
# LotFrontage of all the neighborhood
temp = aggregate(LotFrontage ~ Neighborhood, data = combine, median)
temp2 = c()
for (str in combine$Neighborhood[is.na(combine$LotFrontage)]) {
  temp2 = c(temp2, which(temp$Neighborhood == str))
}
combine$LotFrontage[is.na(combine$LotFrontage)] = temp[temp2, 2]

## Replacing missing data with 0
for (col in c("GarageYrBlt", "GarageArea", "GarageCars", "BsmtFinSF1", 
              "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", 
              "MasVnrArea")) {
  combine[is.na(combine[, col]), col] = 0
}

## Replace missing MSZoning values by 'RL'
combine$MSZoning[is.na(combine$MSZoning)] = "RL"

## Remove Utilities as it has zero variance
combine = combine[, -9]

## Replace missing Functional values with 'Typ'
combine$Functional[is.na(combine$Functional)] = "Typ"

## Replace missing Electrical values with 'SBrkr'
combine$Electrical[is.na(combine$Electrical)] = "SBrkr"

## Replace missing KitchenQual values by 'TA'
combine$KitchenQual[is.na(combine$KitchenQual)] = "TA"

## Replace missing SaleType values by 'WD'
combine$SaleType[is.na(combine$SaleType)] = "WD"

## Replace missing Exterior1st and Exterior2nd values by 'VinylSd'
combine$Exterior1st[is.na(combine$Exterior1st)] = "VinylSd"
combine$Exterior2nd[is.na(combine$Exterior2nd)] = "VinylSd"

## All NAs should be gone, except the test portion of SalePrice
## variable, which we ourselves had initialized to NA earlier.
colSums(is.na(combine))

#Transforming some numerical variables that are really categorical
combine$MSSubClass = as.character(combine$MSSubClass)
combine$OverallCond = as.character(combine$OverallCond)
combine$YrSold = as.character(combine$YrSold)
combine$MoSold = as.character(combine$MoSold)
#Label Encoding some categorical variables that may contain information in their ordering set
#specify the order of the levels, converting categories to integer ranks - 1 to n, the categorical variables.

cols = c("FireplaceQu", "BsmtQual", "BsmtCond", "GarageQual", "GarageCond", 
         "ExterQual", "ExterCond", "HeatingQC", "PoolQC", "KitchenQual", "BsmtFinType1", 
         "BsmtFinType2", "Functional", "Fence", "BsmtExposure", "GarageFinish", 
         "LandSlope", "LotShape", "PavedDrive", "Street", "Alley", "CentralAir", 
         "MSSubClass", "OverallCond", "YrSold", "MoSold")

FireplaceQu = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtCond = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageCond = c("None", "Po", "Fa", "TA", "Gd", "Ex")
ExterQual = c("Po", "Fa", "TA", "Gd", "Ex")
ExterCond = c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC = c("Po", "Fa", "TA", "Gd", "Ex")
PoolQC = c("None", "Fa", "TA", "Gd", "Ex")
KitchenQual = c("Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")
Fence = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv")
BsmtExposure = c("None", "No", "Mn", "Av", "Gd")
GarageFinish = c("None", "Unf", "RFn", "Fin")
LandSlope = c("Sev", "Mod", "Gtl")
LotShape = c("IR3", "IR2", "IR1", "Reg")
PavedDrive = c("N", "P", "Y")
Street = c("Pave", "Grvl")
Alley = c("None", "Pave", "Grvl")
MSSubClass = c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", 
               "90", "120", "150", "160", "180", "190")
OverallCond = NA
MoSold = NA
YrSold = NA
CentralAir = NA
levels = list(FireplaceQu, BsmtQual, BsmtCond, GarageQual, GarageCond, 
              ExterQual, ExterCond, HeatingQC, PoolQC, KitchenQual, BsmtFinType1, 
              BsmtFinType2, Functional, Fence, BsmtExposure, GarageFinish, LandSlope, 
              LotShape, PavedDrive, Street, Alley, CentralAir, MSSubClass, OverallCond, 
              YrSold, MoSold)
i = 1
for (c in cols) {
  if (c == "CentralAir" | c == "OverallCond" | c == "YrSold" | c == "MoSold") {
    combine[, c] = as.numeric(factor(combine[, c]))
  } else combine[, c] = as.numeric(factor(combine[, c], levels = levels[[i]]))
  i = i + 1
}
#Adding an important feature - Total area of basement
combine$TotalSF <- combine$TotalBsmtSF + combine$X1stFlrSF + combine$X2ndFlrSF
head(combine)
#Getting dummy categorical features
# first get data type for each feature
feature_classes <- sapply(names(combine), function(x) {
  class(combine[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical
# features
library(caret)
dummies <- dummyVars(~., combine[categorical_feats])
categorical_1_hot <- predict(dummies, combine[categorical_feats])
#Fixing Skewed features
#transform the skewed features with BoxCox Transformation.

## Determine skew for each numeric feature
library(moments)
library(MASS)
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(combine[[x]], na.rm = TRUE)
})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(combine[[x]], lambda = 0.15)
  combine[[x]] = predict(bc, combine[[x]])
  # combine[[x]] <- log(combine[[x]] + 1)
}
#Reconstruct all data with pre-processed data.
combine <- cbind(combine[numeric_feats], categorical_1_hot)
training <- combine[1:1458, ]
testing <- combine[1459:2917, ]
head(training)
summary(training)
str(training)

### Random Forest ###
library(ggplot2)
library(randomForest)
library(gmodels)
set.seed(2018)

# Quick random forest to subset the variables
quick_RF <- randomForest(x=combine[1:1460,-59], y=combine$SalePrice[1:1460], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

# Exploritory plot for variables to use
ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


### kNN ###

# # Create classes of house prices
# quantile(train$SalePrice, c(.33, .67))
# train$PriceLvl <- 0
# for(i in 1:length(train$SalePrice)){
#   if(train$SalePrice[i] > 12.16){
#     train$PriceLvl[i] <- "High"
#   }
#   else if(train$SalePrice[i] < 11.842){
#     train$PriceLvl[i] <- "Low"
#   }
#   else{
#     train$PriceLvl[i] <- "Mid"
#   }
# }
# 
# ggplot(train, aes(x = GrLivArea, y = SalePrice, col = PriceLvl)) +
#   geom_point()

# Create new test/train sets
gp <- runif(nrow(train))
# Use gp to create the training set (75% of data) 
# and test (25% of data)
test <- train[gp >= 0.75,]
train <- train[gp < .75,]


### 
knn_fit <- train(SalePrice~ GrLivArea, data=train, method = "knn", trControl = trainControl("cv", number = 5), tuneLength = 30)

# See how accuracy changes as k varies
plot(knn_fit, main="Model Accuracy with Varying k in kNN")

# The best choice for k that maximizes model accuracy
best_k <- knn_fit$bestTune
best_k

knn_pred <- predict(knn_fit, test)
#RMSE
mean((test$SalePrice - knn_pred)^2)

setwd("~/Documents/University/UVa/Semester_1/Business Analytics (4220)/Final")
library(MASS)
library(psych)

trainingdata<-read.csv("train.csv", stringsAsFactors = FALSE)

library(ggplot2)

qplot(trainingdata$GrLivArea, trainingdata$SalePrice, main = "With Outliers")
trainingdata <- trainingdata[-which(trainingdata$GrLivArea > 4000 & trainingdata$SalePrice < 3e+05), ]
## Check again after removal.
qplot(trainingdata$GrLivArea, trainingdata$SalePrice, main = "Without Outliers")
## Plot histogram of SalePrice Variable - Right skewed
qplot(SalePrice, data = trainingdata, bins = 50, main = "Right skewed distribution")
## Log transformation of the target variable
trainingdata$SalePrice <- log(trainingdata$SalePrice + 1)

## Normal distribution after transformation
qplot(SalePrice, data = trainingdata, bins = 50, main = "Normal distribution after log transformation")

#check for missing data
colSums(is.na(trainingdata))
#For most of the categorical features, NA values will be imputed as ‘None’, because referring to the data_description.txt #file, the NA of these variables represent values such as ‘No Garage’,‘No Basement’, etc.
#For most of the numerical features, NA values will be replaced by 0, for variables like GarageArea, GarageCars, etc.
#For some categorical features like Functional and Electrical, the NA values will be replaced by the most frequently occuring value for that variable.

## For some variables, fill NA with 'None'
for (x in c("PoolQC", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "BsmtQual", 
            "BsmtExposure", "BsmtFinType2")) {
  trainingdata[is.na(trainingdata[, x]), x] = "None"
}

## Replacing missing data with 0
for (col in c("GarageArea", "BsmtFinSF1", 
              "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath" 
)) {
  trainingdata[is.na(trainingdata[, col]), col] = 0
}

## Replace missing MSZoning values by 'RL'
trainingdata$MSZoning[is.na(trainingdata$MSZoning)] = "RL"

## Replace missing KitchenQual values by 'TA'
trainingdata$KitchenQual[is.na(trainingdata$KitchenQual)] = "TA"

## Replace missing SaleType values by 'WD'
trainingdata$SaleType[is.na(trainingdata$SaleType)] = "WD"

## All NAs should be gone, except the test portion of SalePrice
## variable, which we ourselves had initialized to NA earlier.
colSums(is.na(trainingdata))

#Transforming some numerical variables that are really categorical
trainingdata$OverallCond = as.character(trainingdata$OverallCond)
#Label Encoding some categorical variables that may contain information in their ordering set
#specify the order of the levels, converting categories to integer ranks - 1 to n, the categorical variables.

cols = c("FireplaceQu", "BsmtQual", "GarageQual", 
         "ExterQual", "ExterCond", "HeatingQC", "PoolQC", "KitchenQual", "BsmtFinType1", 
         "BsmtFinType2", "Fence", "BsmtExposure", "GarageFinish", 
         "PavedDrive", "Street", "Alley",
         "OverallCond")

FireplaceQu = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
ExterQual = c("Po", "Fa", "TA", "Gd", "Ex")
ExterCond = c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC = c("Po", "Fa", "TA", "Gd", "Ex")
PoolQC = c("None", "Fa", "TA", "Gd", "Ex")
KitchenQual = c("Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
Fence = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv")
BsmtExposure = c("None", "No", "Mn", "Av", "Gd")
GarageFinish = c("None", "Unf", "RFn", "Fin")
PavedDrive = c("N", "P", "Y")
Street = c("Pave", "Grvl")
Alley = c("None", "Pave", "Grvl")

OverallCond = NA
levelsi = list(FireplaceQu, BsmtQual, GarageQual,
              ExterQual, ExterCond, HeatingQC, PoolQC, KitchenQual, BsmtFinType1, 
              BsmtFinType2, Fence, BsmtExposure, GarageFinish, 
              PavedDrive, Street, Alley, OverallCond)
# i = 1
# for (c in cols) {
#   if (c == "CentralAir" | c == "OverallCond") {
#     trainingdata[, c] = as.numeric(factor(trainingdata[, c]))
#   } else trainingdata[, c] = as.numeric(factor(trainingdata[, c], levels = levelsi[[i]]))
#   i = i + 1
# }
#Adding an important feature - Total area
trainingdata$TotalSF <- trainingdata$TotalBsmtSF + trainingdata$X1stFlrSF + trainingdata$X2ndFlrSF
head(trainingdata)
#Getting dummy categorical features
# first get data type for each feature
feature_classes <- sapply(names(trainingdata), function(x) {
  class(trainingdata[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical
# features
library(caret)
dummies <- dummyVars(~., trainingdata[categorical_feats])
categorical_1_hot <- predict(dummies, trainingdata[categorical_feats])
#Fixing Skewed features
#transform the skewed features with BoxCox Transformation.

## Determine skew for each numeric feature
library(moments)
library(MASS)
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(trainingdata[[x]], na.rm = TRUE)
})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(trainingdata[[x]], lambda = 0.15)
  trainingdata[[x]] = predict(bc, trainingdata[[x]])
  # trainingdata[[x]] <- log(trainingdata[[x]] + 1)
}
#Reconstruct all data with pre-processed data.
trainingdata <- cbind(trainingdata[numeric_feats], categorical_1_hot)
trainingdata <- trainingdata[1:1458, ]
testing <- trainingdata[1459:2917, ]
head(trainingdata)
summary(trainingdata)
str(trainingdata)


#create test and training set because test set is not useful bc no sales price
inTrain <- createDataPartition(y = trainingdata$SalePrice, p = 0.7, list = FALSE)
Training1 <- trainingdata[inTrain, ]
Training1 <- Training1[-973,]
Validation <- trainingdata[-inTrain, ]
head(Training1)

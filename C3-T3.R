
setwd("C:/Users/user/Google Drive/WD")
getwd()

library(dplyr)
library(caret)


trainingData <- read.csv("C:/Users/user/Google Drive/WD/UJIndoorLoc/UJIndoorLoc/trainingData.csv", header = T, sep = ",")

anyNA(trainingData)
#if there are NAs
#sum(is.na(trainingData))
glimpse(trainingData)
summary(trainingData)
dim(trainingData)

# sample
# individual building: Building # 0
building_0 <- trainingData %>% filter(BUILDINGID == 0) 

# identifies zero-variance attributes
var(building_0) 

# remove zero-variance attributes 
nzv <- nearZeroVar(building_0, names = T, saveMetrics = T)
zv <- nzv[nzv$zeroVar, ]
zerovar <- rownames(zv)
all_cols <- names(building_0)
building_0 <- building_0[, setdiff(all_cols, zerovar)]
dim(building_0) 
dim(trainingData)

# add LOCATION attribute
building_0$LOCATION <- paste(building_0$FLOOR, building_0$SPACEID)
str(building_0$LOCATION)
dim(building_0)
glimpse(building_0[, 200:209])

# attribute as factor - dependent variable
building_0$LOCATION <- as.factor(building_0$LOCATION)
glimpse(building_0[, 200:209])

# removing some attributes
building_0$TIMESTAMP <- NULL
building_0$USERID <- NULL
building_0$PHONEID <- NULL
building_0$SPACEID <- NULL
building_0$FLOOR <- NULL
building_0$RELATIVEPOSITION <- NULL
building_0$LATITUDE <- NULL
building_0$LONGITUDE <- NULL

dim(building_0)
glimpse(building_0[, 195:201])

#data slicing
set.seed(123)
TrainSize <- createDataPartition(y = building_0$LOCATION, p = .75, list = FALSE)
training <- building_0[TrainSize,] 
testing <- building_0[- TrainSize,] 
dim(training)
dim(testing)

#10 fold cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

#parameters for particular model: 
modelLookup("C5.0") 
modelLookup("rf")
modelLookup("knn")
modelLookup("naive_bayes")

#Random Forest - 76%
rf <- train(LOCATION ~., data = training, method = "rf",  trControl = ctrl)
rf
summary(rf)

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 3590, 3591, 3605, 3606, 3593, 3593, ... 
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa    
# 2     0.2065677  0.2015997
# 101   0.7583312  0.7572811
# 200   0.7464821  0.7453819

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 101.


pred_rf <- predict(rf, newdata = testing)
pred_rf
summary(pred_rf)

conf_rf <- confusionMatrix(pred_rf, testing$LOCATION)
conf_rf
write.csv(conf_rf$table, file = "RF Confusion Matrix.csv", row.names = TRUE)
write.csv(conf_rf$byClass, file = "Performance Measures.csv", row.names = TRUE)

model_comp <- resamples(list(RF = rf, NB = nb, KNN = knn, C5.0 = c5.0))
summary(model_comp)


#C5.0 
# first try: indicated overfitting 
# second try: only WAPs & LOCATION variables left, rest of the variables removed
# result - 72 % of accuracy 
c5.0 <- train(LOCATION ~., data = training, method = "C5.0",  trControl = ctrl)
c5.0
summary(c5.0)

#model  winnow  trials  Accuracy   Kappa    
#rules  FALSE   20      0.7037136  0.7024280
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were trials = 20, model = rules and winnow = FALSE.

#Naive Bayes - 42% 
nb <- train(LOCATION ~., data = training, method = "naive_bayes",  trControl = ctrl)
nb
summary(nb)
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 3602, 3585, 3590, 3597, 3596, 3592, ... 
#Resampling results across tuning parameters:
  
#usekernel  Accuracy   Kappa    
#FALSE      0.1194467  0.1157938
#TRUE       0.4201979  0.4173122

#Tuning parameter 'fL' was held constant at a value of 0
#Tuning parameter 'adjust' was held constant at a value of 1
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.


# KNN - 55 %  (slightly better result was before removing variables)
#normalization (values between 0 and 1) is traditionally used for knn
#z-score standarization may improve predictive accuracy 
knn <- train(LOCATION ~., data = training, method = "knn",  trControl = ctrl, tuneLength = 15)  
knn
summary(knn)
#preProc = "range" (normalization) not appropriate (no variation for WAP...)
#preProc = "center" (standarization) works?

#k   Accuracy   Kappa    
#5  0.5463773  0.5444368
#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was k = 5.


save.image()

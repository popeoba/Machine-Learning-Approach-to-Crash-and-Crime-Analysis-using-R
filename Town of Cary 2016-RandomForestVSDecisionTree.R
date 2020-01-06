# Install random forest package
install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
fulldata <- read.csv(file.choose(), header = TRUE, na.strings=c(""))
data1 <- subset(fulldata,select=c(7,9,10,11,38))
head(data1)
str(data1)
summary(data1)

# Remove rows with NAs
data1 <- data1[!is.na(data1$weather),]
rownames(data1) <- NULL
data1 <- data1[!is.na(data1$rdsurface),]
rownames(data1) <- NULL
data1 <- data1[!is.na(data1$lightcond),]
rownames(data1) <- NULL
data1 <- data1[!is.na(data1$trafcontrl),]
rownames(data1) <- NULL
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

# Create a Random Forest model with default parameters
model1 <- randomForest(fatalities ~ ., data = TrainSet, importance = TRUE)
model1

# Fine tuning parameters of Random Forest model
model2 <- randomForest(fatalities ~ ., data = TrainSet, ntree = 1000, mtry = 4, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$fatalities) 

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$fatalities)                    
table(predValid,ValidSet$fatalities)

# To check important variables
importance(model2)        
varImpPlot(model2)    

# Using For loop to identify the right mtry for model
a=c()
i=4
for (i in 2:4) {
  model3 <- randomForest(fatalities ~ ., data = TrainSet, ntree = 1000, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-1] = mean(predValid == ValidSet$fatalities)
}

a

plot(2:4,a)


# Compare with Decision Tree
install.packages("rpart")
install.packages("caret")
install.packages("e1071")

library(rpart)
library(caret)
library(e1071)

# We will compare model 1 of Random Forest with Decision Tree model
model_dt = train(fatalities ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$fatalities)
mean(model_dt_1 == TrainSet$fatalities)

# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$fatalities)
mean(model_dt_vs == ValidSet$fatalities)








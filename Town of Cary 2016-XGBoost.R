# Libraries
library(caret)
library(pROC)
library(mlbench)
library(Amelia)
library(dplyr)

# Load Dataset
PoliceIncdata <- read.csv(file.choose(), header = T, na.strings=c(""))

# Subsetting the data
data <- subset(PoliceIncdata,select=c(29,1,11,14,35))

# Output the number of missing values for each column
sapply(data,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(data, function(x) length(unique(x)))

# A visual check for missing data
missmap(data, main = "Missing values vs observed")

# Remove rows with NAs
data <- data[!is.na(data$district),]
rownames(data) <- NULL
data <- data[!is.na(data$domestic),]
rownames(data) <- NULL
data <- data[!is.na(data$crimeday),]
rownames(data) <- NULL

# A visual  re-check for missing data
missmap(data, main = "Missing values vs observed")
str(data)
xtabs(~phxcommunity+crime_category, data = data)
xtabs(~phxcommunity+crimeday, data = data)
xtabs(~phxcommunity+district, data = data)
xtabs(~phxcommunity+domestic, data = data)

#Remove factor levels without adequate responses to the dependent variable
remove.list <- paste(c("MPD1", "WAKE"), collapse = '|')
data <- data %>% filter(!grepl(remove.list, district))

data$crime_category <- as.integer(as.factor(data$crime_category))
data$crimeday <- as.integer(as.factor(data$crimeday))
data$district <- as.integer(as.factor(data$district))
data$domestic <- as.integer(as.factor(data$domestic))
data$phxcommunity <- as.integer(as.factor(data$phxcommunity))

str(data)
summary(data)

data$phxcommunity[data$phxcommunity == 1] <- 0
data$phxcommunity[data$phxcommunity == 2] <- 1
#data$phxcommunity[data$phxcommunity == 0] <- 'No'
#data$phxcommunity[data$phxcommunity == 1] <- 'Yes'
data$phxcommunity <- factor(data$phxcommunity)

write.csv(data,'poldata3.csv')
head(data)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) }
data.subset <- as.data.frame(lapply(data[,2:5], normalize))
head(data.subset)

set.seed(123)
dat.d <- sample(1:nrow(data.subset), size=nrow(data.subset)*0.7, replace =FALSE)
train <- data[dat.d,]
test <- data[-dat.d,]

trainphx <- data[dat.d,1]
testphx <- data[-dat.d,1]

install.packages('class')
library(class)

NROW(trainphx)

knn.1 <- knn(train=train, test=test, cl=trainphx, k=9)


# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind == 1,]
test <- data[ind == 2,]

# KNN Model
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)
fit <- train(phxcommunity ~.,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"))

# Model Performance
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$admit)

# Example-2 Boston Housing (Regression)
data("BostonHousing")
data <- BostonHousing 
str(data)

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind == 1,]
test <- data[ind == 2,]

# KNN Model
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)
set.seed(333)
fit <- train(medv ~.,
             data = training,
             tuneGrid = expand.grid(k=1:70),
             method = 'knn',
             metric = 'Rsquared',
             trControl = trControl,
             preProc = c('center', 'scale'))

# Model Performance
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test)
RMSE(pred, test$medv)
plot(pred ~ test$medv)

#Load needed libraries. Install and load, if library is not installed
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(Amelia)

# Load Dataset
PoliceIncdata <- read.csv(file.choose(), header = T, na.strings=c(""))

#Create a subset with only needed attributes
data <- subset(PoliceIncdata,select=c(1,11,14,29,35))

# Check for missing values in each column
sapply(data,function(x) sum(is.na(x)))

# Check for the number of different values in each column
sapply(data, function(x) length(unique(x)))

# A visual check for missing data
missmap(data, main = "Missing values vs observed")

# Remove rows with NAs
data <- data[!is.na(data$district),]
rownames(data) <- NULL
data <- data[!is.na(data$domestic),]
rownames(data) <- NULL

# A visual  re-check for missing data
missmap(data, main = "Check for missing values")
str(data)

#Check for factor levels without adequate responses to the dependent variable
xtabs(~domestic+crime_category, data = data)
xtabs(~domestic+crimeday, data = data)
xtabs(~domestic+district, data = data)
xtabs(~domestic+phxcommunity, data = data)

#Remove factor levels without adequate responses to the dependent variable
remove.list <- paste(c("MPD1", "WAKE"), collapse = '|')
data <- data %>% filter(!grepl(remove.list, district))

# Partition data into training set and test set
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Running Naive Bayes Model
model <- naive_bayes(domestic ~ ., data = train, laplace=1, usekernel = T)
model

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix for training data
p1 <- predict(model, train)
(tab1 <- table(p1, train$domestic))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix for test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$domestic))
1 - sum(diag(tab2)) / sum(tab2)

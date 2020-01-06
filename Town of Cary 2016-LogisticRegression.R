#Load needed libraries. Install and load, if library is not installed
library(naniar)
library(dplyr)
library(Amelia)

# Load Dataset
PoliceIncdata <- read.csv(file.choose(), header = T, na.strings=c(""))

# Create a subset with only needed attributes
data <- subset(PoliceIncdata,select=c(1,11,14,29,35))

# Check for missing values in each column
sapply(data,function(x) sum(is.na(x)))

# Check for the number of different values in each column
sapply(data, function(x) length(unique(x)))

# A visual check for missing data
graphics.off()
missmap(data, main = "Check for missing values")

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
train <- data[ind==1,]
test <- data[ind==2,]
#Logistic Regression Model
mymodel <- glm(domestic ~ crime_category + crimeday + district + phxcommunity , data = train, family = 'binomial')
summary(mymodel)

#Prediction
p1 <- predict(mymodel, train, type ='response')
head(p1)
head(train)
#Misclassification error for training data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$domestic)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error for test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$domestic)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))


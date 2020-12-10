### In this regression task we will predict the percentage of marks that a student is expected to score based upon the number of hours they studied. This is a simple linear regression task as it involves just two variables.

## Firstly load the Dataset, but we have a url of dataset so we add (data.table) pakage for url data load....

library(data.table)
dataload <- fread("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")


#### library(data.table)
#### dataload <- fread("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")

# Check the Dataset

head(dataload)
summary(dataload)


## Including Plots

# 1. Plot (Scatter Plot)


library(ggplot2)
ggplot(dataload, aes(x = dataload$Hours, y = dataload$Scores)) +
  geom_point() + geom_line() + geom_smooth()


## In this graph, it clearly shows Students who are giving minimum time(hours) their marks is less and who is giving more time(hours) their marks are more. Let's see more graph to clear more..

# 2. Plot (Box Plot)


# box plot for hours
#par(mfrow=c(1,2)) # graph divide in two parts
boxplot(dataload$Hours, main = "Hours", sub = paste("Outlier :", boxplot.stats(dataload$Hours)$out)) # if outlier..
boxplot(dataload$Scores, main = "Scores", sub = paste("Outlier :", boxplot.stats(dataload$Scores)$out)) # if outlier.


## So as we seen there isn't any outlier in these two graph. Now we can see relationship between these two columns Hours and Scores..

# Find correlation cor()

correlation_value <- cor(x = dataload$Hours, y = dataload$Scores) # cor() used for find correlation value between two columns
correlation_value


## Correlation value between two columns is more then 0. This means the vaslue is highly +ve..

## Now we have to see the relationship graphically and numerically. Let us build the linear modal..
# Creating Training Data and Test data..

set.seed(1000) #set seed for random sampling
trainingRowIndex <- sample(1:nrow(dataload), 0.8*nrow(dataload)) #row indices for training data
trainingRowIndex
trainingData <- dataload[trainingRowIndex, ] # model training data
testData <- dataload[-trainingRowIndex, ] #test data


# Build the model on training data

lmMod <- lm(Scores ~ Hours, data = trainingData) #build modal
predictdata <- predict(lmMod, testData) #predict 
summary(lmMod)


# Actual prediction

actual_prediction <- data.frame(cbind(actual = testData$Scores, prediction = predictdata))
correlation_accuracy <- cor(actual_prediction)
head(actual_prediction)


# Min-Max accuracy

min_max_accuracy <- mean(apply(actual_prediction, 1, min) / apply(actual_prediction, 1, max))
min_max_accuracy


## min-max accuracy is 89.29

# Mape calculation

mape <- mean(abs(actual_prediction$prediction - actual_prediction$actual) /  actual_prediction$actual)
mape


## 12.60 is mean absolute percentage error or deviation (MAPE or MAPD)

## So, here we can see measure of accuracy is 12.60% (low) and mean absolute percantage error or deviation is 89.29% (very high). This prediction is enough to show our data is linear (Students gives more time to study they get more marks)

# prediction for checking if student study for 9.25 hrs/day ?

hours <- data.frame(Hours = 9.25)
predict(lmMod , hours)


# According to this prediction if Students study for 9.25hrs/day they might get 93.21..




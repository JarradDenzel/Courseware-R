# k-Nearest Neighbor Algorithm
#   KNN -> non-parametric method used for classification and regression
#  Given a point on a coordinate plane, among other points
#  ...what is the point question closest to in terms of the nearest points
#   ...using an odd number, k, what are the nearest neighbors to the point?
#     ...find the distance amongst the points
#       ...standardize/normalize the variables, e.g. z-score, min-max
# z-score = from each value, subtract mean and divide by standard deviation
# min-max = from each value, subract maximum value and divide by (max - min)
# Applications: Recommendation, Anamoly Detection, Text Categorization, Finance
 
# Libraries
library(caret)
library(pROC)
library(mlbench)

# kNN - Classification
data <- read.csv(file.choose(), header=T)
str(data)
 
# Change the type of admit to that of a factor varible
data$admit[data$admit == 0] <- 'No'
data$admit[data$admit == 1] <- 'Yes'
data$admit <- factor(data$admit)

# Partition Data
set.seed(1234)
partedData <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
trainer <- data[partedData == 1,]
tester <- data[partedData == 2,]

# KNN Model
# Specify a Training Control, using repeated cross-validation method from caret
# ... 10 iterations, repeated 3 times, 3 indicates number of folds
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)
# fit predictive model over tune parameters
fit <- train(admit ~ .,
             data = trainer,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"))

# Model Performance
fit
# Plot Number of Neighbors vs. Accuracy
plot(fit)
# Variable importance
varImp(fit)
pred <- predict(fit, newdata = tester)
confusionMatrix(pred, tester$admit)

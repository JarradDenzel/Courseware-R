
# Read in the file of student data
data <- read.csv("binary.csv", header=TRUE)

# Min - Max Normalization
data$gre <- (data$gre - min(data$gre)) / (max(data$gre) - min(data$gre))
data$gpa <- (data$gpa - min(data$gpa)) / (max(data$gpa) - min(data$gpa))
data$rank <- (data$rank - min(data$rank)) / (max(data$rank) - min(data$rank))

# Data Partition
  #
set.seed(222)

  # From the Data, take sample of size 2
ind <- sample(2, nrow(data), replace = TRUE,  prob = c(0.7, 0.3))

  # Within the ind, the first sample of 2 
  # Use the training set to build neural network
training <- data[ind==1,]

  # Within the ind, the second sample of 2
  # Use the test set to test the resultant network
testing <- data[ind==2,]

#Neural Network

library(neuralnet)
set.seed(333)

  # Create of model the predicts admit as a funtion of gre,gpa,rank 
  # with one hidden layer
n <- neuralnet(admit~gre+gpa+rank, 
               data = training, 
               hidden = 1, 
               err.fct = "ce",
               linear.output = FALSE)

  # Produce visualization of Neural Network
plot(n)

# Prediction
  # Compute on all rows and col, minus the first col, b/c thats the admit column
  # Store computation in a variable, "output"
output <- compute(n, training[,-1])

# Confusion Matrix / Misclassification Error Matrix - training data
output <- compute(n, training[,-1])

  # Probabilities of the of output on the training data
probabilities1 <- output$net.result
  
  # Predictions: if probabilities are greater than 50%, classify as 'admitted'
  # if probabilities are less than 50% classify as 'not admitted'
prediction1 <- ifelse(prediction1>0.5,1,0)
table1 <- table(prediction1, training$admit)
table1

  # To calculate miscalculation error: sum diagonal, divide by sum of tab
  # ...subtract from one
1-sum(diag(table1))/sum(table1)








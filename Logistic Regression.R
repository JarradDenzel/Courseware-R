# Logistic Regression

# Student Application: Classfiy apps as admit or reject
# Target/Dependent Variable: Admit (0-no, 1-yes) *not continuous, but categorical
# Predictors/Independent Variables: GRE, GPA, RANK

# Read data file
mydata <- read.csv(file.choose(), header = T)

# Convert admit and rank cols to factor variables (categorical variables)
mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)

# Two-way table of factor variables
  # ensure there are no cells without values
xtabs(~admit + rank, data = mydata)

# Partition data - training set (80%) & test set (20%)
set.seed(1234)

  # Create 2 Independent Sets of data, 80/20, training & test respectively
ind <- sample(2, nrow(mydata), replace = T, prob = (c(0.8, 0.2)))
trainer <-  mydata[ind==1,]
tester <- mydata[ind==2,]

# Logistic Regression Model, using generalized linear model
  # family = binomial because admit, the target, only takes two values, 0 & 1
  # gre is not statistically significant, so it can be discarded for now
mymodel <- glm (admit ~ gpa + rank, data = trainer, family = 'binomial')

# Prediction
p1 <- predict(mymodel, trainer, type = 'response')

# ln(p/1-p) = b0 + b1x1 + b2x2 ... + bnxn = y
#   p/1-p => "odds"
#   p = probability of acceptance
#   1 - p = probability of rejection
#     ln(p/1-p) = y 
#     p/1-p = e^y
#     1/p = 1 + (1/e^y)
#     p = (e^y/1+e^y)

# Misclassification Error  - trainer
#   if probability of admission is more than 50%, admit, else, reject
pred1 <- ifelse(p1 > 0.5, 1, 0)

#   Create a table, with prediction vs admit col
tab1 <- table(Predicted = pred1, Actual = trainer$admit)

#   Create a Confusion Matrix 
#   ... the diagonal topleft to bottomright, show where the model and reality agree
#   ... the diagonal inverse, show where reality and the model disagree ...the misclassification

tab1

#   Calculate the Misclassification error: 1 minus the diagonal sum / whole sum
1 - sum(diag(tab1))/sum(tab1)

#   Misclassification error - test data
p2 <- predict(mymodel, tester, type = 'response')
pred2 <- ifelse(p2 > 0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = tester$admit)

#   Confusion Matrix for tester data
tab2

# Misclassification Error of tester data set
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-Fit Test
#   calculate p-value using chi-squared, distribution function

with(mymodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))




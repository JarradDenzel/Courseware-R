# Decision Tree
# Cardiotocographic data: 21 Features
# Response Variable: N=normal, S=suspect, P=pathologic

# Load Data
data <- Cardiotocographic

# Convert NSP col into a factor variable rather than an integer variable 
#   Create a new col NSPF that's the factor version of the OG NSP
data$NSPF <- factor(data$NSP)

# Partition data into 2 sets: Trainer(80%) & Tester(20%)
set.seed(1234)
partedData <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
trainer <- data[partedData==1,]
tester <- data[partedData==2,]

# Decision Tree with party package, the Decision Tree will be called 'tree'
# Simplify results by setting confidence to .99 with a min sample of 500
library(party)
tree <- ctree(NSPF ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV , data = trainer, 
              controls = ctree_control(mincriterion=0.99,minsplit = 500))
tree
plot(tree)

# Predict the probability of Normal, Suspect, and Pathologic
predict(tree, tester, type="prob")
  
predict(tree,validate)
  
# Decision Tree with rpart package
library(rpart)
tree1 <- rpart(NSPF ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV, trainer)
library(rpart.plot)
rpart.plot(tree1,extra=4)

# Prediction
predict(tree1,tester)

# Misclassification Error of trainer data set 
tab <- table(predict(tree), trainer$NSPF)
print(tab)
1 - sum(diag(tab))/sum(tab)
  
  
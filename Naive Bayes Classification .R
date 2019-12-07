#Naive Bayes Classification

#Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Load Data
data <- read.csv(file.choose())
str(data)

# Convert the rank and admit col to factor varialbles
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

# Visualization
pairs(data[-1])
data %>%
  ggplot(aes(x=admit, y=gpa, fill=admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")

# Partition Data
set.seed(1234)
partedData <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
trainer <- data[partedData==1,]
tester <- data[partedData==2,]

# Naive Bayes Model
#   Bayes Theorem: P(A|B) = (P(A) * P(A|B))/(P(B))
#   Applied: P(Admit=1 | Rank=1) ...

model <- naive_bayes(admit ~ ., data = trainer, usekernel = T)
model

# Predict
p <- predict(model, trainer, type = 'prob')
head(cbind(p, trainer))

# Confusion Matrix
p1 <- predict(model, trainer)
(tab1 <- table(p1, trainer$admit))
1 - sum(diag(tab))/sum(tab)

p2 <- predict(model, tester)
(tab2 <- table(p2, tester$admit))
1 - sum(diag(tab2))/sum(tab2)





# Vehicle.csv

# Take a peek at the first few lines of data
head(vehicle)

# Produce a matrix of scatterplots
# From the vehicle matrix, make a scatterplot from cols 3, 4 & 5
pairs(vehicle[3:5])

# lc and lh are strongly related, more so than the mileage pairs


# Multiple Linear Regression  
  # Assign the fitting of the linear model to the variable, results
  # lm = used to fit linear models, single stratum analysis of variance 
  # ... and analysis of covariance 
  # A typical model has the form response ~ terms where response is the (numeric) response vector and 
  # terms is a series of terms which specifies a linear predictor for response

results <- lm(lc~lh, vehicle)
  # "results" will print the linear equation coefficients
results
  # the summary will print the probability p-values and show 
  # the significance of the variables in the function, 
  # e.g. *** = Very Significant & no stars = not statistically significance
summary(results)

reduced <- lm(lc~lh,vehicle)
full <- lm(lc~lh+Mileage, vehicle)

# ANOVA = analysis of variance, 
# ... produces analysis-of-variance and analysis-of-deviance tables.

anova(reduced,full)

# Prediction (default = 95% confidence)
# "predict from results, the lc given lh = 10 with a 95% confidence level"
predict(results, data.frame(lh=10), interval = 'confidence')



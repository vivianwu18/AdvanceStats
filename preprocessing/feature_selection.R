library(ggplot2)
library(performance)

### Feature selection methods
# load the data
Boston <- Boston
pairs(Boston)
ggpairs(Boston)

# descriptive Statistic of the dataset.
describe(Boston)

# fit the multiple linear regression
model3 <- lm(crim ~ indus + nox + dis + rad + tax + lstat + medv, data = Boston)
summary(model3)

model3_adjusted <- lm(crim ~ indus + dis + rad + lstat + medv, data = Boston)
summary(model3_adjusted)

### Feature selection
# split the data into training set and testing set
smp_size <- floor(0.8 * nrow(Boston))
set.seed(1)
train_ind <- sample(seq_len(nrow(Boston)), size = smp_size)

Boston_train <- Boston[train_ind, ]
Boston_test <- Boston[-train_ind, ]

intercept_only <- lm(crim ~ 1, data = Boston_train)
full <- lm(crim ~ ., data = Boston_train)

# forward stepwise with p-value threshold of 0.1
forward1 <- ols_step_forward_p(full, penter = 0.1, details = TRUE)
forward1$model
forward11 <- lm(crim ~ rad+lstat, data = Boston_test)

# backward stepwise with p-value threshold of 0.1
backward1 <- ols_step_backward_p(full, penter = 0.1, details = TRUE)
backward1$model
backward11 <- lm(crim ~ zn + indus + nox + dis + rad + ptratio + lstat + medv, data = Boston_test)

# forward stepwise with AIC
forward2 <- step(intercept_only, direction = "forward", scope = formula(full), trace = 0, k = 2)
forward2$coefficients
forward22 <- lm(crim ~ rad+lstat+medv+ptratio, data = Boston_test)

# forward stepwise with BIC
forward3 <- step(intercept_only, direction = "forward", scope = formula(full), trace = 0, k = log(nrow(Boston_train)))
forward3$coefficients
forward33 <- lm(crim ~ rad+lstat, data = Boston_test)

# forward stepwise with Mallows Cp 
# Fit a linear regression model with all predictor variables
full_model <- lm(crim ~. , data = Boston_test)

# initialize an empty list to store the results
CP_results <- 0
selected_features <- c()
remaining_features <- names(Boston[2:10])
cp_diff <- 1000

# perform the Forward Stepwise method
while (length(remaining_features) > 0) {
  temp_best_feature <- ''
  temp_cp <- 0
  for (feature in remaining_features) {
    current_features <- c(selected_features, feature)
    model <- lm(crim ~. , data = Boston[,c(current_features , "crim")])
    cp <- ols_mallows_cp(model , full_model)
    cp_df_diff <- abs(cp - length(current_features))
    if (cp_df_diff < cp_diff) {
      temp_best_feature <- feature
      cp_diff <- cp_df_diff
      temp_cp <- cp
    }
  }
  if (temp_cp == 0) {
    break
  }else{
    selected_features <- c(selected_features, temp_best_feature)
    remaining_features <- remaining_features[remaining_features != temp_best_feature]
    CP_results <- temp_cp
  }
}

cat("Actual Mallows' CP : " , CP_results , "\n")
cat("Difference between Mallows' CP and number of variables : " , cp_diff , "\n")
cat("Features selected : " , selected_features)

forward4 <- lm(crim~ rad+rm+dis+zn+chas, data = Boston_test)

# compare the performance
compare_performance(forward11, backward11, forward22, forward33, forward4, rank = TRUE)


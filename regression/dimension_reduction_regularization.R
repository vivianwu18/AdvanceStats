library(pls)
library(caret)
library(glmnet)

### Dimension Reduction vs Regularization
## split the data
boston <- ISLR2::Boston

set.seed(1)
boston_index <- sample(nrow(boston), nrow(boston) * 0.8)
boston_train <- boston[boston_index, ]
boston_test <- boston[-boston_index, ]


## PCR model
set.seed(1)

# train the pcr model
pcr_model <- pcr(crim ~ ., data = boston_train, scale = TRUE, validation = "CV")
summary(pcr_model)

# visualize cross-validation plot
validationplot(pcr_model, val.type = "RMSEP")
validationplot(pcr_model, val.type = "MSEP")

# use the optimal M to predict and calculate RMSE
pcr_pred <- predict(pcr_model, boston_test[-1], ncomp = 12)
pcr_error <- RMSE(pcr_pred, boston_test[, 1])

cat("The RMSE is", pcr_error, "when the value of pricipal component is 12.")


## PLS model
set.seed(1)

# train the pls model
pls_model <- plsr(crim ~ ., data = boston_train, scale = TRUE, validation = "CV")
summary(pls_model)

# visualize cross-validation plot
validationplot(pls_model, val.type = "RMSEP")
validationplot(pls_model, val.type = "MSEP")

# use the optimal M to predict and calculate RMSE
pls_pred <- predict(pls_model, boston_test[-1], ncomp = 9)
pls_error <- RMSE(pls_pred, boston_test[, 1])

cat("The RMSE is", pls_error, "when the value of pricipal component is 10.")


## Ridge Regression
boston_y <- boston_train$crim
boston_x <- data.matrix(boston_train[, -1])

# train the ridge regression model
ridge_model <- glmnet(boston_x, boston_y, alpha = 0, standardize = TRUE)
summary(ridge_model)

# perform k-fold cross-validation
cv_ridge_model <- cv.glmnet(boston_x, boston_y, alpha = 0, standardize = TRUE)
best_ridge_lambda <- cv_ridge_model$lambda.min
best_ridge_lambda

plot(cv_ridge_model)

# use the optimal lambda to perform the best ridge regression model
best_ridge_model <- glmnet(boston_x, boston_y, alpha = 0, lambda = best_ridge_lambda, standardize = TRUE)
ridge_pred <- predict(ridge_model, s = best_ridge_lambda, newx = data.matrix(boston_test[-1]))
ridge_error <- RMSE(ridge_pred, boston_test[, 1])

cat("The RMSE is", ridge_error, "when the value of lambda is", best_ridge_lambda)


## Lasso Regression
set.seed(1)
boston_y <- boston_train$crim
boston_x <- data.matrix(boston_train[, -1])

# train the lasso regression model
lasso_model <- glmnet(boston_x, boston_y, alpha = 1, standardize = TRUE)
summary(lasso_model)

# perform k-fold cross-validation
cv_lasso_model <- cv.glmnet(boston_x, boston_y, alpha = 1, standardize = TRUE)
best_lasso_lambda <- cv_lasso_model$lambda.min
best_lasso_lambda

plot(cv_ridge_model)

# use the optimal lambda to perform the best ridge regression model
best_lasso_model <- glmnet(boston_x, boston_y, alpha = 1, lambda = best_lasso_lambda, standardize = TRUE)
lasso_pred <- predict(lasso_model, s = best_lasso_lambda, newx = data.matrix(boston_test[-1]))
lasso_error <- RMSE(lasso_pred, boston_test[, 1])

cat("The RMSE is", lasso_error, "when the value of lambda is", best_lasso_lambda)

## summary
result <- data.frame("Model" = c("PCR", "PLS", "Ridge", "Lasso"), "RMSE" = c(pcr_error, pls_error, ridge_error, lasso_error))
result


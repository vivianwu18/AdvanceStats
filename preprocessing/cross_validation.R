### Cross Validation
# generate sample data
n1 <- 1000
X1 <- runif(n1, min = 0, max = 1)
residual1 <- rnorm(n1, mean = 0, sd = 0.5)
Y1 <- 3 * X1^5 + 2 * X1^2 + residual1
sample_data1 <- data.frame(X1, Y1, residual1)

# split the data into training and testing data set
set.seed(1)
smp_size_sample <- floor(0.8 * nrow(sample_data1))
train_ind_sample <- sample(seq_len(nrow(sample_data1)), size = smp_size_sample)

sample_train1 <- sample_data1[train_ind_sample, ]
sample_test1 <- sample_data1[-train_ind_sample, ]

# split training data into 5 folds
folds <- createFolds(sample_train1$Y, k = 5)

# apply cross validation
cv_errors <- rep(0, 10)
d_range1 <- 1:10

for (j in 1:length(d_range1)) {
  d <- d_range1[j]
  fold_error <- rep(0, 5)
  for (i in 1:5) {
    train <- sample_train1[-folds[[i]], ]
    test <- sample_train1[folds[[i]], ]
    model <- lm(Y1 ~ poly(X1, d), data = train)
    predict <- predict(model, test)
    MSE <- mean((test$Y1 - predict) ^ 2)
    fold_error[i] <- MSE
  }
  
  cv_error <- mean(fold_error)
  cv_errors[j] <- cv_error
}

# plot the relationship between the range of d and MSE
plot(d_range1, cv_errors, xlab = "d range", ylab = "CV Error (MSE)")

# compare training MSE and testing MSE
train_MSE <- rep(0, 10)
test_MSE <- rep(0, 10)

for (i in 1:length(d_range1)) {
  d <- d_range1[i]
  model <- lm(Y1 ~ poly(X1, d), data = sample_train1)
  
  train_predict <- predict(model, sample_train1)
  test_predict <- predict(model, sample_train1)
  
  train_MSE[i] <- mean((sample_train1$Y - train_predict) ^ 2)
  test_MSE[i] <- mean((sample_test1$Y - test_predict) ^ 2)
}

test_MSE[3]
par(mfrow = c(1, 2))
plot(d_range1, train_MSE, xlab = "d range", ylab = "Train MSE")
plot(d_range1, test_MSE, xlab = "d range", ylab = "Test MSE")
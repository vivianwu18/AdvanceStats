### Bias Variance Tradeoff
# generate sample data
options(warn=-1)
set.seed(1)
sample_data2 <- list()
for (i in 1:1000) {
  n <- 100
  X <- runif(n, min = 0, max = 1)
  residual <- rnorm(n, mean = 0, sd = 0.5)
  Y <- 3 * X^5 + 2 * X^2 + residual
  sample_data2[[i]] <- data.frame(X, Y)
}

get_bias <- function(prediction, actual) {
  mean(prediction) - actual
}

# calculate bias and variances
d_range2 <- 1:10
bias2 <- rep(0, 10)
variances2 <- rep(0, 10)
prediction2 <- rep(0, 100)

for (j in 1:length(d_range2)) {
  d <- d_range2[j]
  for (i in 1:100) {
    train <- sample_data2[[i]]
    model <- lm(Y ~ poly(X, d), data = train)
    predict <- predict(model, newdata = data.frame(X = 1.01))
    prediction2[i] <- predict
  }
  y_cal <- 3 * 1.01^5 + 2 * 1.01^2
  bias2[j] <- apply(matrix(prediction2), 2, get_bias, y_cal)
  variances2[j] <- apply(matrix(prediction2), 2, var)
}
print(prediction2)

# plot the bias and variances
par(mfrow = c(1, 2))
plot(d_range2, bias2^2, xlab = "d range", ylab = "Bias")
plot(d_range2, variances2, xlab = "d range", ylab = "Variances")

## Scenario1: X is uniform distribution between 0 and 10
# generate sample data
options(warn=-1)
set.seed(1)
sample_data3 <- list()
for (i in 1:1000) {
  n <- 100
  X <- runif(n, min = 0, max = 10)
  residual <- rnorm(n, mean = 0, sd = 0.5)
  Y <- 3 * X^5 + 2 * X^2 + residual
  sample_data3[[i]] <- data.frame(X, Y)
}

get_bias <- function(prediction, actual) {
  mean(prediction) - actual
}

# calculate bias and variances
d_range3 <- 1:10
bias3 <- rep(0, 10)
variances3 <- rep(0, 10)
prediction3 <- rep(0, 100)

for (j in 1:length(d_range3)) {
  d <- d_range3[j]
  for (i in 1:100) {
    train <- sample_data3[[i]]
    model <- lm(Y ~ poly(X, d), data = train)
    predict <- predict(model, newdata = data.frame(X = 1.01))
    prediction3[i] <- predict
  }
  y_cal <- 3 * 1.01^5 + 2 * 1.01^2
  bias3[j] <- apply(matrix(prediction3), 2, get_bias, y_cal)
  variances3[j] <- apply(matrix(prediction3), 2, var)
}
print(prediction3)

# plot the bias and variances
par(mfrow = c(1, 2))
plot(d_range3, bias3 ^ 2, xlab = "d range", ylab = "Bias")
plot(d_range3, variances3, xlab = "d range", ylab = "Variances")


### Scenario2: testing x is -0.5
# generate sample data
options(warn=-1)
set.seed(1)
sample_data4 <- list()
for (i in 1:1000) {
  n <- 100
  X <- runif(n, min = 0, max = 1)
  residual <- rnorm(n, mean = 0, sd = 0.5)
  Y <- 3 * X^5 + 2 * X^2 + residual
  sample_data4[[i]] <- data.frame(X, Y)
}

get_bias <- function(prediction, actual) {
  mean(prediction) - actual
}

# calculate bias and variances
d_range4 <- 1:10
bias4 <- rep(0, 10)
variances4 <- rep(0, 10)
prediction4 <- rep(0, 100)

for (j in 1:length(d_range4)) {
  d <- d_range4[j]
  for (i in 1:100) {
    train <- sample_data4[[i]]
    model <- lm(Y ~ poly(X, d), data = train)
    predict <- predict(model, newdata = data.frame(X = -0.5))
    prediction4[i] <- predict
  }
  y_cal <- 3 * (-0.5)^5 + 2 * (-0.5)^2
  bias4[j] <- apply(matrix(prediction4), 2, get_bias, y_cal)
  variances4[j] <- apply(matrix(prediction4), 2, var)
}

# plot the bias and variances
par(mfrow = c(1, 2))
plot(d_range4, bias4^2, xlab = "d range", ylab = "Bias")
plot(d_range4, variances4, xlab = "d range", ylab = "Variances")
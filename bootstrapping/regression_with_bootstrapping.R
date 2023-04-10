library(ISLR2)
library(boot)


## Booststrap for Regression
Auto <- Auto

# build the baseline model: mpg ∼ horsepower
model1 <- lm(mpg ~ horsepower, data = Auto)
summary(model1)$coefficients[, 2]

# use bootstrapping
coef_func1 <- function(data, indices){
  model <- lm(mpg ~ horsepower, data = data[indices, ])
  return(coef(model))
}
boot_Auto1 <- boot(data = Auto, R = 1000, statistic = coef_func1)

# display the standard error
print(apply(boot_Auto1$t, 2, sd))


# build the second baseline model: mpg ∼ horsepower + I(horsepower^2)
model2 <- lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
summary(model2)$coefficients[, 2]

# use bootstrapping
coef_func2 <- function(data, indices){
  model <- lm(mpg ~ horsepower + I(horsepower^2), data = data[indices, ])
  return(coef(model))
}
boot_Auto2 <- boot(data = Auto, R = 1000, statistic = coef_func2)

# display the standard error in both cases
print(apply(boot_Auto2$t, 2, sd))


# case 1: mpg ∼ horsepower
print(summary(model1)$coefficients[, 2])
print(apply(boot_Auto1$t, 2, sd))

# case 2: mpg ∼ horsepower + I(horsepower^2)
print(summary(model2)$coefficients[, 2])
print(apply(boot_Auto2$t, 2, sd))

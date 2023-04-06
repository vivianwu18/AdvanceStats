library(ISLR)

### Simple Linear Regression Analysis
# load the data
data("Auto", package = "ISLR")

model <- lm(mpg ~ horsepower, data = Auto)
summary(model)

cor(Auto$mpg, Auto$horsepower)

# check the correlation
plot(Auto$horsepower, Auto$mpg, main = "Correlation between horsepower and mpg", xlab = "horsepower", ylab = "mpg")
abline(model, col = "red")

par(mfrow = c(2, 2))
plot(model)

# find the confidence and prediction interval
predict(model, data.frame(horsepower = 98), interval = "confidence")
predict(model, data.frame(horsepower = 98), interval = "prediction")

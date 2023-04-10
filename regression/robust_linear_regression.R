library(MASS)
library(ggplot2)
library(cowplot)

## Robust Linear Regression
# read the data
data(fat, package = "faraway")
fat <- faraway::fat

# build a lm/rlm model
model_lm <- lm(brozek ~ ., data = fat)
model_rlm <- rlm(brozek ~ ., data = fat)

# summary for lm/rlm models
summary(model_lm)
summary(model_rlm)


par(mfrow = c(1,2))
# plot weight against height
plot1 <- ggplot(aes(x = height, y = weight), data = fat) + geom_point(alpha = 0.7)

# identify outliers
outliers <- which(fat$weight > 300 | fat$height < 30)
fat_outliers <- fat[outliers, ]

# plot the outliers
plot2<- ggplot(aes(x = height, y = weight), data = fat) + geom_point(alpha = 0.7) + 
  geom_point(aes(x = height, y = weight), data = fat_outliers, color = "red", alpha = 0.7)

plot_grid(plot1, plot2, labels = c("Initial Plot", "Plot wiht outliers identificaiton"))

# plot the lm model
par(mfrow = c(2, 2))
plot(model_lm)

# plot the rlm model
par(mfrow = c(2, 2))
plot(model_rlm)
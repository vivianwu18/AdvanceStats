library(faraway)
library(ggplot2)
library(MASS)

### Non constant variance
pipeline <- pipeline

model1 <- lm(Lab ~ Field, data = pipeline)
summary(model1)

ggplot(pipeline, aes(x = Field, y = Lab)) + geom_point() + geom_smooth(method = "lm")
ggplot(pipeline, aes(x = Field, y = model1$residuals)) + geom_point() + geom_hline(yintercept = 0) + ylab("Residuals")

### Data Transfromation
## Transform Lab
# Try square root transformation
plots_list <- list (
  ggplot(pipeline, aes(x = Field, y = sqrt(Lab))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("sqrt(Lab) ~ Field"),
  
  # Try log transformation
  ggplot(pipeline, aes(x = Field, y = log(Lab))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("log(Lab) ~ Field"),
  
  # Try inverse transformation
  ggplot(pipeline, aes(x = Field, y = 1/Lab)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("1/Lab ~ Field"),
  
  ## Transform Field
  # Try square root transformation
  ggplot(pipeline, aes(x = Field, y = sqrt(Lab))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("Lab ~ sqrt(Field)"),
  
  # Try log transformation
  ggplot(pipeline, aes(x = Field, y = log(Lab))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("Lab ~ log(Field)"),
  
  # Try inverse transformation
  ggplot(pipeline, aes(x = Field, y = 1/Lab)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("Lab ~ 1/Field"),
  
  ## Transform Lab & Field
  # Try square root transformation
  ggplot(pipeline, aes(x = sqrt(Field), y = sqrt(Lab))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("sqrt(Lab) ~ sqrt(Field)"),
  
  # Try log transformation
  ggplot(pipeline, aes(x = log(Field), y = log(Lab))) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("log(Lab) ~ log(Field)"),
  
  # Try inverse transformation
  ggplot(pipeline, aes(x = 1/Field, y = 1/Lab)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("1/Lab ~ 1/Field")
)


# Plot the ggplots in a 3 x 3 grid
plot_grid(plotlist = plots_list, ncol = 3, nrow = 3, align = "hv")

# Plot (residuals vs fitted) to check transformed variance.  
par(mfrow = c(3, 3))
model_sqrt <- lm(sqrt(Lab) ~ Field, data = pipeline)
plot(model_sqrt, which = 1)
model_log <- lm(log(Lab) ~ Field, data = pipeline)
plot(model_log, which = 1)
model_inv <- lm(1/Lab ~ Field, data = pipeline)
plot(model_inv, which = 1)

model_sqrt_f <- lm(Lab ~ sqrt(Field), data = pipeline)
plot(model_sqrt_f, which = 1)
model_log_f <- lm(Lab ~ log(Field), data = pipeline)
plot(model_log_f, which = 1)
model_inv_f <- lm(Lab ~ 1/Field, data = pipeline)
plot(model_inv_f, which = 1)

model_sqrt_xy <- lm(sqrt(Lab) ~ sqrt(Field), data = pipeline)
plot(model_sqrt_xy, which = 1)
model_log_xy <- lm(log(Lab) ~ log(Field), data = pipeline)
plot(model_log_xy, which = 1)
model_inv_xy <- lm(1/Lab ~ 1/Field, data = pipeline)
plot(model_inv_xy, which = 1)

### Box Cox Transformation
ozone <- ozone
bc1 <- boxcox(O3 ~ temp + humidity + ibh, data = ozone)
plot(bc1)

# choose lambda
lambda1 <- bc1$x[which.max(bc1$y)]
cat("The lmabda used for the best transformation is", lambda1)

if (lambda1 == 0) {
  ozone$O3_transformed <- log(ozone$O3)
} else {
  ozone$O3_transformed <- (ozone$O3^lambda1 - 1) / lambda1
}

# plot the result
par(mfrow = c(1, 2))
model_ozone <- lm(O3 ~ temp + humidity + ibh, data = ozone)
plot(model_ozone, which = 1)
model_ozone_transformed <- lm(O3_transformed ~ temp + humidity + ibh, data = ozone)
plot(model_ozone_transformed, which = 1)
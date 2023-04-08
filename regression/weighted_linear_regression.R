library(dplyr)

### Weighted Linear Regression
set.seed(1)
pipeline <- faraway::pipeline

# check the heteroscedasticity for the model without weights
initial_model <- lm(Lab ~ Field, data = pipeline)
summary(initial_model)

# set up the necessary parameters
i <- order(pipeline$Field)
npipe <- pipeline[i,]
ff <- gl(12, 9)[-108]
meanfield <- unlist(lapply(split(npipe$Field, ff), mean))
varlab <- unlist(lapply(split(npipe$Lab, ff), var))

# perform the model
weight_model <- lm(log(varlab[-12]) ~ log(meanfield[-12]))
summary(weight_model)

# calculate weights for WLS
a0 <- exp(weight_model$coefficients[1])
a1 <- weight_model$coefficients[2]
wt <- 1 / (a0 * (meanfield ^ a1))

# assign weights to each group
pipeline <- pipeline %>%
  arrange(Field) %>%
  mutate(group = ff, weight = wt[ff])

wls_model <- lm(Lab ~ Field, data = pipeline, weights = weight)
summary(wls_model)
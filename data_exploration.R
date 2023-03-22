library(readr)
library(dplyr)
library(tidyr)
library(corrplot)
library(readr)
library(moments)
library(e1071)
library(caret)
library(ggplot2)
options(digits = 4)

### Data Exploration
# load the data
source_path <- "/Users/vivianwu/Desktop/BAX 442/HW1/crx.csv"
data <- read_csv(source_path, col_names = FALSE, 
                 col_types = cols(X2 = col_double(), X11 = col_number(), X14 = col_number()))
head(data, 10)

# understand the structure of the data
data[data$X16 == "+",]
str(data)

descriptive_stat <- function(x) {
  c(Mean = round(mean(x, na.rm = TRUE), 2),
    Variance = round(var(x, na.rm = TRUE), 2),
    StdDev = round(sd(x, na.rm = TRUE), 2),
    Q1 = round(quantile(x, 0.25, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    Q3 = round(quantile(x, 0.75, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2),
    Range = round((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)), 2))
}

# calculate all statistics for numeric characteristics
descriptive_stat(data$X2)
descriptive_stat(data$X3)
descriptive_stat(data$X8)
descriptive_stat(data$X11)
descriptive_stat(data$X14)
descriptive_stat(data$X15)

# find the unique values for each feature
unique(data$X1)
unique(data$X4)
unique(data$X5)
unique(data$X6)
unique(data$X7)
unique(data$X9)
unique(data$X10)
unique(data$X12)
unique(data$X13)
unique(data$X16)

# check the skewness of the target variable
transform_x16 <- ifelse(data$X16 == "+", 1, 0)
skewness(transform_x16, type = 2) # 0.2212

# create a frequency table
frequency_table <- table(data$X10, data$X16)
frequency_table

# create a marginal frequency table
marginal_frequency_table_v10 <- margin.table(frequency_table, 1)
marginal_frequency_table_v16 <- margin.table(frequency_table, 2)

marginal_frequency_table_v10
marginal_frequency_table_v16

# create a row-wise proportion table
rowwise_proportion_table <- prop.table(frequency_table, 1)
rowwise_proportion_table

# calculate the accuracy
V10_accuracy <- 209 / (209 + 86)
V10_accuracy

### Data cleaning
# find out the most frequent in each column
names(which.max(table(data$X1)))
names(which.max(table(data$X4)))
names(which.max(table(data$X5)))
names(which.max(table(data$X6)))
names(which.max(table(data$X7)))

# replace missing values with the most frequent value
data$X1[data$X1 == "?"] <- "b"
data$X4[data$X4 == "?"] <- "u"
data$X5[data$X5 == "?"] <- "g"
data$X6[data$X6 == "?"] <- "c"
data$X7[data$X7 == "?"] <- "v"

# convert the target variable to binary variable
data$X16 <- ifelse(data$X16 == "+", TRUE, FALSE)

# discretization
EqualWidth <- function(data, bins, plotit = FALSE){
  minvalue <- min(data, na.rm = TRUE)
  maxvalue <- max(data, na.rm = TRUE)
  width = (maxvalue - minvalue) / bins
  if (plotit == TRUE){
    barplot(table(cut(data, breaks = seq(minvalue, maxvalue, width))))
  }
}
EqualWidth(data$X2, bins = 10, plotit = TRUE)


# scaling
numeric_data <- data[, c("X2", "X3", "X8", "X11", "X14", "X15")]

hist(numeric_data$X2)
hist(numeric_data$X3)
hist(numeric_data$X8)
hist(numeric_data$X11)
hist(numeric_data$X14)
hist(numeric_data$X15)

skewness(data$X2, type = 2, na.rm = TRUE)

# 1. normalize Data with Min-Max Scaling
process <- preProcess(as.data.frame(numeric_data[2:6]), method = c("range"))
numeric_data_scale1 <- predict(process, as.data.frame(numeric_data[2:6]))
print(head(numeric_data_scale1, 10))

# 2.normalize Data with Standard Scaling
numeric_data_scale2 <- scale(numeric_data[1])
print(head(numeric_data_scale2, 10))

# identify outliers for V3
V3_Q1 <- round(quantile(data$X3, 0.25, na.rm = TRUE), 2)
V3_Q3 <- round(quantile(data$X3, 0.75, na.rm = TRUE), 2)
V3_IQR <- V3_Q3 - V3_Q1

V3_outlier_upper <- V3_Q3 + 1.5 * V3_IQR
V3_outlier_lower <- V3_Q1 - 1.5 * V3_IQR

data[(data$X3 < V3_outlier_lower | data$X3 > V3_outlier_upper), ]

# create a density plot for V2
V2_density <- density(data$X2, na.rm = TRUE)
plot(V2_density, main = "Density plot of V2")


# correlation analysis (redundant or non-informative features)
cor(data$X3, data$X15)

# filter out the numeric variables
corr_matrix_numeric <- data %>%
  select_if(is.numeric) %>%
  cor(method = "pearson", use = "pairwise.complete.obs")

corrplot(corr_matrix_numeric, method = "circle")
corr_matrix_numeric
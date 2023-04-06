library(jpeg)
library(abind)
library(ggplot2)

# read in image
tiger <- readJPEG("tiger.jpeg")

## store all the three matrices into separate variables.
variable_1 <- tiger[,,1]
variable_2 <- tiger[,,2]
variable_3 <- tiger[,,3]

## use PCA to compress the data
variable_1_pca <- prcomp(variable_1,center = FALSE)
variable_2_pca <- prcomp(variable_2,center = FALSE)
variable_3_pca <- prcomp(variable_3,center = FALSE)

## collect the PCA objects into a list
tiger_pca <- list(variable_1_pca, variable_2_pca, variable_3_pca)

# plot the fraction of variance as k increases
par(mfrow=c(1,3))
plot(cumsum(variable_1_pca$sdev^2/sum(variable_1_pca$sdev^2)), 
     xlab="Number of Principal Components (k)", 
     ylab="Fraction of Variance", 
     main="Red Component", 
     col="red")
plot(cumsum(variable_2_pca$sdev^2/sum(variable_2_pca$sdev^2)), 
     xlab="Number of Principal Components (k)", 
     ylab="Fraction of Variance", 
     main="Green Component",
     col="green")
plot(cumsum(variable_3_pca$sdev^2/sum(variable_3_pca$sdev^2)), 
     xlab="Number of Principal Components (k)", 
     ylab="Fraction of Variance", 
     main="Blue Component",
     col="blue")

## Reconstruct the compressed image from principal components
for (i in c(3, 5, 10, 25, 50, 100, 150, 200, 250, 300, 350, ncol(variable_1_pca$x))) {
  new_image <- abind(variable_1_pca$x[,1:i] %*% t(variable_1_pca$rotation[,1:i]),
                     variable_2_pca$x[,1:i] %*% t(variable_2_pca$rotation[,1:i]),
                     variable_3_pca$x[,1:i] %*% t(variable_3_pca$rotation[,1:i]),
                     along = 3)
  writeJPEG(new_image, paste0('Compressed_image_with_',i, '_components.jpg'))
  
}

# set the range of k values
k_values <- c(3, 5, 10, 25, 50, 100, 150, 200, 250, 300, 350, ncol(variable_1_pca$x))

# calculate the compression ratios for each k value
compression_ratios <- numeric(length(k_values))
original_size <- file.info('tiger.jpeg')$size / 1024 # size of original image in KB

for (i in seq_along(k_values)) {
  # load the compressed image and calculate its size
  path <- paste0('Compressed_image_with_', k_values[i], '_components.jpg')
  compressed_size <- file.info(path)$size / 1024
  
  # calculate the compression ratio and store it in the vector
  compression_ratios[i] <- compressed_size / original_size
}

# plot the compression ratios as a function of k
df <- data.frame(k = k_values, ratio = compression_ratios)
ggplot(df, aes(x = k, y = ratio)) + 
  geom_line() + 
  geom_point() +
  xlab("Number of Principal Components") +
  ylab("Compression Ratio") +
  ggtitle("Compression Ratio vs Number of Principal Components")
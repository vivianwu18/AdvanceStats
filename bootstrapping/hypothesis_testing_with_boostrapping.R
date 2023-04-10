## Hypothesis Testing using boostraapping
# run a t-test
A <- c(10.2, 8.1, 9.4, 8.7, 9.2, 7.5,9.9, 8.9, 10.1)
B <- c(9.4, 7.1, 7.9, 8.5, 8.1, 8, 5.7, 7.5, 9)

t.test(A, B, alternative = "two.sided", var.equal = FALSE)

# calculate the mean difference for two samples
B <- 1000
mean_diff_list <- rep(0, B)
for (i in 1:B){
  resampled_A <- sample(A, length(A), replace = TRUE)
  resampled_B <- sample(B, length(B), replace = TRUE)
  
  mean_diff <- mean(resampled_A) - mean(resampled_B)
  mean_diff_list[i] <- mean_diff
}

# find the quantile
quantiles <- quantile(mean_diff_list, c(0.025, 0.975))
quantiles

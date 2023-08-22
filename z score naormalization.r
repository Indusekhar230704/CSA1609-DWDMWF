# Example .rdata
data <- c(12, 25, 30, 42, 50, 58, 65, 73, 80, 92)
# Calculate mean and standard deviation
mean_data <- mean(data)
sd_data <- sd(data)
# Perform Z-score normalization
z_scores <- (data - mean_data) / sd_data
# Print the original data and the normalized data
print("Original Data:")
print(data)
print("Z-score Normalized Data:")
print(z_scores)

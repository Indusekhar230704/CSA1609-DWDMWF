# Sample data
data <- c(12, 25, 30, 42, 50, 58, 65, 73, 80, 92)
# Calculate quartiles
q1 <- quantile(data, 0.25)
q2 <- quantile(data, 0.5)  # This is the median
q3 <- quantile(data, 0.75)
# Print quartiles
print(q1)
print(q2)
print(q3)
quantiles <- quantile(data, c(0.25, 0.5, 0.75))
print(quantiles)

eq freq--------------------------

data <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
num_bins <- 3
bin_width <- (max(data) - min(data)) / num_bins
bin_boundaries <- seq(min(data), max(data), by = bin_width)
bins <- cut(data, breaks = bin_boundaries, include.lowest = TRUE)
print(bins)

equal width----------------------------
# Sample data
data <- c(12, 25, 38, 42, 55, 62, 70, 82, 95, 105)

# Function to perform equal-width binning
equal_width_binning <- function(data, num_bins) {
  # Calculate the range of the data
  data_range <- range(data)
  
  # Calculate the bin width
  bin_width <- (data_range[2] - data_range[1]) / num_bins
  
  # Create the bins
  bins <- seq(data_range[1], data_range[2], by = bin_width)
  
  # Perform binning
  bin_labels <- cut(data, breaks = bins, labels = FALSE, include.lowest = TRUE)
  
  return(bin_labels)
}

# Number of bins
num_bins <- 3

# Perform equal-width binning
bin_labels <- equal_width_binning(data, num_bins)

# Print the results
cat("Data:", data, "\n")
cat("Bin Labels:", bin_labels, "\n")
clustering--------------------------------------------------
# Load the necessary library for k-means clustering
library(stats)

# Load the Iris dataset
data(iris)

# Select the columns you want to cluster on (e.g., Sepal Length and Width)
features <- iris[, c("Sepal.Length", "Sepal.Width")]

# Set the number of clusters (k) you want to create
num_clusters <- 3

# Perform k-means clustering
kmeans_result <- kmeans(features, centers = num_clusters)

# Get cluster assignments for each data point
cluster_assignments <- kmeans_result$cluster

# Print the cluster assignments
print(cluster_assignments)

# Visualize the clustering results (optional)
plot(features, col = cluster_assignments)
points(kmeans_result$centers, col = 1:num_clusters, pch = 8, cex = 2)



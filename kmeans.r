
# Data
data <- data.frame(
  x = c(2,2,8,5,7,6,1,4),
  y = c(10,5,4,8,5,4,2,9)
)

# Labels
names <- paste0("A",1:8)

# K-means (given centroids, 1 iteration)
result <- kmeans(data,
                 centers = data[c(1,4,7),],
                 iter.max = 1)

# Plot points
plot(data$x, data$y,
     col = result$cluster,
     pch = 19,
     xlab = "X",
     ylab = "Y",
     main = "K-Means Clustering")

# Add labels
text(data$x, data$y, labels = names, pos = 3)

# Plot centroids
points(result$centers,
       col = 1:3,
       pch = 8,
       cex = 2)
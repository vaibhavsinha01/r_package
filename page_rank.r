Page Ranking Algorithm - which is discussed in the lab class but in lms submission is not provided. but FAT lab question will be ....
# Install if not already installed
install.packages("igraph")

# Load library
library(igraph)

# Define edges
edges <- c("A","B", "A","C",
           "B","C", "B","D",
           "C","A",
           "D","C")

# Create graph
g <- graph(edges, directed=TRUE)

# Compute PageRank
pr <- page_rank(g, damping = 0.85)

# Output
print(pr$vector)
# Scale node sizes based on PageRank
V(g)$size <- 20 + 100 * pr$vector

# Add labels
V(g)$label <- names(pr$vector)

# Color nodes
V(g)$color <- "skyblue"

# Edge style
E(g)$arrow.size <- 0.5

# Plot graph
plot(g, main="PageRank Visualization")

plot(g,
     layout = layout_with_fr(g),   # Force-directed layout
     vertex.size = V(g)$size,
     vertex.label = V(g)$label,
     vertex.color = "lightgreen",
     edge.arrow.size = 0.6,
     main = "PageRank (Node Size ∝ Importance)")

# Find highest PageRank node
top_node <- names(which.max(pr$vector))

# Color top node differently
V(g)$color <- ifelse(V(g)$name == top_node, "red", "lightblue")

plot(g,
     layout = layout_with_fr(g),
     vertex.size = V(g)$size,
     main = paste("Top Page:", top_node))
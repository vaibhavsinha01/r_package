install.packages("igraph")
library(igraph)

edges <- c("A","B", "A","C","B","C", "B","D","C","A","D","C")

g <- graph(edges, directed=TRUE)
pr <- page_rank(g, damping = 0.85)

print(pr$vector)
V(g)$size <- 20 + 100 * pr$vector
V(g)$label <- names(pr$vector)
V(g)$color <- "skyblue"
E(g)$arrow.size <- 0.5
plot(g, main="PageRank Visualization")
plot(g,layout = layout_with_fr(g), vertex.size = V(g)$size,vertex.label = V(g)$label,vertex.color = "lightgreen",edge.arrow.size = 0.6,main = "PageRank (Node Size ∝ Importance)")

# Find highest PageRank node
top_node <- names(which.max(pr$vector))

# Color top node differently
V(g)$color <- ifelse(V(g)$name == top_node, "red", "lightblue")

plot(g,layout = layout_with_fr(g),vertex.size = V(g)$size,main = paste("Top Page:", top_node))
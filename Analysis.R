########################################################################
# Preparations
########################################################################

# Load packages
library(igraph)

# Set working directory
setwd("/Users/nunnlab/Desktop/GitHub/Primate_social_networks/")

# Load example adjacency matrices
Papio.adj <- read.csv("data/Papio_adjacency.csv", row.names=1)
Papio.adj <- as.matrix(Papio)
Pan.adj <- read.csv("data/Pan_adjacency.csv", row.names=1)
Pan.adj <- as.matrix(Pan)

# Convert adjacency matrix into edge list with weight attribute
Papio.edges <- graph_from_adjacency_matrix(Papio.adj, mode="undirected", weighted=TRUE)
Pan.edges <- graph_from_adjacency_matrix(Pan.adj, mode="undirected", weighted=TRUE)

########################################################################
# Visualization
########################################################################

# Most basic graph
quartz()
plot(Papio.edges, main="Papio")

# Weighted edges and color coded sex
quartz()
pap.sexes <- vertex_attr(Papio.edges)$name
pap.sexes <- gsub("M[0-9]*", "skyblue", pap.sexes)
pap.sexes <- gsub("F[0-9]*", "hotpink", pap.sexes)
plot(Papio.edges, main="Papio- pretty version", vertex.label.cex=0.5, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.25, edge.color="black")

# Some different graph layouts
quartz()
layout(matrix(1:9, 3, 3, byrow=TRUE))
par(mar=c(1,0,3,0))
plot(Papio.edges, main="Grid", layout=layout_on_grid, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Circle", layout=layout_in_circle, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Sphere", layout=layout_on_sphere, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Simulated annealing", layout=layout_with_dh, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Force directed", layout=layout_with_fr, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Spring model", layout=layout_with_kk, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Multidimensional scaling", layout=layout_with_mds, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Randomly", layout=layout_randomly, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")
plot(Papio.edges, main="Nicely", layout=layout_nicely, vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.1, edge.color="black")

# Visually compare Papio and Pan
quartz()
layout(matrix(1:2, 1, 2))
par(mar=c(0,0,2,0))
plot(Papio.edges, main="Papio", vertex.label=NA, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.25, edge.color="black", layout=layout_with_fr)
pan.sexes <- vertex_attr(Pan.edges)$name
pan.sexes <- gsub("[A-D]M[0-9]*", "skyblue", pan.sexes)
pan.sexes <- gsub("[A-D]F[0-9]*", "hotpink", pan.sexes)
plot(Pan.edges, main="Pan", vertex.label=NA, vertex.color=pan.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.25, edge.color="black", layout=layout_with_fr)

########################################################################
# Analysis- Individual level
########################################################################

# Degree
pan.degree <- degree(Pan.edges)

# Weighted degree, or strength
pan.strength <- Pan.edges

# Weighted closeness
pan.closeness <- closeness(Pan.edges)

# Weighted betweenness
pan.betweenness <- betweenness(Pan.edges)

# Eigenvector centrality
pan.eigencent <- eigen_centrality(Pan.edges)

# Local weighted clustering coefficient
pan.trans <- transitivity(Pan.edges, type="weighted")

# Plot networks with node size propotional to centrality
layout(matrix(1:6, 2, 3, byrow = TRUE))

########################################################################
# Analysis- Network level
########################################################################



# Eigenvector centralization index

# Global weighted clustering coefficient
transitivity(Pan.edges, type="barrat")











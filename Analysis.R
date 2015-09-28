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

# Most basic graph for Papio
quartz()
plot(Papio.edges, main="Papio")

# Weighted edges and color coded sex for Papio
quartz()
pap.sexes <- V(Papio.edges)$name
pap.sexes <- gsub("M[0-9]*", "skyblue", pap.sexes)
pap.sexes <- gsub("F[0-9]*", "hotpink", pap.sexes)
plot(Papio.edges, main="Papio- pretty version", vertex.label.cex=0.5, vertex.color=pap.sexes, vertex.size=10, edge.width=edge_attr(Papio.edges)$weight*0.25, edge.color="black")

# Add attributes to new network, Papio.edges2
Papio.edges2 <- set.vertex.attribute(Papio.edges, "color", value=pap.sexes)
Papio.edges2 <- set.vertex.attribute(Papio.edges2, "size", value=10)
Papio.edges2 <- set.edge.attribute(Papio.edges2, "width", value=edge_attr(Papio.edges)$weight*0.1)
Papio.edges2 <- set.edge.attribute(Papio.edges2, "color", value="black")

# Visualize different graph layouts for Papio
quartz()
layout(matrix(1:9, 3, 3, byrow=TRUE))
par(mar=c(1,0,3,0))
plot(Papio.edges2, main="Grid", layout=layout_on_grid, vertex.label=NA)
plot(Papio.edges2, main="Circle", layout=layout_in_circle, vertex.label=NA)
plot(Papio.edges2, main="Sphere", layout=layout_on_sphere, vertex.label=NA)
plot(Papio.edges2, main="Simulated annealing", layout=layout_with_dh, vertex.label=NA)
plot(Papio.edges2, main="Force directed", layout=layout_with_fr, vertex.label=NA)
plot(Papio.edges2, main="Spring model", layout=layout_with_kk, vertex.label=NA)
plot(Papio.edges2, main="Multidimensional scaling", layout=layout_with_mds, vertex.label=NA)
plot(Papio.edges2, main="Randomly", layout=layout_randomly, vertex.label=NA)
plot(Papio.edges2, main="Nicely", layout=layout_nicely, vertex.label=NA)

# Define vector of colors based on sex for Pan
pan.sexes <- vertex_attr(Pan.edges)$name
pan.sexes <- gsub("[A-D]M[0-9]*", "skyblue", pan.sexes)
pan.sexes <- gsub("[A-D]F[0-9]*", "hotpink", pan.sexes)

# Add attributes to Pan network
Pan.edges2 <- set.vertex.attribute(Pan.edges, "color", value=pan.sexes)
Pan.edges2 <- set.vertex.attribute(Pan.edges2, "size", value=10)
Pan.edges2 <- set.edge.attribute(Pan.edges2, "width", value=edge_attr(Pan.edges)$weight*0.1)
Pan.edges2 <- set.edge.attribute(Pan.edges2, "color", value="black")

# Visually compare Papio and Pan
quartz()
layout(matrix(1:2, 1, 2))
par(mar=c(0,0,2,0))
plot(Papio.edges2, main="Papio", vertex.label=NA)
plot(Pan.edges2, main="Pan", vertex.label=NA)

########################################################################
# Analysis- Individual level
########################################################################

# Degree
pan.degree <- degree(Pan.edges)
papio.degree <- degree(Papio.edges)

# Weighted degree, or strength
pan.strength <- strength(Pan.edges)
papio.strength <- strength(Papio.edges)

# Weighted closeness
pan.closeness <- closeness(Pan.edges)
paio.closeness <- closeness(Papio.edges)

# Weighted betweenness
pan.betweenness <- betweenness(Pan.edges)
papio.betweenness <- betweenness(Papio.edges)

# Eigenvector centrality
pan.eigencent <- eigen_centrality(Pan.edges)
papio.eigencent <- eigen_centrality(Papio.edges)

# Local weighted clustering coefficient
pan.trans <- transitivity(Pan.edges, type="weighted", isolates="zero")
papio.trans <- transitivity(Papio.edges, type="weighted", isolates="zero")

# Plot networks with node size propotional to centrality
layout(matrix(1:6, 2, 3, byrow = TRUE))

########################################################################
# Analysis- Network level
########################################################################

# Centralization scores for Papio
pap.centz <- centr_degree(Papio.edges)$centralization
pap.centz[2] <- centr_clo(Papio.edges)$centralization
pap.centz[3] <- centr_betw(Papio.edges)$centralization
pap.centz[4] <- centr_eigen(Papio.edges)$centralization

# Centralization scores for Pan
pan.centz <- centr_degree(Pan.edges)$centralization
pan.centz[2] <- centr_clo(Pan.edges)$centralization
pan.centz[3] <- centr_betw(Pan.edges)$centralization
pan.centz[4] <- centr_eigen(Pan.edges)$centralization

# Centralization table
tab.centz <- rbind(Papio=pap.centz, Pan=pan.centz)
dimnames(tab.centz)[[2]] <- c("Degree", "Closeness", "Betweenness", "Eigenvector")
tab.centz

# Plot histograms of centrality scores
quartz()
layout(matrix(1:10, 2, 5, byrow = TRUE))
par(mar=c(3,5,3,1))
hist(papio.degree, main="Degree", col="gray", xlab="", ylab="")
mtext("Papio", side=2, line=3)
hist(papio.strength, main="Strength", col="gray", xlab="", ylab="")
hist(papio.closeness, main="Closeness", col="gray", xlab="", ylab="")
hist(papio.betweenness, main="Betweenness", col="gray", xlab="", ylab="")
hist(papio.eigencent, main="Eigenvector centrality", col="gray", xlab="", ylab="")
hist(pan.degree, main="", col="gray", xlab="", ylab="")
mtext("Pan", side=2, line=3)
hist(pan.strength, main="", col="gray", xlab="", ylab="")
hist(pan.closeness, main="", col="gray", xlab="", ylab="")
hist(pan.betweenness, main="", col="gray", xlab="", ylab="")
hist(pan.eigencent, main="", col="gray", xlab="", ylab="")

# Clustering algorithm for baboons and chimps
Papio.clust <- cluster_spinglass(Papio.edges)
Pan.clust <- cluster_spinglass(Pan.edges)

# Overlay modules onto network plots
quartz()
layout(matrix(1:2, 1, 2))
par(mar=c(1,1,2,1))
plot(Papio.edges2, vertex.label=NA, mark.groups=communities(Papio.clust), main="Papio")
plot(Pan.edges2, vertex.label=NA, mark.groups=communities(Pan.clust), main="Pan")












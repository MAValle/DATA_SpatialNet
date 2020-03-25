
# Simulation with small world networks using spatial coordinates provided in 
# TIEDENSITY.RData.


# creation: 14-ene-20
# name: simulation_V6.R

rm(list = ls())
library(ggplot2)
library(gdata)
library(igraph)
source("lattice_creation_function.R")
source("genBANet_function.R")
source("genBA_SpatialNet_function.R")
source("get_powerLaw_prob_function.R")
source("genSpatialDistribution_function.R")
source("small_world_spatial_reconnect_function.R")
source("put_humanoids_function.R")

load("TIEDENSITY.RData")

# # # # # Create the grille
# https://stackoverflow.com/questions/26086955/set-up-a-igraph-graph-using-the-xy-co-ordinates-for-nodes
# necesito partir con esta custom surface con todos lso nodos conectados a 
# sus vecinos mas cercanos.




N = 50 #total number of nodes (humanoids) of the grid-net

grilla <- make_lattice(length = N, dim = 2)
V(grilla)$name <- seq(from =1, to=N*N)

par(mar = c(1.5, 1.5, 1.5, 1.5))
plot(grilla, layout=layout_on_grid,
     edge.arrow.size =1.5,
     edge.curved = 0,
     vertex.color = "yellow",
     vertex.frame.color="#555555",
     #vertex.label = V(grilla)$name,
     vertex.label.color = "black",
     vertex.label.cex=.2,
     vertex.size = 1)
title("Grille", cex.main=0.5, col.main="black")
hist(degree(grilla))



# dic 27, 2019
# Now we try with the spatial small world network
# 1. get the coordinates from the lattice: coor <- layout_on_grid(grilla)
coord <- layout_on_grid(grilla)
coord <- as.data.frame(coord)
human <- seq(from = 1, to = N*N)
coord <- cbind(human, coord)
colnames(coord) <- c("human", "x", "y")
# 2. get the spatial distribution
C <- genStatialDistribution(df = coord, alpha = 1.1) # constante

# 3. get the distances among nodes in the lattice
distances <- dist(coord[, c(2,3)] )
distances <- as.matrix(distances)

# The magic!!
# # # # # Select other node k, and reconnect with node i with probability p
nn <- N*N
for (i in seq_along(1:nn)) {
  grilla <- small_world_spatial_reconnect(g = grilla, C=C, distances=distances, vtx = i, alpha = 1.1, ord = 1) 
}
# # # # # Select other node k, and reconnect with node i with probability p


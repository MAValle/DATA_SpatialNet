
# Simulation with small worldnetworks spatial networks


# creation: 26-dic-19
# name: simulation_V3.R


# Procedure (generate a small world net) in a grid:
# 1. set a grid of NXN. Here, in every node there is a humanoid.
# 2. select node i (could be in order starting from node 1)
# 3. select each neighbour of node i, and with probability p
#   we reconnect this edge to another vertex k at random not in {i,j}.
# 4. back to 2 until all nodes have been reconnected.
# 5. repeat 2-6 with second neares neighborhood.

# necessary functions:
# to determine the neigboors grade 1 for node i.
# to determine the neigboors grade 1 for node i.
# to do steps 3,4, and 5.

# For spatial net:
# in step 3, p=C x d_ij ^-alpha
# in step 4, if u < 1-p, select k \ {i,j} and do 5.


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

# # # # # Create the grille
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


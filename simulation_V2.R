
# Simulation with BA spatial networks


# creation: 04-cic-19
# name: simulation_V2.R

rm(list = ls())
library(ggplot2)
library(gdata)
library(igraph)
source("lattice_creation_function.R")
source("genBANet_function.R")
source("genBA_SpatialNet_function.R")
#parameters
m0 = 3 #initial number of full connected nodes
m = 2 #number of connection desired for each ieration
N = 200 #total number of nodes (iterations) of the net
alpha = 1.1

g_spatial <- genBA_SpatialNet(m0 = 3, m=2, N=200, alpha=1.1, size_lattice=100)

# plot the spatial net from adjacency matrix A
par(mar = c(1.5, 1.5, 1.5, 1.5))
plot(g_spatial,
     edge.arrow.size =.3,
     edge.curved = 0,
     vertex.color = "yellow",
     vertex.frame.color="#555555",
     #vertex.label = V(mst_g)$name,
     vertex.label.color = "black",
     vertex.label.cex=.6,
     vertex.size = 7)
title("Spatial BA Net", cex.main=0.5, col.main="black")
hist(degree(g_spatial))


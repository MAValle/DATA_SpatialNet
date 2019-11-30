
# Simulation with ER networks


# creation: 19-nov-19
# name: simulation_V1.R

rm(list = ls())
library(ggplot2)
library(gdata)
library(igraph)
source("lattice_creation_function.R")
source("get_powerLaw_prob_function.R")
source("genSpatialDistribution_function.R")
source("genSpatialNet_function.R")

N = 100
a = 2.0

# obs: cuando N/m^2 es aprox igual al 10%, la red comienza a conectarse mas.
# n/m^2 es la probabilidad que un hueco del lattice sea ocupado por un humanoide.

df <- lattice_creation(m = 30, number_of_humans = N)
C <- genStatialDistribution(df = df, alpha = a)
g <- genSpatialNet(df = df, number_of_humans = N, C = C, alpha = a)

par(mar = c(1.5, 1.5, 1.5, 1.5))
plot(g,
     edge.arrow.size =.3,
     edge.curved = 0,
     vertex.color = "yellow",
     vertex.frame.color="#555555",
     #vertex.label = V(mst_g)$name,
     vertex.label.color = "black",
     vertex.label.cex=.6,
     vertex.size = 7)
title("Spatial ER Social Net", cex.main=0.5, col.main="black")


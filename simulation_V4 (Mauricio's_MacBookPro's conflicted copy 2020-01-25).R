
# Simulation of ER net using spatial coordinates provided in 
# TIEDENSITY.RData.



# creation: 04-ene-20
# name: simulation_V4.R


# Procedure (generate a small world net) in a grid:
# 1. set a grid of NXN. Here, in every node there is a humanoid.
# 2. select node i (could be in order starting from node 1)
# 3. select each neighbour of node i, and with probability p
#   we reconnect this edge to another vertex k at random not in {i,j}.
# 4. back to 2 until all nodes have been reconnected.
# 5. repeat 2-6 with second neares neighborhood.




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
source("genSpatialNet_function.R")
source("put_humanoids_function.R")

#load("TIEDENSITY.RData")
#save(file="tidensity_victor.RData")
#write.csv(coords,'coordinates_victor.csv')
coords <- read.csv('coordinates_victor.csv')

# Get an idea of the "tiedensity" surface in which we have to simulate the social net
plot(coords, xlab="x", ylab="y", xlim=xLims, ylim=yLims, asp=1, type="n")
lines(circ, col="blue", lwd=2)
points(coords, pch=16, cex=1.5)
head(coords) # 757   2


# Jan 04, 2020
# # # # Simulation ER social net in tiedensity surface
N = seq(from=100, to=500, by=2)
a = seq(from=1.1, to=1.5, by=0.01) # clustering exponent. the Higher the exponent, more difficult the conection at higher distances

# necesito, dado coords, samplearlas sin reemplazo para colocar humanoides
# en esa grilla semicircular. El resultado debe ser un dataframe com
# human, x , y.
df <- put_humanoids2(coords = coords, N=N[1])
C <- genStatialDistribution(df = df, alpha = a[1])
g <- genSpatialNet(df = df, number_of_humans = N[1], C = C, alpha = a[1])

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










# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# jan 22, 2020
# necesito siumular muchas redes con tamanos de N = 100 a 500
# y calcular:
# number of edges, E
# density D
# clustering coefficient C
# mean path link L
# w
# SWI 

# Starting simulations
N = seq(from=100, to=500, by=2)
a = seq(from=1.1, to=1.5, by=0.01) # clustering exponent. the Higher the exponent, more difficult the conection at higher distances

library(svMisc)
container <- matrix(NA, nrow=length(N), ncol=9)
colnames(container) <- c("it", "alpha",  "N", "E", "D", "C", "L", "w", "SWI")
for (i in seq_along(1:length(N))) {
  progress(i)
  Sys.sleep(0.01)
  # simulation of the ER spatial network
  df <- put_humanoids2(coords = coords, N=N[i]) 
  C <- genStatialDistribution(df = df, alpha = a[1])
  g <- genSpatialNet(df = df, number_of_humans = N[i], C = C, alpha = a[1])
  
  # parameters for a random graph
  Cr <- mean(degree(g))/N[i]
  Lr <- log(N[1])/log(mean(degree(g)))
  # parameters for a lattice graph
  Cl <- (3*(mean(degree(g))-2)) / (3*(mean(degree(g))) - 1)
  Ll <- N[1]/(2*mean(degree(g)))
  
  # resources:
  # https://rsnippets.blogspot.com/2012/09/plotting-watts-strogatz-model.html
  # http://www.stats.ox.ac.uk/~reinert/talks/swtalkparis.pdf
  # network parameters
  E <- gsize(g)
  D <- edge_density(g)
  C <- transitivity(g, type="global", isolates="NaN")
  L <- average.path.length(g) 
  w <- 1 - abs( (Lr/L) - (C/Cl) )
  SWI <- ( (L - Ll)/(Lr - Ll) ) * ( (C - Cr)/(Cl - Cr) )  
  values <- c(i, a[1], N[i], E, D, C, L, w, SWI)
  container[i, ] <- values
}
container <- as.data.frame(container)

write.csv(container,'ER_spatial_simulation240120b.csv')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Simulation of a BARABASI-ALBERT spatial net using spatial coordinates provided in 
# TIEDENSITY.RData.



# creation: 14-ene-20
# name: simulation_V5.R




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

# load("TIEDENSITY.RData")
coords <- read.csv('coordinates_victor.csv')

# Get an idea of the "tiedensity" surface in which we have to simulate the social net
plot(coords, xlab="x", ylab="y", xlim=xLims, ylim=yLims, asp=1, type="n")
lines(circ, col="blue", lwd=2)
points(coords, pch=16, cex=0.5)
head(coords) # 757   2


# Jan 14, 2020
# # # # Simulation BA social net in tiedensity surface
#parameters
m0 = 3 #initial number of full connected nodes
m = 2 #number of connection desired for each ieration
N = seq(from=100, to=500, by=100) #total number of nodes (iterations) of the net
a = 1.1 #clustering exponent. the Higher the exponent, more difficult the conection at higher distances


g_spatial <- genBA_SpatialNet_custom(coords=coords, m0 = 3, m=2, N=N[1], alpha=a)



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
title("Spatial BA Social Net", cex.main=0.5, col.main="black")
hist(degree(g_spatial))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# jan 25, 2020
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
#parameters
m0 = 3 #initial number of full connected nodes
m = 2 #number of connection desired for each ieration

library(svMisc)
list_container <- vector(mode = "list", length = length(a))
for (j in 1:length(a)) {
  progress(j)
  Sys.sleep(0.01)
  container <- matrix(NA, nrow=length(N), ncol=9)
  colnames(container) <- c("it", "alpha",  "N", "E", "D", "C", "L", "w", "SWI")
  alpha <- a[j]
  for (i in seq_along(1:length(N))) {
    # simulation of the ER spatial network
    g <- genBA_SpatialNet_custom(coords=coords, m0 = 3, m=2, N=N[i], alpha=a)
    
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
    values <- c(i, a[j], N[i], E, D, C, L, w, SWI)
    container[i, ] <- values
  }
  #container <- as.data.frame(container)
  list_container[[j]] <- container
}

super_container <- do.call("rbind", list_container)
write.csv(super_container,'BA_spatial_simulation250120.csv')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# jan 25, 2020
# plotting results for alpha=1.1
super_container <- read.csv("BA_spatial_simulation250120.csv")
super_container <- as.data.frame(super_container)
container <- subset(super_container, alpha == 1.1)
par(mfrow=c(3,2))
plot(container$N, container$E, xlab = "N", ylab = "E - Edges",
     pch = 19, col="red")
plot(container$N, container$D, xlab = "N", ylab = "D - Density",
     pch = 19, col="red")
plot(container$N, container$C, xlab = "N", ylab = "C - Clustering",
     pch = 19, col="red")
plot(container$N, container$L, xlab = "N", ylab = "L - Path Lenth",
     pch = 19, col="red")
plot(container$N, container$w, xlab = "N", ylab = "w",
     pch = 19, col="red")
plot(container$N, container$SWI, xlab = "N", ylab = "SWI",
     pch = 19, col="red")

plot(container$SWI, container$w, xlab = "SWI", ylab = "w",
     pch = 19, col="red")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

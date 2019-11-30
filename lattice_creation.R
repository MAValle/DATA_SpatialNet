# Script to create the lattice of m X m

# Following paper:
# generating and analysinz spatial social networks de Alizadeh


# creation: nov 18, 2019
# name: lattice_creation.R

# Notes:
# 18-nov-19: creation


# start
library(ggplot2)
library(gdata)
library(igraph)

# Lattice parameters
m = 100
number_of_humans = 100
x_axe <- y_axe <- 1:m

# sampling positions
x_positions <- sample(x_axe, number_of_humans, replace = TRUE) # with TRUE coordinates migtht be repeated.
y_positions <- sample(x_axe, number_of_humans, replace = TRUE)

# create dataframe of humans
df <- data.frame(human = 1:number_of_humans, x = x_positions, y = y_positions)

# Plot teh positions
br <- seq(from= 0, to=m, by=1)
ggplot(df, aes(x,y)) + geom_point(col = "red", fill="black", size = 5, shape = 19) +
 scale_x_continuous(name="X", breaks=br) +
 scale_y_continuous(name="Y", breaks=br) +
 theme_bw() 


# # Generating the spatial ER net
# 1. get de distance matrix among humans from the lattice
distances <- dist(df[, c(2,3)] )
max_distance <- max(distances)
min_distance <- min(distances)
if (min_distance == 0) {
  print("Danger!!! We have min distance = 0")
}

# 2. computing the normalizing coefficient C
alpha = 1.5  # alpha mus be greater than 1
C <- (1 - alpha)/ (max_distance^(1-alpha) - min_distance^(1-alpha) )
# 3. plot the pdf
dd <- seq(from = min_distance, to = max_distance, length.out = 200)
pdf <- C*dd^(-alpha)
plot(dd, pdf)



# # Generating the spatial net according to the spatial ER
distances <- as.matrix(distances)
nodes <- colnames(distances)
A <- matrix(NA, ncol = number_of_humans, nrow = number_of_humans) # adjacency matrix
colnames(A) <- rownames(A) <- nodes

for ( i in 1:number_of_humans) {
  nodo = nodes[i]
  otherones <- setdiff(nodes, nodo)
  
  cols_id <- which(colnames(distances) %in% otherones)
  row_id <- which(rownames(distances) == nodo)
  distances_to_node <- distances[row_id, cols_id]
  distances_to_probabilities <- C*distances_to_node^(-alpha)
  random_ <- runif(length(distances_to_probabilities) ) 
  connections <- 1*(random_ <= distances_to_probabilities)
  # put connections to the adjacency matrix
  A[row_id, cols_id] <- connections
}
# we need to make A simmetrical. Just drop the lower part of the matrix
lowerTriangle(A) <- upperTriangle(A, byrow = TRUE)





# # graph the net
g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE, add.colnames = NULL, add.rownames = NA)
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


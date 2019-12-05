# Script to create the Barabasi Albert net 

# dic 12, 2019
# paper: Emergence of scaling in random networks de barabasi y reka
# paper: generating and analysing spatial social network: de alizadeh


# 1. create red completa de m0 nodos con igraph g
# 2. obtener la matriz de adyacencia de g
# 3. seleccionar aleatoriamente un nodo j del set m0 y conectarlo con nodo i con
# probabilidad k_i/sum(k)
# 4. repetir paso 3 hasta que nodo i tenga grado m
# 5. repetir paso 3 y 4 para todo el set de m0+1,...,N nodos


# resources:
# https://github.com/AlxndrMlk/Barabasi-Albert_Network
# https://www.geeksforgeeks.org/barabasi-albert-graph-scale-free-models/

# creation: dic 04, 2019
# name: genBANet_function.R

# Notes:
# 09-dic-19: creation

# inputs:
# m0 = 3 #initial number of full connected nodes
# m = 2 #number of connection desired for each ieration
# N = 200 #total number of nodes (iterations) of the net
# output:
# g: BA network object igraph

genBANet <- function(m0, m, N) {
  library(igraph)
  
  g <- make_full_graph(m0)
  V(g)$name <- 1:m0
  A <- as_adjacency_matrix(g, sparse = FALSE)
  for (j in 1:N) {
    dgg <- colSums(A)
    sum_degree <- sum(dgg)
    the_probabilities <- dgg/sum_degree
    
    # choose a node j from g at random
    conection <- random_node_select(A, the_probabilities, m = m)
    
    # ahora tengo que formar nueva matriz de adyacencia con los datos de conection
    # el nuevo nodo "4" se coenctara a los nodos de conection que tengan valor 1.
    A <- cbind(A, conection)
    A <- rbind(A, c(conection, 0 ) )
    fin <- j + m0
    colnames(A) <- rownames(A) <- 1:fin
  }
  gg <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE, add.colnames = NULL, add.rownames = NA)
  
  return(gg)
}
# example
# g <- genBANet (m0 = 3, m = 2, N = 200)
# plot
# par(mar = c(1.5, 1.5, 1.5, 1.5))
# plot(g, 
#      edge.arrow.size =.3, 
#      edge.curved = 0,
#      vertex.color = "yellow", 
#      vertex.frame.color="#555555",
#      #vertex.label = V(mst_g)$name, 
#      vertex.label.color = "black",
#      vertex.label.cex=.6,
#      vertex.size = 7)
# title("Spatial ER Social Net", cex.main=0.5, col.main="black")

# Funcion necesaria para generar seleccion aleatoria de nodos y 
# necesaria para la funcion genBANet
# input:
#   A: adjacency matrix
#   the_probabilities: probabilidades de cada nodo 
#   m 
# output
#   conection: vector que indica que nodos se conectan al nuevo nodo.
random_node_select <- function(A, the_probabilities, m) {
  dg = 0
  conection <- numeric(length = ncol(A) ) # salida importante 
  sampled_nodes <- vector()
  nodes_to_sample <- as.numeric(colnames(A))
  while (dg < m) {
    node_sample <- sample(nodes_to_sample, 1) 
    p <- the_probabilities[node_sample]
    v <- runif(1)
    if ( p > v ) {
      conection[node_sample] <- 1
      dg <- dg + 1
      sampled_nodes <- c(sampled_nodes, node_sample) # lista de nodos sampleados
      nodes_to_sample <- nodes_to_sample[-node_sample] # ya no se puede samplear nuevamente este nodo
    } 
  }
  return(conection)
}
# ejemplo:
# random_node_select(A, the_probabilities)





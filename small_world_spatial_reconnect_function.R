# Script to create a small world spatial network

# dic 27, 2019
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

# creation: dic 27, 2019
# name: small_world_spatial_reconnect_function.R

# Notes:
# 27-dic-19: creation
# Inputs:
# g: la red o grilla
# C = constance de la funcion de distribucion de distancias
# distances: matrix of distances among vertexs
# vtx = nombre del nodo de g para reconexion
# alpha = fixed cluster exponent of the distance pdf
# ord = order of the adyacency (1 = to neighborhood 1)
# Output:
# new object g reconected for node vtx
small_world_spatial_reconnect <- function(g, C, distances, vtx, alpha = 1.5, ord = 1) {
  # # # # # Get all neighborhoods of a node in a vector
  # Ojo que el primer valor es el mismo nodo i.
  if (ord == 2) {
    ady1 <- as.numeric(unlist(ego(grilla, order = 1, vtx)))
    ady2 <- as.numeric(unlist(ego(grilla, order = 2, vtx)))
    ady <- c(ady1[1], setdiff(ady2, ady1))
  } else {
    ady <- as.numeric(unlist(ego(grilla, order = ord, vtx)))  
  }
  # ady <- ady[-1]
  #adjacent_vertices(grilla, 12 )
  # # # # # Get all neighborhoods of a node in a vector
  
  # select the k nodes to be sampled except ady
  num = length(ady)-1
  knodes <- sample(setdiff(V(g)$name, ady), num, replace=F)
  
  # start
  for (i in 1:num) {
    j <- i + 1
    u <- runif(1)
    # calculo de p segun la distancia
    distt <- distances[ady[1], knodes[i]]
    p <- get_powerLaw_prob(C = C, alpha = alpha, d = distt)
    if ( u <= p) {
      # add the edge
      g <- add_edges(g, c(ady[1], knodes[i] ) )
      # remove the old edge
      ei <- get.edge.ids(g, c(ady[1], ady[j]) )
      #E(grilla)[ei]
      g <- delete_edges(g, ei)
    } 
  }
  return(g)
  
}
# Example
# grilla <- small_world_spatial_reconnect(g = grilla, C=C, distances = distances, vtx = 1, alpha = 1.5, ord=1) 

# Ver simulation_V3.R para ver el ejemplo completo.




# esto solo es para smal network (not spatial)
# FUNCTION TO RECONECT ACCORDING TO THE SMALL WORLD NETWORK # # # # # # # # # # # # # # # # # # #
# inputs:
# g: la red o grilla
# p = probabilidad de reconexion
# vtx = nombre del nodo de g para reconexion
# ord = order of the adyacency (1 = to neighborhood 1)
# Output:
# new object g reconected for node vtx
small_world_reconnect <- function(g, p, vtx, ord = 1) {
  # # # # # Get all neighborhoods of a node in a vector
  # Ojo que el primer valor es el mismo nodo i.
  if (ord == 2) {
    ady1 <- as.numeric(unlist(ego(grilla, order = 1, vtx)))
    ady2 <- as.numeric(unlist(ego(grilla, order = 2, vtx)))
    ady <- c(ady1[1], setdiff(ady2, ady1))
  } else {
    ady <- as.numeric(unlist(ego(grilla, order = ord, vtx)))  
  }
  # ady <- ady[-1]
  #adjacent_vertices(grilla, 12 )
  # # # # # Get all neighborhoods of a node in a vector
  
  # select the k nodes to be sampled except ady
  num = length(ady)-1
  knodes <- sample(setdiff(V(g)$name, ady), num, replace=F)
  
  # start
  for (i in 1:num) {
    j <- i + 1
    u <- runif(1)
    if ( u <= p) {
      # add the edge
      g <- add_edges(g, c(ady[1], knodes[i] ) )
      # remove the old edge
      ei <- get.edge.ids(g, c(ady[1], ady[j]) )
      #E(grilla)[ei]
      g <- delete_edges(g, ei)
    } 
  }
  return(g)
  
}
# end function
# example
# grilla <- small_world_reconnect(g = grilla, p=0.8, vtx = 1) 
# Nota: 
# Si la grilla conecta los nodos con vecinos de orden 1, entonces
# la opcion ord=2 no se debe utilizar. De hecho esta alternativa
# no se debe usar porque aun no esta programada. Si eventualmente
# tenemos una grilla en que un nodo se conecta a otro nodo saltandose
# el mas cercano, tendriamos vecinos cercanos de orden 2.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 






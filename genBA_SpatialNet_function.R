# create a spatial BA network
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



# spatial net function generator based on BA network
# Inputs
#m0 #initial number of full connected nodes
# number of connection desired for each ieration
# total number of nodes (iterations) of the net
# alpha : distance decay exponent
# size_lattice: size of one side of teh lattice
# Outputs:
# igraph object with the spatial BA network
genBA_SpatialNet <- function(m0, m, N, alpha, size_lattice) {
  # lattice creation
  df <- lattice_creation(m = size_lattice, number_of_humans = N) #aqui se incluyen los nodos iniciales de m0 (los primero m0)
  distances <- dist(df[, c(2,3)] )
  max_distance <- max(distances)
  min_distance <- min(distances)
  c1 <- max_distance^(1-alpha) - min_distance^(1-alpha) 
  
  g <- make_full_graph(m0)
  V(g)$name <- 1:m0
  A <- as_adjacency_matrix(g, sparse = FALSE)
  
  # The next is repeated N-m0 times
  for (j in seq(from = m0+1, to = N, by=1)) {
    dgg <- colSums(A) # degree de los nodos
    max_degree <- max(colSums(A))
    min_degree <- min(colSums(A))
    c2 <- ifelse(max_degree^2 - min_degree^2 == 0, 1, max_degree^2 - min_degree^2)
    
    # calculo de la constante de la integral
    C <- 2*(1-alpha)/(c1*c2)
    
    # calculo de las probabilidades de conecciones
    # para esto: primero elegir el nodo (no en el set de m0)
    # segundo calcular la distancia entre ese nodo y todos los demas nodos en A.
    #j va desde m0+1 hasta N
    distances <- as.matrix(dist(df[c(1:j), c(2,3)]))
    distances <- distances[,ncol(distances)]
    distances <- distances[-length(distances)]
    distances_ <- distances^(1-alpha)
    the_probabilities <- C*dgg*distances_
    
    # choose a node j from g at random
    conection <- random_node_select(A, the_probabilities, m = m)
    
    # ahora tengo que formar nueva matriz de adyacencia con los datos de conection
    # el nuevo nodo "4" se coenctara a los nodos de conection que tengan valor 1.
    A <- cbind(A, conection)
    A <- rbind(A, c(conection, 0 ) )
    colnames(A) <- rownames(A) <- 1:j
  }
  
  g_spatial <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE, add.colnames = NULL, add.rownames = NA)
  return(g_spatial)
}
# example
# g_spatial <- genBA_SpatialNet(m0 = 3, m=2, N=200, alpha=1.1, size_lattice = 100)
# # plot the spatial net from adjacency matrix A
# par(mar = c(1.5, 1.5, 1.5, 1.5))
# plot(g_spatial,
#      edge.arrow.size =.3,
#      edge.curved = 0,
#      vertex.color = "yellow",
#      vertex.frame.color="#555555",
#      #vertex.label = V(mst_g)$name,
#      vertex.label.color = "black",
#      vertex.label.cex=.6,
#      vertex.size = 7)
# title("Spatial BA Net", cex.main=0.5, col.main="black")
# hist(degree(g_spatial))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 14-ene-20
# The same than genBA_SpatialNet, but we allow for a custom surface.
# Inputs:
# coords : matrix of two columns indicating x and y position of the surface
genBA_SpatialNet_custom <- function(coords, m0, m, N, alpha, size_lattice) {
  # lattice creation
  #df <- put_humanoids(coords = coords, N=N) #aqui se incluyen los nodos iniciales de m0 (los primero m0)
  df <- put_humanoids2(coords = coords, N=N) 
  distances <- dist(df[, c(2,3)] )
  max_distance <- max(distances)
  min_distance <- min(distances)
  c1 <- max_distance^(1-alpha) - min_distance^(1-alpha) 
  
  g <- make_full_graph(m0)
  V(g)$name <- 1:m0
  A <- as_adjacency_matrix(g, sparse = FALSE)
  
  # The next is repeated N-m0 times
  for (j in seq(from = m0+1, to = N, by=1)) {
    dgg <- colSums(A) # degree de los nodos
    max_degree <- max(colSums(A))
    min_degree <- min(colSums(A))
    c2 <- ifelse(max_degree^2 - min_degree^2 == 0, 1, max_degree^2 - min_degree^2)
    
    # calculo de la constante de la integral
    C <- 2*(1-alpha)/(c1*c2)
    
    # calculo de las probabilidades de conecciones
    # para esto: primero elegir el nodo (no en el set de m0)
    # segundo calcular la distancia entre ese nodo y todos los demas nodos en A.
    #j va desde m0+1 hasta N
    distances <- as.matrix(dist(df[c(1:j), c(2,3)]))
    distances <- distances[,ncol(distances)]
    distances <- distances[-length(distances)]
    distances_ <- distances^(1-alpha)
    the_probabilities <- C*dgg*distances_
    
    # choose a node j from g at random
    conection <- random_node_select(A, the_probabilities, m = m)
    
    # ahora tengo que formar nueva matriz de adyacencia con los datos de conection
    # el nuevo nodo "4" se coenctara a los nodos de conection que tengan valor 1.
    A <- cbind(A, conection)
    A <- rbind(A, c(conection, 0 ) )
    colnames(A) <- rownames(A) <- 1:j
  }
  
  g_spatial <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE, add.colnames = NULL, add.rownames = NA)
  return(g_spatial)
}
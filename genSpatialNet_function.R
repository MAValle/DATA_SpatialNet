# Script to create the spatial net according to the spatial ER

# Following paper:
# generating and analysingspatial social networks de Alizadeh


# creation: nov 19, 2019
# name: genSpatialNet_function.R

# Notes:
# 18-nov-19: creation
# 13-mar-20: cuando el numero de nodos es mayor a 200, la funcion demora demasiado en ejecutarse
# debido al comando dist. Agregamos la opcion method = "fast" para calcular las distancias
# un poco mas rapido con https://rdrr.io/cran/Rfast/man/Dist.html.
# Haciendo pruebas, no se bien que hace Dist, porque las distancias no coinciden con las de dist.

# inputs:
#   * df: dataframe coming from lattice_creTION FUNCTION 
#   * numberof_humans
#   * alpha constant for power law pdf
#   * C constante for power law distributiomn
genSpatialNet <- function(df, number_of_humans, C, alpha, method="normal") {
  library(igraph)
  library(Rfast)
  message("Now computing distance matrix.....")
  if (method == "fast") {
    #distances <- dist(df[, c(2,3)] )
    distances1 <- Dist(df[, c(2,3)] , method = "euclidean", square = TRUE, vector = FALSE)
    distances1 <- as.matrix( dist(distances) )
  } else {
    distances2 <- dist(df[, c(2,3)] )
    distances2 <- as.matrix(distances)
  }
  

  
  nodes <- colnames(distances)
  A <- matrix(NA, ncol = number_of_humans, nrow = number_of_humans) # adjacency matrix
  colnames(A) <- rownames(A) <- nodes
  
  # # Generating the spatial net according to the spatial ER
  for ( i in 1:number_of_humans) {
    print(paste("Iteration ", i, " of ", number_of_humans))
    nodo = nodes[i]
    otherones <- setdiff(nodes, nodo)
    cols_id <- which(colnames(distances) %in% otherones)
    row_id <- which(rownames(distances) == nodo)
    distances_to_node <- distances[row_id, cols_id]
    
    distances_to_probabilities <- get_powerLaw_prob(C = C, alpha = alpha, d = distances_to_node)
    distances_to_probabilities[!is.finite(distances_to_probabilities)] <- 1
    
    random_ <- runif(length(distances_to_probabilities) ) 
    connections <- 1*(random_ <= distances_to_probabilities)
    # put connections to the adjacency matrix
    A[row_id, cols_id] <- connections
  }
  # we need to make A simmetrical. Just drop the lower part of the matrix
  lowerTriangle(A) <- upperTriangle(A, byrow = TRUE)

  # # the resulting net
  g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE, add.colnames = NULL, add.rownames = NA)
  
  return(g)
}
# example
# g <- genSpatialNet(df = df, number_of_humans = 100, C = C, alpha = 1.5)
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






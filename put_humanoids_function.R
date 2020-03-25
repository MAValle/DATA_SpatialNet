# put humans or animals in a custom grid or semicircle.
# jan 14, 2020
# paper: Emergence of scaling in random networks de barabasi y reka
# paper: generating and analysing spatial social network: de alizadeh


# resources:
# https://github.com/AlxndrMlk/Barabasi-Albert_Network
# https://www.geeksforgeeks.org/barabasi-albert-graph-scale-free-models/



# spatial net function generator based on BA network
# Inputs
# coords = matrix of two columns indicating x and y position of the surface
# N = number of humanoids to put on the surface

# Outputs:
# df: fata frame with 3 columns, human id, x and y.
put_humanoids <- function(coords, N) {
  dpli <- 1
  it = 1
  while (length(dpli) > 0) { # mientras hayan duplicados 
    cat("trying :", it, '\n')
    idx <- sample(1:nrow(coords), N, replace = TRUE)
    idy <- sample(1:nrow(coords), N, replace = TRUE)
    x <- coords[idx,1]
    y <- coords[idy,1]
    human <- 1:N
    df <- data.frame(human = human, x=x, y=y)
    # density:
    # N/nrow(coords)
    dpli <- which(duplicated(df[,c(2,3)])) # si da mayor que cero, tenemos coordenadas repetidas.
    it <- it +1
    #print(it)
  }
  # trasladamso todas las coordenadas originales al plano x-y positivo.
  df$x <- df$x + abs(min(df$x))
  df$y <- df$y + abs(min(df$y))
  return(df)
}

# example
# df <- put_humanoids(coords = coords, N=300)

# JAN 24, 2020
# another way to sample coordenates and put humanoids
put_humanoids2 <- function(coords, N) {
  df <- matrix(NA, ncol=3, nrow=N)
  coor2 <- as.matrix(coords)
  for (i in 1:N){
    id <- sample(1:nrow(coor2), 1, replace = FALSE)
    df[i, ] <- (coor2[id,] )
    coor2 <- coor2[-id,] 
  }
  colnames(df) <- c("human", "x", "y")
  df <- as.data.frame(df)
  #which(duplicated(df[,c(2,3)]))
  # trasladamso todas las coordenadas originales al plano x-y positivo.
  df$x <- df$x + abs(min(df$x))
  df$y <- df$y + abs(min(df$y))
  return(df)
}
#example
#df <- put_humanoids2(coords = coords, N=500)

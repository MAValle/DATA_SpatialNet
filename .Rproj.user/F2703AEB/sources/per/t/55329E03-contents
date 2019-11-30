# Script to create the spatial ER distribution 

# Following paper:
# generating and analysingspatial social networks de Alizadeh


# creation: nov 19, 2019
# name: genSpatialDistribution_function.R

# Notes:
# 18-nov-19: creation

# Inputs: df: dataframe coming from lattice_creTION FUNCTION 
# with spatial positions of humans.
# Input: alpha   = numeric value of teh distance decay exponent. alpha mus be greater than 1
# Output: constante value of the pdf
genStatialDistribution <-function(df, alpha) {
  # # Generating the spatial ER net
  # 1. get de distance matrix among humans from the lattice
  distances <- dist(df[, c(2,3)] )
  max_distance <- max(distances)
  min_distance <- min(distances)
  # if (min_distance == 0) {
  #   #print("Danger!!! We have min distance = 0. That is not allowed :(")
  #   stop("Danger!!! We have min distance = 0. That is not allowed :(")
  # }
  
  # 2. computing the normalizing coefficient C
  C <- (1 - alpha)/ (max_distance^(1-alpha) - min_distance^(1-alpha) )
  return(C)
  
  # dd <- seq(from = min_distance, to = max_distance, length.out = 200)
  # pdf <- C*dd^(-alpha)
  # plot(dd, pdf)
}
# example
#C <- genStatialDistribution(df = df, alpha = 1.5)







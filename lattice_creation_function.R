# Script to create the lattice of m X m

# Following paper:
# generating and analysingspatial social networks de Alizadeh


# creation: nov 18, 2019
# name: lattice_creation_function.R

# Notes:
# 18-nov-19: creation


# start



# function to create a lattice
# input: * m = size of teh lattice (m X m)
#        * number_of_humans
# Output: dataframe with 3 columns: humans, x position, y position
lattice_creation <- function(m, number_of_humans) {
  x_axe <- y_axe <- 1:m
  back_to_generate <- 1
  
  while ( back_to_generate == 1) { # en caso que hayan coordenadas repetidas, se genera otra grilla.
    # sampling positions
    x_positions <- sample(x_axe, number_of_humans, replace = TRUE) # with TRUE coordinates migtht be repeated.
    y_positions <- sample(x_axe, number_of_humans, replace = TRUE)
    
    # create dataframe of humans
    df <- data.frame(human = 1:number_of_humans, x = x_positions, y = y_positions)
    
    chec <- duplicated(df[,c(2,3)])
    if ( sum(chec) != 0) {
      back_to_generate <- 1
    } else {
      back_to_generate <- 0
    }
  }
  
  return(df)
  
}
# example
# df <- lattice_creation(m = 100, number_of_humans = 100)
# # Plot teh positions
# br <- seq(from= 0, to=m, by=1)
# ggplot(df, aes(x,y)) + geom_point(col = "red", fill="black", size = 5, shape = 19) +
#   scale_x_continuous(name="X", breaks=br) +
#   scale_y_continuous(name="Y", breaks=br) +
#   theme_bw() 



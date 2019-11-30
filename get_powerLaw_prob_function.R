# Function to generate values of probabilities according to the power law distribution
# p(d) = C * d ^ (-alpha)

# inputs:
# C constant 
# alpha distance decay exponent
# output: value of probability
get_powerLaw_prob <- function(C, alpha, d) {
  p = C*d^(-alpha)
  return(p)
}
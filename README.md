### Project Data_SpatialNet

Script for playing with humanoids in a spatial net.



#### Some simulations

**simulation_v1.R**: simulation ER spatial net.

**simulation_v2.R**: simulation Barabasi-Albert  spatial net.

**simulation_v3.R**: simulation with small world spatial nets.

**simulation_v4.R**: simulation of a ER spatial net using semicircle area provided in TIEDENSITY.RData, dataframe coords.

**simulation_v5.R**: simulation of a Barabasi-Albert  spatial net using semicircle area provided in TIEDENSITY.RData, dataframe coords.

**simulation_v6.R**: simulation of a SW  spatial net using semicircle area provided in TIEDENSITY.RData, dataframe coords.



**lattice_diffusion_v1.R**: we simulate a diffusion of a message through a network based on a lattice. The lattice is just the sPATIAL STRUCTURE for which the network is build. This network could be based in a ER o small network type. We want to study the diffusion of a message on these kind of network and see the possibility of percolation. 



####Functions:

**lattice_creation.R**: 

**lattice_creation_function.R**: Creates a lattice and put humanoids inside it. 

**gen_powerLaw_prob_function.R**: Computes a probability following teh power law distribution ffor a given distance,

**gen_SpatialNet_function.R**: Develop a ER spatial network simulation.

**genSpatialDistribution_function.R**: 

**genBA_SpatialNet_function.R**: Develop a BA spatial network simulation.

**small_world_spatial_reconnect_function.R**: develop a reconnection of a lattice to create a small world spatial network based on a probability distribution of vertex connections of distance.



####Environments:





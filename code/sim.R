# Reproduce the simulation of the paper 
# C:\Users\robin\AppData\Local\R\win-library\4.4\copula\demo

# Libraries ---------------------------------------------------------------
# library(copula)
# library(VineCopula)

# Global parameters --------------------------------------------------------------
n = 5000

# Simulation - Gumbel -----------------------------------------------------
#Note: There are some restrictions on theta depending on the generator!
theta_1 = 2
theta_2 = 10
# Define nested structure
c_gumbel = copula::onacopula(family = "G", nacStructure = C(theta_1, 3, C(theta_2, c(1, 2))))
# Simulate data
u_gumbel = copula::rnacopula(n, c_gumbel)
copula::splom2(u_gumbel, cex = 0.4)

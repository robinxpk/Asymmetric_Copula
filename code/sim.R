# Reproduce the simulation of the paper 

# Libraries ---------------------------------------------------------------
# library(copula)

# Parameters --------------------------------------------------------------
n = 1000




# Simulation - Gumbel -----------------------------------------------------
gc_theta = 2
gc = copula::gumbelCopula(gc_theta, dim = 2)

u = copula::rCopula(n, gc)
plot(u)



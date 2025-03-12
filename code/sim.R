# Simulation Structure ----------------------------------------------------

# Part a) and b)
# Part a) "Copula": Focuses on the copula and its estimation
# Part b) "GAM": Deals with GAM in Copula, i.e. including covaraites into the copula and how covariates affect the dependence structure

# xxxxxxxxxxxxxx a) Copula fitting xxxxxxxxxxxxxxxxxxxxx

# >> Simulation: 
# Repeat B times: 
#   For each sample size (n), each copula family, each dependence structure (AC, NAC, Vine), tau:
#     Draw random sample  
#   For each sample, fit AC, NAC and Vine copula and evaluate
# (Note: I only consider positive correlation bc 1. data only contains positive and 2. negative correlation can be modelled wlog by 1-u)

# -- Considered Copula Families
#   Gumbel, Clayton, Joe

# -- Considered Copula Structures
#   AC (3: symmetric Gumbel, Clayton, Joe) 
#   NAC (3: M4, M5, M6) 
#   Vines (3 + ?):
#     (3) Same bivariate dependence: Gumbel, Clayton, Joe ---> Relevant so we can compare with NACs 
#     (?) Mixed copula families. Might be interesting to look at a frequently often encountered combination of copulas
#       Just to see if it is identified correctly

# >> Evaluation
# 1) Retrieval of correct structure
#   Fit all 3 copula types (using their implemented selection method)
#   Choose among these 3 dependence structures using AIC
#   How of (%) is the correct copula selected 

# 2) MSE and Biases
#   Issue: MSE only defined between the true and estimated structure
#
#   Using the true parameters and the copula fitted in step 1, calc MSE for: 
#   tau, copula param, tail dependence
#     (Added tail dep. bc it also should be a non-linear function of tau / theta and thus be biased?)
#   Interesting: 
#   - How does sample size affect MSE (Bias and Variance)?
#   - How reliable are small sample estimates (variance for small samples)?
#   - Is there a bias in estimates due to Jensen's inequality
#
# 2.1) MSE in those cases, where the correct structure is selected
# 2.2) MSE for all cases

# 3) Performance (under misspecification)
#   - Compare each of the 3 fitted copulas using the KL (allows to compare whole distribution)
# TODO: Grimaldi: Reproduce behavior of AC tau if NAC is true; i.e. is tau between true taus, but has some bias? 
#     His setup: outer_tau: seq(from = 0.05, to = 0.95, by = 0.1), inner_tau: seq(from = 0.35, to = 0.95, by = 0.1)
#           Then: (tau_symm - tau_outer) / (tau_inner - tau_outer) 
#           Result: tau_symm closer to tau_outer and takes on values ~0.3 * tau_inner -> underestimate dependece structure with AC
# TODO: Can I reproduce this for the vine case? -> Anything dependence underestimated? 

# xxxxxxxxxxxxxx b) GAM            xxxxxxxxxxxxxxxxxxxxx

# -> Modelling dep. structure 
# ! Model multiple copulas i.e. simulate multiple stations

# >> Simulation
# Same as above, but let tau be dependent on a lin. pred.

# -- Models
#   intercept only (no covariate) ->  Intercept only and zero-param models are equivalent: Constant tau between stations
#   zero param                    ->  But zero-param is part of estimation where we assume the GAM to have a covariate
#                           These two cases are my consideration of "missspecification" of the linear model. After all, take my applied result "with grain of salt" (See README)
#   non-zero param
# NOTE: If the underlying true model is more complex and we estimate a simpler model, usual bias considerations hold

# -- Response function
#   gamCopula: Fisher z-Transform by default
#   Does package allow to change it? How does estimate change?

# >> Evaluation
# 1) Fit multiple copulas (again)

# 2) MSE for beta estimates
#   For HAC: I would first fit HAC and then apply GLM to whatever I have fitted. Is this even valid? 
#     It is more of an idea based on a paper I read..(Li - Improving forecasting...) 
#     ! Alternative: I just show that NACs are NOT feasible in the case of their assumption violation
#     Because their assumption is actually violated in our data, we only proceed with the Vine GAMs

# Libraries ---------------------------------------------------------------
library(ggplot2)
# library(HAC)
# library(gamCopula)
library(doParallel)
source("functions.R")
# TODO: How do vines behave if tree structure not necessarily true
# as.data.frame(res |> dplyr::filter(river == "SimilarAndLow"))
# run_one_nac(seed = 1, n = 1000, taus = taus, cop = "Clayton") # Think there are errors if I modify the current script while running parallel
# run_one_vine(seed = 343, n = 15, taus = taus) 

# Parameters --------------------------------------------------------------
# Sample size of simulated sample
# 500 and 5000 are used in Vatter's paper and considered "small and large", 
# but for a way more complex Vine and GAM... Let's see how sample sizes behave in my simpler case
# sample_sizes = c(15, 30, 50, 100, 500, 5000) 
# 15, 30, 50 are sample sizes similar to mine
# 1000 as large sample size (sample size of 5k takes soooo long...)
sample_sizes = c(15, 30, 50, 1000)

# Observed dependencies
taus = list(
  # Requirement: Order taus in INCREASING order when adding new one (Ensures NAC sim works)
  # How simulations use taus:
  # NACs: tau_outer = mean(tau1, tau2) and tau_inner = tau3
  # Vine: c("1-3", "2-3", "1-2") with: 1,3|2 - 2,3 - 1,2, i.e. 1 - 2 - 3
  "Donau" = c(0.1729242, 0.3210276, 0.8122395),
  "Isar" = c(0.1354267, 0.3774687, 0.6985702),
  # For the river correlations, NACs are not even that bad according to KLD
  # Thus, I would like to model one challenging dependence strucutre.
  # Because I do not consider negative correlation wlog, the onlx case of 3 unique dependence structures is relevant
  # That is:
  # a) Two similar dependence structure, but both HIGH, so only one of them can be nested. 
  #   But outer cannot model two different degrees of dependence between the nested variables
  "LowHighHigh" = c(0.1, 0.8, 0.8),
  "LowMedHigh" = c(0.1, 0.5, 0.9)
)

# Cores used during parallelization
n_cores = parallel::detectCores() - 2

# a) Copula ---------------------------------------------------------------
# NACs ----

# Number of simulations per setting
B = 3000

# Copula families considered during the simulation
copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5)

# Start simulation
cluster <- parallel::makeCluster(n_cores, outfile = "")
doParallel::registerDoParallel(cluster)

# Ensure cluster stops after execution
on.exit(parallel::stopCluster(cluster))

for (n in sample_sizes){
  for (cop in names(copula_families)){
      # Run parallelized
      res <- foreach(
        seed = 1:B
        # .packages = pkg
        # .export = ls()
      ) %dopar% {
        # Simulate
        tryCatch(
          run_one_nac(
            seed = seed,
            n = n,
            cop = cop,
            taus = taus
          ),
          error = function(e) message(e))
      }

      res = res |> dplyr::bind_rows(.id = "seed")
      res$seed = as.numeric(res$seed)
      # browser()
      attr(res, "n") = n
      attr(res, "cop") = cop
      attr(res, "dep") = "nac"
      
      attr(res, "B") = B
      attr(res, "sample sizes") = sample_sizes
      attr(res, "copula families") = copula_families
      
      # Save simulation results in file
      filename = paste("../data/simulation/simulation_n", n, "_cop", cop, "_depnac.Rdata", sep = "")
      save(res, file = filename)
  }
}



# Vines ----
# For drawing vines, we have 3^3 possible vine structures. Thus, draw 27000 so that every vine structure has 1k (on average bc I draw them randomly)
B = 27 * 1000
# Also, the copula families have a different encoding
copula_families = list("Clayton" = 3, "Gumbel" = 4, "Frank" = 5)

cluster <- parallel::makeCluster(n_cores, outfile = "")
doParallel::registerDoParallel(cluster)

# Ensure cluster stops after execution
on.exit(parallel::stopCluster(cluster))  

for (n in sample_sizes){
    # Run parallelized
    res <- foreach(
      seed = 1:B
    ) %dopar% {
      tryCatch(
      run_one_vine(
        seed = seed,
        n = n,
        taus = taus
      ),
        error = function(e) NULL)
    }

    res = res |> dplyr::bind_rows(.id = "seed")
    attr(res, "n") = n
    attr(res, "dep") = "vine"

    attr(res, "B") = B
    attr(res, "sample sizes") = sample_sizes
    attr(res, "copula families") = copula_families
    
    # Save simulation results in file
    filename = paste("../data/simulation/simulation_n", n, "_depvine.Rdata", sep = "")
    save(res, file = filename)
}

# # # b) GAM ------------------------------------------------------------------
# # # Here, I only care how the gamVine packages performs. NACs are not relevant for my use case so the coefficient estimation for a non-relevant
# # #   structure is pointless anyway. Plus, HAC so unreliable anyway, who cares. Really. Fuck that package and all the other inconsistent copula packages

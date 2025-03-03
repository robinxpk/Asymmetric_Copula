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
# Parameters --------------------------------------------------------------
# Number of simulations per setting
B = 1000

# Sample size of simulated sample
sample_sizes = c(15, 30, 50, 100, 500, 5000) # 500 and 5000 are used in Vatter's paper and considered "small and large", but for a way more complex Vine and GAM... Let's see how sample sizes behave in my simpler case

# Copula families considered during the simulation
copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5)

# Observed dependencies
taus = list(
  "Donau" = c(0.1729242, 0.3210276, 0.8122395),
  "Isar" = c(0.1354267, 0.3774687, 0.6985702)
)

# Cores used during parallelization
n_cores = parallel::detectCores() - 2

# a) Copula ---------------------------------------------------------------
# NACs ----
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
          error = function(e) NULL)
      }

      res = res |> dplyr::bind_rows(.id = "seed")
      attr(res, "n") = n
      attr(res, "cop") = cop
      attr(res, "dep") = "nac"
      # Save simulation results in file
      filename = paste("../data/simulation/simulation_n", n, "_cop", cop, "_depnac.Rdata", sep = "")
      save(res, file = filename)
  }
}

# Vines ----
# For drawing vines, we have 3^3 possible vine structures. Thus, draw 27000 so that every vine structure has 1k (on average bc I draw them randomly)
B = 27000
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
      # .packages = pkg
      # .export = ls()
    ) %dopar% {
      # Simulate
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
    # Save simulation results in file
    filename = paste("../data/simulation/simulation_n", n, "_depvine.Rdata", sep = "")
    save(res, file = filename)
}

# b) GAM ------------------------------------------------------------------
# Here, I only care how the gamVine packages performs. NACs are not relevant for my use case so the coefficient estimation for a non-relevant
#   structure is pointless anyway. Plus, HAC so unreliable anyway, who cares. Really. Fuck that package and all the other inconsistent copula packages




# Simulation Analysis -----------------------------------------------------
# If mostly Vines are selected, it is interested how these look and how close they are to the actual, true model
# My simulation mostly goes to show how well these packages are applicable bzw. their implemented methods.
# Thus, my simulation is more of different models AND different methods which concludes to simulation to compare package performance
#     is okay, since I will apply these package to the data and not implement anything myself here

# Group the analysis by the true underlyding structure (ac, nac, vine). Then, show for each n, (and over the true copula family?) how 

# Analysis NAC ------------------------------------------------------------
all_res = read_dep_files(true_dep = "nac", in_dir = "../data/simulation/") 
# Remove attributes that are no longer applicable
for (a in c("n", "true dep", "true cop")) attr(all_res, a) = NULL

# Sanity check
nrow(all_res) # For each dependence structure: 1000 seeds * 6 different n * 3 copulas = 18000
table(all_res$n) # Should be 3000 each
table(all_res$cop) # Should be 6000 each
table(all_res$true_tau_inner) # Distribution of selected correlation; Should be equivalent to the number of different rivers I consider

# Checking for any NaNs
any(is.na(all_res))
# Investiage:
all_res |> dplyr::filter(is.na(nac_kl))

# xxxxxxxxxxxxxx a) Copula fitting xxxxxxxxxxxxxxxxxxxxx
# >> Evaluation
# Start with 3. point so I can adjust data later on...
# 3) KL empirical density plot to evaluate how the other copula models perform under misspecification
clayton_kl = klplots(all_res, "Clayton", scales = "free") 
plot(clayton_kl)
gumbel_kl = klplots(all_res, "Gumbel", scales = "free")
plot(gumbel_kl)
frank_kl = klplots(all_res, "Frank", scales = "free")
plot(frank_kl)

# Relevant for us are mainly the small sample cases where n < 100
clayton_kl = klplots(all_res |> dplyr::filter(n < 100), "Clayton", scales = "fixed")
plot(clayton_kl)
gumbel_kl = klplots(all_res |> dplyr::filter(n < 100), "Gumbel", scales = "fixed")
plot(gumbel_kl)
frank_kl = klplots(all_res |> dplyr::filter(n < 100), "Frank", scales = "fixed")
plot(frank_kl)
# Does the multimodality come from the different tau values (In AC)
# Vine and NACs perform pretty much identical for all sample sizes
# Reason: Vines are just more flexible such that NACs are somewhat a subset
# Important for us: Fitting Vine is reasonable for underlying structure being NAC

# 1) Retrieval of correct structure
#   Using AIC among these 3 dependence structures, how of (%) is the correct copula selected 
all_res = all_res |> 
  # Row wise, check which AIC is minimal
  dplyr::rowwise() |> 
  dplyr::mutate(
    # Minimal AIC in Row (i.e. in simulated sample)
    min_aic = min(ac_aic, nac_aic, vine_aic),
    # Choice according to AIC
    choice_aic = dplyr::case_when(
      ac_aic == min_aic ~ "ac",
      nac_aic == min_aic ~ "nac",
      vine_aic == min_aic ~ "vine"
    ),
    choice_correct = choice_aic == dep
  ) |>
  dplyr::ungroup()

# Fraction correctly selected copula structure
mean(all_res$choice_correct) 
# Marginal distribution of missclassifications
table(all_res$choice_aic)
# Behavior through sample size and copula
table(all_res$choice_aic, all_res$n, all_res$cop)

# Visualization of the above:
# How does AIC compare in those cases where vine was selected over NAC
all_res |>
  dplyr::filter(choice_aic == "vine") |>
  dplyr::mutate(aic_ratio =  vine_aic / nac_aic) |> 
  ggplot(aes(y = aic_ratio)) + 
  geom_boxplot()
# Judging by this plot, there are some times where the ratio between the AICs is quite large
# That is, the AIC for vines is up to 1.6 times larger bzw. smaller than the AIC for NACs. That is quite a lot.
# When do these cases occure? How do  they change depending on sample size and copula?
all_res |>
  dplyr::filter(choice_aic == "vine") |>
  dplyr::mutate(aic_ratio =  vine_aic / nac_aic) |> 
  ggplot(aes(y = aic_ratio, x = cop)) + 
  geom_boxplot() +
  facet_wrap(~ n)
# The AIC ratio mainly depends on the sample size and less on the copula family
# For our data, we move around 50 obs per stations, where the discrapency between the AICs is still sometimes up to a factor of 1.2
# That is, even we have a potentially large difference between NAC and Vine AIC, AIC selection might end up selecting the more complex structure
# However, as we have seen from the KL, both alternatives behave very similar in terms of approaching the true distribution

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
# 2) MSE in those cases, where the correct structure is selected
# To remove the variance in the true tau bzw. thetas, we consider the MSE for each true tau separately, i.e. for each river separately
# Note: Underlying is NAC, that is, HAC-package used --> theta was estimated
crt = all_res |> 
  dplyr::filter(choice_correct == TRUE) |>
  dplyr::rowwise() |> 
  dplyr::mutate(
    theta_inner_diff = fit_theta_inner - true_theta_inner,
    theta_outer_diff = fit_theta_outer - true_theta_outer,
    tau_inner_diff = fit_tau_inner - true_tau_inner,
    tau_outer_diff = fit_tau_outer - true_tau_outer
  ) |> 
  dplyr::ungroup() |> 
  # TODO: During simulation, save which river the correlation structure is taken from. Here is a temp fix for now so I do not need to simulate all again:
  dplyr::mutate(
    river = dplyr::if_else(true_tau_inner == 0.8122395, "Donau", "Isar")
  )
  
  
# Display differences
# Consider theta and tau separately bc different values. Also, tau is restricted onto -1 and 1, thus largest possible difference in 2
ldf = crt |> 
  tidyr::pivot_longer(
    cols = contains("diff"),
    names_to = "est",
    values_to = "diff"
  ) |> 
  dplyr::mutate(tau = grepl("tau", est, fixed = TRUE)) 

# How does dependence structure affect the MSE in tau and theta? ----
# ## tau
ldf |> dplyr::filter(tau) |> 
  ggplot(aes(y = diff, x = est)) + 
  geom_boxplot() + 
  facet_wrap(~ paste(river, cop), scale = "fixed")
# Deviations from the true tau all look somewhat identical among the different copula families and rivers
# Important: Cannot really observe a bias?
# ## theta
ldf |> dplyr::filter(!tau) |> 
  ggplot(aes(y = diff, x = est)) + 
  geom_boxplot() + 
  facet_wrap(~ paste(river, cop), scale = "fixed")
# Independent of the underlying dependence structure, the error seems to be dependent on the copula family
# But this may be due to skewness in the distribution which is hard to see in boxplots
ldf |> dplyr::filter(!tau) |> 
  ggplot(aes(x = diff, fill = est)) +
  geom_histogram(alpha = 0.6) + 
  facet_wrap(~ paste(river, cop, scale = "fixed"))
# Does not really look like distributions are crazy skewed, but I am still not sure if a negative difference is just less likely due to 
#   the range of the thetas
# How does sample size affect MSE ----
# -> From previously, it looked like the copula family affects the MSE. Thus, keep family as further effect "control"
# ## tau
ldf |> dplyr::filter(tau) |> 
  ggplot(aes(y = diff, x = est, color = cop)) + 
  geom_boxplot() + 
  facet_wrap(~ n, scale = "fixed")
# Basing the tau estimates on the functional form implied by the fitted model, we have a substantial error in the outer tau that only slowly decays
# But there still is no bias to observe -> Jensen's inequality does not seem to introduce a meaningful bias 
# ## theta
ldf |> dplyr::filter(!tau) |> 
  ggplot(aes(y = diff, x = est, color = cop)) + 
  geom_boxplot() + 
  facet_wrap(~ n, scale = "fixed")
# Frank copula seems to be the most difficult to fit bzw. the most unreliable. However, the error in theta decays a lot faster than in tau
# Also, here the INNER theta seems to be of higher error compared to the outer theta. Not sure why?
# --> There does not seem to be a bias in any of the estimates. Thus, no bias due to Jensens
# --> The estimates of theta for small samples are somewhat reliable, however, the small sample estimates for tau based on the functional form are less reliable / high in variance

# MSE table
crt |> 
  dplyr::summarise(
    n = dplyr::n(), # number cases where specific river correlation structure was drawn
    mse_theta_inner = mean(theta_inner_diff^2),
    var_theta_inner = var(fit_theta_inner),
    bias_theta_inner = mse_theta_inner - var_theta_inner,
    
    mse_theta_outer = mean(theta_outer_diff^2),
    var_theta_outer = var(fit_theta_outer), 
    bias_theta_outer = mse_theta_outer - var_theta_outer,

    mse_tau_inner = mean(tau_inner_diff^2),
    var_tau_inner = var(fit_tau_inner),
    bias_tau_inner = mse_tau_inner - var_tau_inner,

    mse_tau_outer = mean(tau_outer_diff^2),
    var_tau_outer = var(fit_tau_outer),
    bias_tau_outer = mse_tau_outer - var_tau_outer,

    .by = c(true_tau_inner, cop) # group by correlation (i.e. by river)
  )  |> 
  dplyr::select(contains("mse"), contains("bias"))
# Also, depending on the copula FAMILY, we have changing theta values given a tau value
# Thus, we need to group by river AND copula family
# No fucking idea why the bias is negative. Cannot be. But it is so small, that it might be due to accumulation of computer inaccuracy in estimation?
# I just take it for now... I mean, based on the plots a bias of 0 is reasonable

# TODO Add behavior of tau AC estimates in case of NACs. Paper mentioned that it is biased towards the lower true tau
#   Idea: For each tau (i.e. each river), calculate the AC MLE for tau and take mean(MLE) and check where it lies
#   --> I do not need sim of AC, because paper examined behavior of AC under NAC!!!



# Analysis Vines ----------------------------------------------------------
all_res = read_dep_files(true_dep = "vine", in_dir = "../data/simulation/") |> 
  dplyr::mutate(
    true_famname_12 = get_vine_famname(true_fam_12),
    true_famname_13 = get_vine_famname(true_fam_13),
    true_famname_23 = get_vine_famname(true_fam_23),
    famcomb = as.factor(
      paste(
        substr(true_famname_12, start = 1, stop = 1), 
        substr(true_famname_23, 1, 1), 
        substr(true_famname_13, 1, 1), 
        sep = "-"
      )
    )
  )
table(all_res$river, all_res$n)
table(all_res$true_famname_12, all_res$true_famname_13, all_res$true_famname_23)

# I care mostly about the stronger dependent variables, right? 
# I care about fitting the correct copula family here and about the MSE for this one
# Accuracy in the other one is nice, but since the dependence structure is less severe anyway, it's not too relevant 
# (i.e. getting something non-relvant off is not that severe)

# Sanity check
nrow(all_res) # Should be length(sample_size) * B 
table(all_res$n) # Should be B each
table(all_res$river) # Distribution of selected correlation; Should be equivalent to the number of different rivers I consider
table(all_res$famcomb) # Realized family combinations

# Checking for any NaNs
any(is.na(all_res))

# xxxxxxxxxxxxxx a) Copula fitting xxxxxxxxxxxxxxxxxxxxx
# >> Evaluation
# Start with 3. point so I can adjust data later on...
# 3) KL empirical density plot to evaluate how the other copula models perform under misspecification
# KL by river (dependence structure)
all_res |> 
    dplyr::select(n, river, contains("_kl")) |> 
    tidyr::pivot_longer(
      cols = contains("_kl"),
      names_to = "dep",
      values_to = "kld"
    ) |> 
  ggplot(aes(x = kld, color = dep)) +
  geom_density() + 
  facet_wrap(~ river)
# KL by n
all_res |> 
    dplyr::select(n, river, contains("_kl")) |> 
    tidyr::pivot_longer(
      cols = contains("_kl"),
      names_to = "dep",
      values_to = "kld"
    ) |> 
  ggplot(aes(x = kld, color = dep)) +
  geom_density() + 
  facet_wrap(~ n)
# KL by families
all_res |> 
  dplyr::select(n, river, contains("_kl"), famcomb) |> 
  tidyr::pivot_longer(
      cols = contains("_kl"),
      names_to = "dep",
      values_to = "kld"
  ) |> 
  ggplot(aes(x = kld, color = dep)) +
  geom_density() + 
  facet_wrap(~ famcomb)



library(ggplot2)
library(patchwork)

# Load Data ---------------------------------------------------------------
get_copula_df = function(
    p_threshold = NULL,
    in_dir = "../data/output/rdata/copula_dfs/", 
    all = FALSE
  ){
  "
  Read all copula dfs and join them to one large copula df
  "
  if (is.null(p_threshold)) assertthat::assert_that(all == TRUE) # If not all dataframes, then p_threhsold must be given
  if (!is.null(p_threshold)) assertthat::assert_that(all != TRUE) # If all dataframes, then no p_threshold must be given
  
  pattern = "*.Rdata"
  if (!all) pattern = paste("_", p_threshold, "_copula.Rdata", sep = "")
  filenames = paste(in_dir, list.files(in_dir, pattern = pattern), sep = "")
  
  cop_df = purrr::map_dfr(filenames, load_rdata)
  
  cop_df = cop_df |> 
    dplyr::mutate(
      pobs_dur = copula::pobs(duration_min),
      pobs_peak = copula::pobs(peak),
      pobs_vol= copula::pobs(volume),
      .by = unit
    ) 
  
  return(cop_df)
}

get_long_df = function(
    in_dir = "../data/output/rdata/threshold_dfs/"
  ){
  #TODO: SAME as get_copula_df, but with different path. lol. Just have one function....!
  "
  Read all long ('extended') data frames and join to one large one
  "
  filenames = paste(in_dir, list.files(in_dir, pattern = "*data"), sep = "")

  return(purrr::map_dfr(filenames, load_rdata))
}

load_rdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
}


# Rendering River Templates -----------------------------------------------
render_rivertemplate = function(
    df,
    out_dir = "../riverreports/"
  ){
  report_name = paste(out_dir, "Report_", unique(df$river), "_t", unique(df$p_threshold), ".html", sep = "")
  
  rmarkdown::render(
    input = "02_riverreport_template.Rmd",
    output_file = report_name,
    params = list(
      df = df,
      generate = TRUE
    )
  )
}


analyse_dependence = function(df){
  
  vol_dur = ggplot(df, aes(x = volume, y = duration_min)) + 
    geom_point()
  
  vol_peak = ggplot(df, aes(x = volume, y = peak)) + 
    geom_point()
  
  dur_peak = ggplot(df, aes(x = duration_min, y = peak)) + 
    geom_point()
  
  combined_plot = (vol_dur | vol_peak | dur_peak) 
  print(combined_plot)
  
  print(paste("Vol-Dur: ", cor(df$volume, df$duration_min, method = "kendall")))
  print(paste("Vol-Peak:", cor(df$volume, df$peak, method = "kendall")))
  print(paste("Dur-Peak:", cor(df$duration_min, df$peak, method = "kendall")))
}

# Simulation --------------------------------------------------------------

get_nac_tau = function(tauvec){
  # Based on 3 taus, return largest and build average above the other two
  # Assumption: 3rd is largest tau
  tau = c(mean(tauvec[1:2]), tauvec[3])
  return(tau)
}



nac_tau2theta = function(family_index, tau){
  return(
    c(
      HAC::tau2theta(tau = tau[1], type = family_index),
      HAC::tau2theta(tau = tau[2], type = family_index)
    )
  )
}
nac_theta2tau = function(family_index, theta){
  theta = unname(theta)
  return(
    c(
      HAC::theta2tau(theta = theta[1], type = family_index),
      HAC::theta2tau(theta = theta[2], type = family_index)
    )
  )
}

fit_ac = function(mat, cops, debug = FALSE){
  # HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
  # Solution: copula Package: Can fit ONLY non-nested ACs for more than 2 variables
  # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
  # Due to the implementation in this package: 1) fit possible models 2) Use AIC to select the best one 
  if (debug) browser()
  ac_fits = lapply(cops, function(cop) copula::emle(u = mat, cop = copula::onacopula(family = cop, nacStructure = C(1, 1:3))))
  
  # Select the one with smallest AIC
  # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
  ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
  ac_best_fit = which.max(ac_lls)
  # if (is.integer(ac_best_fit) && length(ac_best_fit) == 0) ac_best_fit = sample(1:3, 1) # TODO: TEMP SOL: In case of it breaking down, select one copula at random
  ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
  
  # Actual copula
  ac = copula::onacopulaL(family = cops[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
  attr(ac, "logLik") = ac_lls[ac_best_fit]
  
  return(ac)
}

fit_nac = function(mat, families){
  # Estimate copula using implemented selection method
  # HAC package does not offer way to select the copula family
  # Solution: Fit all considered families and use AIC to select accordingly
  
  # Allowed copula families
  types = unlist(unname(families))
  
  # Select one with smallest AIC. NOTE: p const --> use logLik
  nac_fits = lapply(types, function(type) HAC::estimate.copula(mat, type = type))
  # If any of the NACs suggests a non-nested structure, remove it. 
  # Two reasons: 
  # 1) I already fit a non-nested structure
  # 2) The HAC logLik function breaks down for these functions. And since I already fit one anyway, I use the simplest way to deal with this issue
  # Non-nested NACs have a tree structure of length 4. Use this to filter them out:
  nac_fits = nac_fits[!lapply(nac_fits, function(nac) length(nac$tree)) == 4]
  
  nac_lls = lapply(nac_fits, function(nac) get_nac_loglik(mat, nac))
  nac_best_fit = which.max(nac_lls)
  
  # Best fit
  nac = nac_fits[[nac_best_fit]]
  
  attr(nac, "logLik") = nac_lls[nac_best_fit]
  attr(nac, "theta") = get_nac_estimates(nac)
  attr(nac, "tau") = nac_theta2tau(nac$type, attr(nac, "theta"))
  
  return(nac)
}

get_nac_loglik = function(mat, nac_mdl){
  return(HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE))
}

get_nac_estimates = function(nac_mdl){
  return(c(nac_mdl$tree[[3]], nac_mdl$tree[[1]][[3]]))
}

fit_vine = function(mat, families){
  # The VineCopula package is faster and applicable as long as we do not also estimate covariates
  # Families: 3, 4, 5 (see docu)
  vine = VineCopula::RVineStructureSelect(data = mat, rotations = FALSE, familyset = families)
  return(vine)
}

get_nac_name = function(idx, inverse_copula_families = list("1" = "Gumbel", "3" = "Clayton", "5" = "Frank")){
  return(inverse_copula_families[as.character(idx)][[1]])
}

get_ac_estimate = function(ac) unname(ac@copula@tau( ac@copula@theta ))

run_one_nac = function(
    seed,
    n,
    cop,
    taus,
    copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5) # Copula families contained in the package
    ){
  # Simulate from true model ------------------------------------------------
  # For reproducibility
  set.seed(seed)
  
  # Draw random tau vector and determine nac structure from this
  rtau = sample(taus, 1)
  river = names(rtau)
  tau = get_nac_tau(tauvec = rtau[[1]])
  theta = nac_tau2theta(family_index = copula_families[cop], tau = tau)
  
  # Create HAC-object
  mdl = HAC::hac.full(type = copula_families[cop], y = c("v3", "v2", "v1"), theta = theta)
  # Draw sample according to true_mdl (simulated sample)
  mat = HAC::rHAC(n, mdl) 
  
  # Fit models --------------------------------------------------------------
  ac = fit_ac(mat, names(copula_families))
  
  nac = fit_nac(mat, copula_families)
 
  vine = fit_vine(mat, families = c(3, 4, 5))
  
  # Save results ------------------------------------------------------------
  res = data.frame(
    list(
      # True values
      seed = seed,
      river = river,
      true_tau_outer = tau[1],
      true_theta_outer = theta[1],
      true_tau_inner = tau[2],
      true_theta_inner = theta[2],
      # Estimated values
      fit_cop = get_nac_name(nac),
      fit_theta_outer = attr(nac, "theta")[1],
      fit_theta_inner = attr(nac, "theta")[2],
      fit_tau_outer = attr(nac, "tau")[1],
      fit_tau_inner = attr(nac, "tau")[2],
      nac_aic = ll2aic(ll = attr(nac, "logLik")[[1]], p = 2),
      nac_kl = klMonteCarlo(mdl, nac),
      # Other copulas / Misspecifications
      # Results of AC fit
      ac_tau = get_ac_estimate(ac),
      ac_aic = ll2aic(ll = attr(ac, "logLik")[[1]], p = 1),
      ac_kl = klMonteCarlo(mdl, HAC::nacopula2hac(ac), est_mdl_AC = TRUE),
      # Results for Vine
      vine_aic = vine$AIC,
      vine_kl = klMonteCarlo(mdl, vine, est_mdl_vine = TRUE)
    )
  )
  return(res)
}




# For each sample size (n), each copula family, each dependence structure (AC, NAC, Vine), tau:
#   Repeat B times: 
#     Draw random sample  
#   For each sample, fit AC, NAC and Vine copula and evaluate
# run_one_n_ac = function(
#     seed, 
#     n,
#     cop,
#     dep,
#     copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5), # Copula families contained in the package
#     beta_a = 2.5, 
#     beta_b = 1.5
#   ){
#   # Simulate from true model ------------------------------------------------
#   # For reproducibility
#   set.seed(seed)
#   
#   # If symmetric, the values of the 'inner' and 'outer' tau bzw. copula parameter are simply identical
#   tau_inner = rbeta(n = 1, shape1 = beta_a, shape2 = beta_b)
#   tau_outer = tau_inner # base case is symmetric AC
#   if (dep == "nac") tau_outer = 1/2 * tau_inner # Simple solution to ensure outer parameter is smaller than inner
#   
#   # Define a selection of possible tau
#   # Derive a selection from the empirical observations? 
#   # AC: Take average of the observed tau
#   # NAC: Take most nested and average of other 2
#   # Vine: Take the observed taus
#   
#   # Package uses integer for copulas. Get this integer 
#   fam = copula_families[cop]
#   
#   # Create HAC-object
#   true_mdl = HAC::hac.full(
#     type = fam, 
#     y = c("v3", "v2", "v1"), 
#     theta = c(HAC::tau2theta(tau = tau_outer, type = fam), HAC::tau2theta(tau = tau_inner, type = fam))
#   )
#   
#   # Draw sample according to true_mdl (simulated sample)
#   mat = HAC::rHAC(n, true_mdl) 
#   attr(mat, "true_mdl") = true_mdl 
#   attr(mat, "tau") = c(outer = tau_outer, inner = tau_inner)
#   
#   # Fit models --------------------------------------------------------------
#   
#   # I) (symmetric) Archimedean copulas ----
#   # Funny enough, HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
#   # Lucky me, the copula package can fit ONLY non-nested ACs for more than 2 variables
#   # Copula packages are a mess, wtf...
#   # Also, due to the implementation in this package, I first fit all models, then use AIC to select the best one and finally create the best model
#   # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
#   ac_fits = lapply(names(copula_families), function(name) copula::emle(u = mat, cop = copula::onacopula(family = name, nacStructure = C(1, 1:3))))
#   # Select the one with smallest AIC
#   # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
#   ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
#   ac_best_fit = which.max(ac_lls)
#   ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
#   # Actual copula
#   ac_mdl = copula::onacopulaL(family = names(copula_families)[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
# 
#   
#   # II) Nested Archimedean copulas ----
#   # Estimate copula using implemented selection method
#   nac_mdl = HAC::estimate.copula(mat) # TODO There is no structure selection!! Again, solve this using AIC use type 1, 3, 5
#   # Calculate ll to evaluate AIC
#   nac_ll  = HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE)
#   
#   estimates = c(outer = nac_mdl$tree[[3]], inner = nac_mdl$tree[[1]][[3]])
#   
#   # III) Vine copulas ----
#   # The VineCopula package is faster and applicable as long as we do not also estimate covariates
#   vine_mdl = VineCopula::RVineStructureSelect(
#     data = mat, 
#     rotations = FALSE, familyset = c(3, 4, 5) # Only allow for the considered copula families
#   )
#   vine_mdl
#   
#   # Save results in df ----
#   res = data.frame(
#     list(
#       seed = seed,
#       true_tau_inner = tau_inner,
#       true_tau_outer = tau_outer,
#       # Results of AC fit
#       ac_selected_cop = names(copula_families)[ac_best_fit],
#       ac_theta = ac_mle,
#       ac_tau = HAC::theta2tau(ac_mle, type = copula_families[names(copula_families)[ac_best_fit]]),
#       ac_aic = ll2aic(ll = ac_lls[[ac_best_fit]], p = 1),
#       ac_bic = ll2bic(ll = ac_lls[[ac_best_fit]], p = 1),
#       ac_kl = klMonteCarlo(true_mdl, HAC::nacopula2hac(ac_mdl), est_mdl_AC = TRUE),
#       # Results of NAC fit
#       nac_selected_cop = HAC::hac2nacopula(nac_mdl)@copula@name,
#       nac_theta_outer = estimates["outer"][[1]],
#       nac_theta_inner = estimates["inner"][[1]],
#       nac_tau_outer = HAC::theta2tau(theta = estimates["outer"][[1]], type = nac_mdl$type),
#       nac_tau_inner = HAC::theta2tau(theta = estimates["inner"][[1]], type = nac_mdl$type),
#       nac_aic = ll2aic(ll = nac_ll, p = 2),
#       nac_bic = ll2bic(ll = nac_ll, p = 2),
#       nac_kl = klMonteCarlo(true_mdl, nac_mdl),
#       # Results of Vine fit
#       vine_aic = vine_mdl$AIC,
#       vine_bic = vine_mdl$BIC,
#       vine_kl = klMonteCarlo(true_mdl = true_mdl, est_mdl = vine_mdl, est_mdl_vine = TRUE)
#     )
#   )
# 
#   return(res)
# }
run_one_vine = function(
    seed,
    n,
    taus,
    vine_copula_families = list("Gumbel" = 4, "Clayton" = 3, "Frank" = 5), # Copula families contained in the VineCopula package
    hac_copula_families = list("Gumbel" = 2, "Clayton" = 3, "Frank" = 5), # Copula families contained in HAC copula package
    vine_colnames = c("1-3", "2-3", "1-2")
    ){
  # Simulate from true model ------------------------------------------------
  # For reproducibility
  set.seed(seed)

  # Draw random tau vector and determine nac structure from this
  rtau = sample(taus, 1)
  river = names(rtau)
  tau = unlist(rtau)
  names(tau) = vine_colnames
  
  # Vine matrix defining the (un)conditional copulas in the model
  # 1 - 2 - 3 
  vine_matrix = matrix(
    c(
      1, 0, 0,
      3, 2, 0,
      2, 3, 3
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )

  # Randomly drawing the combination of copula families
  fams = unlist(sample(vine_copula_families, size = 3, replace = T))
  names(fams) = vine_colnames
  # Save families in family copula matrix
  family_matrix = matrix(0, nrow = 3, ncol = 3)
  family_matrix[2, 1] = fams["1-3"]
  family_matrix[3, 1:2] = fams[c("1-2", "2-3")]

  params = matrix(0, nrow = 3, ncol = 3)
  theta = c(
    "1-3" = VineCopula::BiCopTau2Par(fams["1-3"], tau["1-3"]),
    "1-2" = VineCopula::BiCopTau2Par(fams["1-2"], tau["1-2"]),
    "2-3" = VineCopula::BiCopTau2Par(fams["2-3"], tau["2-3"])
  )
  params[2, 1] = theta["1-3"]
  params[3, 1:2] = c(theta["1-2"], theta["2-3"])

  rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
  mat = VineCopula::RVineSim(n, RVM = rvmat)

  attr(mat, "rvm") = rvmat
  colnames(mat) = c("v1", "v2", "v3")
  
  # Fit models --------------------------------------------------------------
  ac = fit_ac(mat, names(hac_copula_families))

  nac = fit_nac(mat, hac_copula_families)

  vine = fit_vine(mat, families = c(3, 4, 5))

  # Save results ------------------------------------------------------------
  res = data.frame(
    list(
      # True values
      seed = seed,
      river = river,
      true_tau_12 = tau["1-2"],
      true_fam_12 = fams["1-2"],
      true_theta_12 = theta["1-2"],
      true_tau_23 = tau["2-3"],
      true_fam_23 = fams["2-3"],
      true_theta_23 = theta["2-3"],
      true_tau_13 = tau["1-3"],
      true_fam_13 = fams["1-3"],
      true_theta_13 = theta["1-3"],
      # Estimated values
      fit_tau_12 = vine$tau[3, 1],
      fit_fam_12 = vine$family[3, 1],
      fit_theta_12 = vine$par[3, 1],
      fit_tau_23 = vine$tau[3, 2],
      fit_fam_23 = vine$family[3, 2],
      fit_theta_23 = vine$par[3, 2],
      fit_tau_13 = vine$tau[2, 1],
      fit_fam_13 = vine$family[2, 1],
      fit_theta_13 = vine$par[2, 1],
      vine_aic = vine$AIC,
      vine_kl = vine_klMonteCarlo(rvmat, vine, est = "vine"),
      # Other copulas / Misspecifications
      # NAC fit
      nac_tau_outer = attr(nac, "tau")[1],
      nac_tau_inner = attr(nac, "tau")[2], 
      nac_aic = ll2aic(ll = attr(nac, "logLik")[[1]], p = 2),
      nac_kl = vine_klMonteCarlo(rvmat, nac, est = "nac"),
      # AC fit
      ac_aic = ll2aic(ll = attr(ac, "logLik")[[1]], p = 1),
      ac_kl = vine_klMonteCarlo(rvmat, HAC::nacopula2hac(ac), est = "ac")
    )
  )
  return(res)
}

vine_klMonteCarlo = function(
    rvmat,
    est_mdl, 
    est = "vine", 
    values =  seq(from = 0.01, to = 0.99, length.out = 25) # Keep from (to) relatively high (low) simmplifying numerical stability. Downside: KL not considering full support..........lol.
  ){
  grid = expand.grid(v1 = values, v2 = values, v3 = values)
  
  true_d = VineCopula::RVinePDF(grid, rvmat)
  
  # ifelse cannot return matrices, aparently. Thus, keep it in two if-statements...
  if (est == "vine") est_d = VineCopula::RVinePDF(grid, est_mdl)
  if (est == "ac") for (i in 1:3) est_mdl$tree[[i]] = paste("v", i, sep = "")
  if (est != "vine") est_d = HAC::dHAC(as.matrix(grid), est_mdl)

  return(mean(log(true_d / est_d) ))
}
 
# TODO
# run_one_vine = function(
#     seed, 
#     n,
#     cop,
#     dep
#   ){
#   # Simulate from true model ------------------------------------------------
#   # Tau for the dependence structure
#   tau_13 = rbeta(n = 1, shape1 = beta_a, shape2 = beta_b)
#   tau_23 = 3/5 * tau_13
#   tau_12 = 1/3 * tau_13
#   
#   # Vine matrix defining the (un)conditional copulas in the model
#   # 1 - 2 - 3
#   vine_matrix = matrix(
#     c(
#       1, 0, 0, 
#       3, 2, 0,
#       2, 3, 3
#     ),
#     nrow = 3, ncol = 3,
#     byrow = TRUE
#   )
#   
#   # Randomly drawing the combination of copula families
#   fams = unlist(sample(copula_families, size = 3, replace = T))
#   # Save families in family copula matrix
#   family_matrix = matrix(0, nrow = 3, ncol = 3)
#   family_matrix[2, 1] = fams[1]
#   family_matrix[3, 1:2] = fams[2:3]
#   
#   params = matrix(0, nrow = 3, ncol = 3)
#   params[2, 1] = VineCopula::BiCopTau2Par(fams[1], tau_13)
#   params[3, 1:2] = c(
#     VineCopula::BiCopTau2Par(fams[2], tau_12),
#     VineCopula::BiCopTau2Par(fams[3], tau_23)
#   )
#   
#   rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
#   mat = VineCopula::RVineSim(n, RVM = rvmat)
#   
#   attr(mat, "rvm") = rvmat
#   colnames(mat) = c("v1", "v2", "v3")
#   
#   # Fit models --------------------------------------------------------------
#   
#   
#   # I) (symmetric) Archimedean copulas ----
#   # Funny enough, HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
#   # Lucky me, the copula package can fit ONLY non-nested ACs for more than 2 variables
#   # Copula packages are a mess, wtf...
#   # Also, due to the implementation in this package, I first fit all models, then use AIC to select the best one and finally create the best model
#   # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
#   ac_fits = lapply(names(copula_families), function(name) copula::emle(u = mat, cop = copula::onacopula(family = name, nacStructure = C(1, 1:3))))
#   # Select the one with smallest AIC
#   # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
#   ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
#   ac_best_fit = which.max(ac_lls)
#   ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
#   # Actual copula
#   ac_mdl = copula::onacopulaL(family = names(copula_families)[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
#   
#   # II) Nested Archimedean copulas ----
#   # Estimate copula using implemented selection method
#   nac_mdl = HAC::estimate.copula(mat)
#   # Calculate ll to evaluate AIC
#   nac_ll = HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE)
#   
#   # III) Vine copulas ----
#   # The VineCopula package is faster and applicable as long as we do not also estimate covariates
#   vine_mdl = VineCopula::RVineStructureSelect(
#     data = mat, 
#     rotations = FALSE, familyset = c(3, 4, 5) # Only allow for the considered copula families
#   )
#   vine_mdl
#   
#   # Save result df ----
#   res = data.frame(
#     list(
#       seed = seed,
#       # Results of AC fit
#       ac_aic = ll2aic(ll = ac_lls[[ac_best_fit]], p = 1),
#       ac_kl = klMonteCarlo(true_mdl, HAC::nacopula2hac(ac_mdl), est_mdl_AC = TRUE),
#       # Results of NAC fit
#       nac_aic = ll2aic(ll = nac_ll, p = 2),
#       nac_kl = klMonteCarlo(true_mdl, nac_mdl),
#       # Results of Vine fit
#       
#       vine_aic = vine_mdl$AIC,
#       vine_kl = klMonteCarlo(true_mdl = true_mdl, est_mdl = vine_mdl, est_mdl_vine = TRUE)
#     )
#   )
#   
#   return(res)
# }

klMonteCarlo = function(
    true_mdl, 
    est_mdl, 
    est_mdl_AC = FALSE, 
    est_mdl_vine = FALSE, 
    values =  seq(from = 0.01, to = 0.99, length.out = 25) # Keep from (to) relatively high (low) simmplifying numerical stability. Downside: KL not considering full support..........lol.
                                                           # This implies that I am "numerically blind" for differences below or above these thresholds. 
  ){
  "
  Function to numerically approximate the KL divergence.
  Not fully reliable tbh... have to work on it. But not now, I postpone this until later.
  "
  # Ensure correct names (Is an issue when dealing with copula to HAC transformed copula. Cannot deal with it any other way aparently)
  if (est_mdl_AC) for (i in 1:3) est_mdl$tree[[i]] = paste("v", i, sep = "")
  
  # Grid points where the copula density is evaluated on
  grid = expand.grid(v1 = values, v2 = values, v3 = values)
  
  # Densities
  true_d = HAC::dHAC(as.matrix(grid), true_mdl)
  # ifelse cannot return matrices, aparently. Thus, keep it in two if-statements...
  if (est_mdl_vine) est_d = VineCopula::RVinePDF(newdata = grid, RVM = est_mdl)
  if (!est_mdl_vine) est_d = HAC::dHAC(as.matrix(grid), est_mdl)

  # Numerically estimate E[log(p/q)] by mean(log(p/q)) 
  # The way I calculate KL now lead to NAs for:
  # tau_inner = 0.2011794
  # tau_outer = 0.1005897
  # Not sure why tho...
  return(mean(log(true_d / est_d) ))
}

ll2aic = function(ll, p){
  return(-2 * ll + 2 * p)
}


draw_model_beta = function(){
  "
  Linear model used to calculate the underlying - true - correlation.
  
  in 50% of the cases, I want the model to have a covariate included (i.e. beta != 0) and 
  in the other cases, no covariate in the true model (beta == 0 = intercept / constant model)
  "
  # Intercept affects 
  
  # Inclusion is 50:50
  include = rbinom(n = 1, size = 1, prob = 0.5)
  beta = ifelse(include, rnorm(n = 1, mean = 5, sd = 1), 0)
  return(beta)
}

read_dep_files = function(
    true_dep ,
    in_dir = "../data/simulation/"
  ){
  filenames = paste(
    in_dir, 
    list.files(in_dir, pattern = paste("dep", true_dep, sep = "")),
    sep = ""
  )
  
  return(purrr::map_dfr(filenames, load_depdata))
}

load_depdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
  
  # Append attributes to df. Necessary before merging them into one big df
  n = attr(env$res, "n")
  dep = attr(env$res, "dep")
  cop = attr(env$res, "cop")
  if (dep == "vine") cop = "vine"
  
  # Sanity messages
  message(paste("n:", n, "-- cop:", cop, "-- dep:", dep,"-- #seeds:", nrow(env$res)))
  
  return(env$res <- env$res |> dplyr::mutate(n = n, cop = cop, dep = dep, .after = seed))
}


# Simulation Analysis -----------------------------------------------------
klplots = function(
    df,
    cop_name,
    dens_alpha = 1,
    col_ac = "red",
    col_nac = "blue",
    col_vine = "green",
    scales = "fixed"
  ){
  p = df |> 
    dplyr::filter(cop == cop_name) |> 
    dplyr::select(n, contains("_kl")) |> 
    tidyr::pivot_longer(
      cols = contains("_kl"),
      names_to = "dep",
      values_to = "kld"
    ) |>
    dplyr::mutate(dep = as.factor(stringr::str_remove(dep, "_kl"))) |>
    ggplot() + 
    geom_density(aes(x = kld, color = dep), alpha = dens_alpha) +
    geom_vline(xintercept = 0, color = "black") + 
    facet_wrap(~ n, scale = scales) +
    labs(
      title = "Kullback Leibler by Sample Size and Fit",
      x = "Kullback Leibler Divergence",
      y = "Density"
    ) +
    theme(legend.position = "bottom") 
    
  return(p)
}

get_vine_famname = function(idx){
  vine_famlist = list("4" = "Gumbel", "3" = "Clayton", "5" = "Frank")
  return(unname(unlist(lapply(idx, function(i) vine_famlist[as.character(i)]))))
}



# Presentation Plotting ---------------------------------------------------
gkd2gg = function(
    df, 
    coord_cols, 
    current_crs = 25832, # ETRS25832
    into_crs = 4326 # EPSG4326
  ){
  "
  Takes positions given by GKD website (CooRdinateSystem[crs] = ETRS25832) and returns a coordinate system ggplot can work with (crs = EPSG4326).
  "
  return(
    sf::st_transform(
      sf::st_as_sf(df, coords = coord_cols, crs = current_crs),
      crs = into_crs
    )
  )
}







# Copula oder so ----------------------------------------------------------
filter_infeasible_stations = function(
    cop_df, min_nyears = 15 # Sim had min 15 obs, so stick to that
  ){
  feasible_stations = cop_df |> 
    dplyr::summarise(
      years = dplyr::n(),
      .by = unit
    ) |> 
    dplyr::filter(years > min_nyears) |> 
    dplyr::select(unit)
  
  return(cop_df |> dplyr::filter(unit %in% feasible_stations$unit))
}

get_density_values = function(vine, dgrid, unit_name){
  plot_df = as.data.frame(dgrid) |>
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    # 1-2: index [3, 1]
    # 1-3: index [2, 1]
    # 2-3: index [3, 2]
    dplyr::mutate(
      z12 = VineCopula::BiCopPDF(x, y, family = vine$family[3, 1], par = vine$par[3, 1]),
      z13_2 = VineCopula::BiCopPDF(x, y, family = vine$family[2, 1], par = vine$par[2, 1]),
      z23 = VineCopula::BiCopPDF(x, y, family = vine$family[3, 2], par = vine$par[3, 2])
    ) |>
    tidyr::pivot_longer(
      cols = contains("z"),
      names_to = "vars",
      values_to = "dens"
    ) |>
    # Standardize density values so scale does not matter
    dplyr::group_by(vars) |>
    dplyr::mutate(
      density = (dens - mean(dens)) / sd(dens)
    ) |> 
    dplyr::ungroup()
  # Add family 
  plot_df$fam = rep(c(f12 = vine$family[3, 1], f13 = vine$family[2, 1], f23 = vine$family[3, 2]), nrow(plot_df) / 3)
  plot_df$unit = unit_name
  return(plot_df)
}

get_cdf_values = function(vine, dgrid, unit_name){
  plot_df = as.data.frame(dgrid) |>
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    # 1-2: index [3, 1]
    # 1-3: index [2, 1]
    # 2-3: index [3, 2]
    dplyr::mutate(
      z12 = VineCopula::BiCopCDF(x, y, family = vine$family[3, 1], par = vine$par[3, 1]),
      z13_2 = VineCopula::BiCopCDF(x, y, family = vine$family[2, 1], par = vine$par[2, 1]),
      z23 = VineCopula::BiCopCDF(x, y, family = vine$family[3, 2], par = vine$par[3, 2])
    ) |>
    tidyr::pivot_longer(
      cols = contains("z"),
      names_to = "vars",
      values_to = "cdf"
    ) 
    # Standardize density values so scale does not matter
    # dplyr::group_by(vars) |>
    # dplyr::mutate(
    #   density = (cdf - mean(cdf)) / sd(cdf)
    # ) |> 
    # dplyr::ungroup()
  # Add family 
  # plot_df$fam = rep(c(f12 = vine$family[3, 1], f13 = vine$family[2, 1], f23 = vine$family[3, 2]), nrow(plot_df) / 3)
  plot_df$unit = unit_name
  return(plot_df)
}

get_tail_dependencies = function(vine, name){
  utdp = vine$taildep$upper
  ltdp = vine$taildep$lower
  data.frame(
    unit = name,
    ltdp_12 = ltdp[3, 1],
    ltdp_23 = ltdp[3, 2],
    ltdp_13_2 = ltdp[2, 1],
    utdp_12 = utdp[3, 1],
    utdp_23 = utdp[3, 2],
    utdp_13_2 = utdp[2, 1]
  )
}

get_synthetic_data = function(vine, n, unit_name){
  as.data.frame(VineCopula::RVineSim(n, RVM = vine)) |> dplyr::mutate(unit = unit_name)
}

inverse_ecdf <- function(u, data) {
  quantile(data, probs = u, type = 1)  # Use type = 1 for stepwise approximation
}

inverse_ecdf_unitwise <- function(syn, syn_col, df, df_col, unit_name) {
  syn_ = (unlist((syn |> dplyr::filter(unit == unit_name))[syn_col]))
  names(syn_) = df_col
  df_ = unname(unlist((df |> dplyr::filter(unit == unit_name))[df_col]))
  return(unname(sapply(syn_, inverse_ecdf, data = df_)))
}

get_contour = function(
    rel, 
    splot_df, 
    sdf, 
    varx, 
    vary, 
    bwidth = 0.1, 
    plt_pts = TRUE, 
    title = "TITLE",
    x_lab = "x",
    y_lab = "y",
    contour_alpha = 1,
    z = "density"
  ){
  p = ggplot() +
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    geom_contour(data = splot_df |> dplyr::filter(vars == rel), aes_string(x = "x", y = "y", z = z), binwidth = bwidth, alpha = contour_alpha) + 
    labs(
      title = title,
      y = y_lab,
      x = x_lab 
    ) + 
    scale_x_continuous(breaks = c(0, 1)) + 
    scale_y_continuous(breaks = c(0, 1))
  if (plt_pts) p = p + geom_point(data = sdf, mapping = aes_string(x = varx, y = vary), alpha = .8)
  return(p)
}

get_syn_scatter = function(
    ssyn_df, 
    varx_syn,  
    vary_syn, 
    sdf,  
    varx_df, 
    vary_df,  
    syn_color = "lightblue", 
    syn_alpha = 0.3,
    x_lab = "x",
    y_lab = "y",
    x_min = NULL,
    x_max = NULL,
    y_min = NULL,
    y_max = NULL
  ){
  p = ggplot() + 
    geom_point(data = ssyn_df, mapping = aes_string(x = varx_syn, y = vary_syn), color = syn_color, alpha = syn_alpha) + 
    geom_point(data = sdf, mapping = aes_string(x = varx_df, y = vary_df)) +
    labs(
      x = x_lab,
      y = y_lab
    ) 
    if (!is.null(c(x_min, x_max))) p = p + scale_x_continuous(breaks = c(x_min, x_max)) 
    if (!is.null(c(y_min, y_max))) p = p + scale_y_continuous(breaks = c(y_min, y_max))
  return(p)
}

marginal_fit = function(vec, type){
  return(extRemes::fevd(vec, type = type))
}

dmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  shape = FALSE
  if (type != "Gumbel") shape = mle[["shape"]]
  
  
  return(
    extRemes::devd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = shape,
      type = type 
    )
  )
}

pmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  shape = FALSE
  if (type != "Gumbel") shape = mle[["shape"]]
  
  return(
    extRemes::pevd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = shape,
      type = type 
    )
  )
}
qmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  
  return(
    extRemes::qevd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = mle[["shape"]], 
      type = type 
    )
  )
  
}

invPIT = function(name, df, u){
  vec = unname(unlist(df[, name]))
  fit = logspline::logspline(vec, lbound = 0)
  
  return(logspline::qlogspline(u, fit))
}




showcase_copula_contours = function(tau, n_gen_sim = 5000, title = "TODO: Title",
                                    textsize_lab = 20, textsize_tick = 10, textsize_strip = 10, textsize_title = 20){
  # Formulas according to Hofert "Elements of copula Modeling with R" p. 98
  gen_clayton = function(t, theta) (1 + t)^(-1 / theta)
  gen_frank = function(t, theta) -log(1 - exp(-t) * (1 - exp(-theta))) / theta
  gen_gumbel = function(t, theta) exp(-t^(1 / theta))
  
  cop_fams = c("clayton" = 3, "gumbel" = 4, "frank" = 5)
  vine_matrix = matrix(
    c(
      1, 0, 0,
      3, 2, 0,
      2, 3, 3
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )
  
  family_matrix = matrix(0, nrow = 3, ncol = 3)
  family_matrix[2, 1] = cop_fams["clayton"]
  family_matrix[3, 1:2] = cop_fams[c("gumbel", "frank")]
  
  theta = c(
    "clayton" = VineCopula::BiCopTau2Par(cop_fams["clayton"], tau),
    "gumbel" = VineCopula::BiCopTau2Par(cop_fams["gumbel"], tau),
    "frank" = VineCopula::BiCopTau2Par(cop_fams["frank"], tau)
  )
  
  params = matrix(0, nrow = 3, ncol = 3)
  params[2, 1] = theta["clayton"]
  params[3, 1:2] = c(theta["gumbel"], theta["frank"])
  
  rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
  
  # Contours
  x = seq(from = 0.001, to = 0.99, length.out = 200)
  y = seq(from = 0.001, to = 0.99, length.out = 200)
  density_grid = expand.grid(x = x, y = y)
  
  plot_df = get_density_values(rvmat, density_grid, "showcase") |>
    dplyr::mutate(
      vars = dplyr::case_when(
        vars == "z12" ~ "Gumbel",
        vars == "z13_2" ~ "Clayton",
        vars == "z23" ~ "Frank"
      )
    )
  
  contours = ggplot(plot_df) +
    geom_contour(aes(x = x, y = y, z = density), binwidth = 0.5) + 
    labs(x = latex2exp::TeX("$u_1$"), y = latex2exp::TeX("$u_2$")) + 
    theme(axis.text = element_text(size = textsize_tick), axis.title = element_text(size = textsize_lab), strip.text = element_text(size = textsize_strip)) +
    facet_wrap(~vars)
  
  # Generator plots
  t = seq(0, 9, length.out = 1000)
  # Clayton Generator
  gens = data.frame(
    # Clayton = copula::copClayton@psi(t, theta["clayton"]),
    Clayton = gen_clayton(t, theta["clayton"]),
    # Gumbel Generator
    # Gumbel = copula::copGumbel@psi(t, theta["gumbel"]),
    Gumbel = gen_gumbel(t, theta["gumbel"]),
    # Frank Generator
    # Frank = copula::copFrank@psi(t, theta["frank"]),
    Frank = gen_frank(t, theta["frank"]),
    t = t
  ) |> 
    tidyr::pivot_longer(
      cols = c(Clayton, Gumbel, Frank),
      names_to = "family",
      values_to = "generator"
    ) |> 
    ggplot() + 
    geom_line(aes(x = t, y = generator), linewidth = 0.8) +
    facet_wrap(~family) +
    xlim(0, 1) + 
    theme(axis.text = element_text(size = textsize_tick), axis.title = element_text(size = textsize_lab), strip.text = element_text(size = textsize_strip), title = element_text(size = textsize_title)) +
    labs(x = "", y = "Generator", title = title)  
    
  
  gen_cont = gens / contours
  return(gen_cont)
}

savegg = function(
    filename, 
    ending = ".png", out_path = "../Präsentation/pictures/", width = 10, height = 8, dpi = 300)
  {
  ggsave(
    paste(out_path, filename, ending, sep = ""),
    width = width,
    height = height,
    dpi = dpi
  )
}

fit_nacs = function(cop_df, all_units){
  nac_fams = list("1" = "Gumbel", "3" = "Clayton", "5" = "Frank")
  
  nacs = lapply(
    all_units,
    function(name){
      fit_nac(mat = cop_df |> dplyr::filter(unit == name) |> dplyr::select(contains("pobs")) |> as.matrix(), families = c(1, 3, 5))
    }
  )
  names(nacs) = all_units
  
  return(nacs)
}


fit_vines = function(
    cop_df, 
    all_units,
    # Vine structure we assumed (Conditional copula is dur|peak - vol|peak)
    assumed_vine_structure =  matrix(
      # 1 - 2 - 3 where: 1:dur, 2:peak, 3:vol
      c(
        1, 0, 0,
        3, 2, 0,
        2, 3, 3
      ),
      nrow = 3, ncol = 3,
      byrow = TRUE
    )
  ){
  vines = lapply(
    all_units,
    function(name) {
      mat = cop_df |> dplyr::filter(unit == name) |> dplyr::select(contains("pobs")) |> as.matrix() 
      
      vine = VineCopula::RVineCopSelect(data = mat, Matrix = assumed_vine_structure, familyset = c(3, 4, 5))
    }
  )
  names(vines) = all_units
  
  return(vines)
}


rcond_vine_draws = function(HQ_prob, vine, n_syn = 1000, debug = FALSE){
  if (debug) browser()
  # Indices for AFTER drawing the samples
  idx_dur = 1
  idx_vol = 2
  p = 1 - HQ_prob # Use 1 - HQ_prob to use in CDF: CDF = P(X <= x), HQ = P(X > x)
  
  bicop_dp = VineCopula::BiCop(family = vine$family[3, 1], par = vine$par[3, 1])
  bicop_pv = VineCopula::BiCop(family = vine$family[3, 2], par = vine$par[3, 2])
  bicop_cond_dv = VineCopula::BiCop(family = vine$family[2, 1], par = vine$par[2, 1]) 
  
  # Draw samples of conditional probabilities
  #   Draw u_1|u_2 and u_3|u_2
  cond_sample = VineCopula::BiCopSim(N = n_syn, obj = bicop_cond_dv)
  # Inverse conditional probabilities
  #   Use BiCopHinv2: Inverse of u_1|u_2 --> obtain u_1
  dur = VineCopula::BiCopHinv2(u1 = cond_sample[, idx_dur], u2 = rep(p, n_syn), bicop_dp) 
  vol = VineCopula::BiCopHinv2(u1 = cond_sample[, idx_vol], u2 = rep(p, n_syn), bicop_pv)
  if (debug) plot(dur, vol)
  return(
    data.frame(hq_prob = rep(HQ_prob, n_syn), pobs_peak = rep(p, n_syn), pobs_dur = dur, pobs_vol = vol)
  )
}

cond_marginal_dens = function(vol_dur_vec, peak, marginal_vol, marginal_dur, marginal_peak, mdl, mdl_type, min = F, factor = 1e3){
  vol = vol_dur_vec[1]
  dur = vol_dur_vec[2]
  
  uMatrix = cbind(
    pmarginal(dur, marginal_dur),
    pmarginal(vol, marginal_vol),
    pmarginal(peak, marginal_peak)
  )
  colnames(uMatrix) = c("pobs_dur", "pobs_vol", "pobs_peak")
  
  if (mdl_type == "nac"){
    copula_density = HAC::dHAC(X = uMatrix, hac = mdl)
  } else if (mdl_type == "vine") {
    copula_density = VineCopula::RVinePDF(uMatrix, mdl)
  }
  
  # f(vol, dur | peak) = c(vol, dur, peak) f(vol) f(dur)
  conditional_marginal_density =  copula_density * dmarginal(vol, marginal_vol) * dmarginal(dur, marginal_dur)
  
  if (min) conditional_marginal_density = - factor * conditional_marginal_density
  return(conditional_marginal_density)
}

get_most_probable_voldur = function(
    hq_prob, 
    mdl, mdl_type,
    gev_vol, gev_dur, gev_peak, 
    initial_vol = 0, initial_dur = 0,
    optimizer = "L-BFGS-B", lower = 1e-5,
    trace = 1
    ){
  # Find most probable combination of vol and dur given a peak value
  # by maximizing the conditional density (using numerical approach)
  out = optim(
    par = c(initial_vol, initial_dur), # Initial values
    fn = cond_marginal_dens,
    peak = qmarginal(1 - hq_prob, gev_peak),
    marginal_vol = gev_vol,
    marginal_dur = gev_dur,
    marginal_peak = gev_peak,
    mdl = mdl, 
    mdl_type = mdl_type,
    min = T,
    method = optimizer,
    # hessian = T,
    lower = lower,
    control = list(
      trace = trace,
      maxit = 1e6,
      factr = 1e2
    )
  )
  vol = out$par[1]
  dur = out$par[2]
  return(
    data.frame(vol = vol, dur = dur, hq_prob = hq_prob)
  )
}

grab_taildeps = function(station, vines, cop_df){
  vine = vines[[station]]
  df = cop_df |> dplyr::filter(unit == station)
  river = df$river[1]
  north = df$north[1]
  east = df$east[1]
  
  data.frame(
    variables = c(
      "D - P",
      "P - V",
      "D - V"
    ),
    cop_fam = c(
      get_copname(vine$family[3, 1]),
      get_copname(vine$family[3, 2]),
      get_copname(vine$family[2, 1])
    ),
    upper = c(
      vine$taildep$upper[3, 1],
      vine$taildep$upper[3, 2],
      vine$taildep$upper[2, 1]
    ),
    lower = c(
      vine$taildep$lower[3, 1],
      vine$taildep$lower[3, 2],
      vine$taildep$lower[2, 1]
    )
  ) |> 
    dplyr::mutate(
      river = river,
      unit = station,
      north = north,
      east = east
    )
}


filter_cop_df = function(cop_df, n_minobs){
  "
  This functions removes the stations that have too little information to fit a copula on.
  "
  obs_status = cop_df |> 
  dplyr::summarise(
    n = dplyr::n(),
    .by = c(river, unit)
  ) |> 
  dplyr::mutate(
    nlarge = n > min_num_obs 
  ) 
  considered_stations = obs_status |> dplyr::filter(nlarge == TRUE)
  removed_stations = obs_status |> dplyr::filter(nlarge == FALSE)
  # cop_df only contains stations with more than threshold number of observations
  message(
    paste(length(considered_stations$unit), "stations considered")
  )
  message(
    paste(length(removed_stations$unit), "stations removed")
  )
  message("Removed station table:")
  table_details = data.frame(table(removed_stations$n))
  colnames(table_details) = c("n", "count")
  message(
    paste0(
      capture.output(table_details),
      collapse = "\n"
    )
  )
      
  return(cop_df |> dplyr::filter(unit %in% considered_stations$unit))
}


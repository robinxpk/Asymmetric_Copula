# Content for the interim presentation
# Approach:
# 1 Simulation (Gumbel copula)
# 1.1) Simulate from trivariate Gumbel NAC
# 1.2) Transform to non-univariate margins (Check paper for which distr. to choose)
# 2 Application
# 2.0) Visualize the data + Describtives
# 2.1) Methods to identify margins / PIT
# 2.2) Methods to fit copula / generator parameters
# 2.3) Methods to compare copula fits / Diagnostics

set.seed(123)

# Libraries ---------------------------------------------------------------
library(ggplot2) # For plots
library(patchwork) # For plots
# library(copula) # For copula methods; https://cran.r-project.org/web/packages/copula/index.html
# library(HAC) # For fitting which is NOT IMPLEMENTED in copula package. wtf is this

# Global parameters --------------------------------------------------------------
n = 200 # paper uses 5000 for simulation

# 1.1) Simulate from trivariate Gumbel NAC --------------------------------
theta_1 = 4
theta_2 = 14
# Define nested structure
c_gumbel = copula::onacopula(family = "G", nacStructure = C(theta_1, 3, C(theta_2, c(1, 2))))
# Simulate data
u_gumbel = copula::rnacopula(n, c_gumbel)
copula::splom2(u_gumbel, cex = 0.4)


# ---- Finding out what some in paper "NACs meet R" mentioned do
dim(c_gumbel) # number of variables
copula::allComp(c_gumbel) # order of variables in nested structure
copula::printNacopula(c_gumbel) # display NAC
copula::pnacopula(c_gumbel, c(0.5, 0.5, 0.5)) # cdf for given u vector
copula::rnacopula(n, c_gumbel) # generate random sample from copula


# 1.2) Transform to non-univarite margins ---------------------------------
# Distributions from real world data in the paper:
# Peak (P): Frechet with x_0 = 2539.692 (scale), theta = 4.045 (shape) 
frechet_param_x = 2539.692
frechet_param_theta = 4.045
# Density plot
ggplot(data = data.frame(x = c(1000, 8000)), aes(x = x)) + 
  stat_function(
    fun = VGAM::dfrechet,
    args = list(scale = frechet_param_x, shape = frechet_param_theta)
  )
# Volume (V): Gamma with 1/lambda = 141.976 (rate parameter), beta = 1.349 (shape parameter)
gamma_param_lam = 141.976
gamma_param_beta = 1.349
# Density plot
ggplot(data = data.frame(x = c(50, 200)), aes(x = x)) + 
  stat_function(
    fun = dgamma,
    args = list(shape = gamma_param_lam , rate = gamma_param_beta)
  )
# Duration (D): Lognormal with mu = 0.799, sigma^2 = (0.516)^2
lnorm_param_mu = 0.799
lnorm_param_sigma = 0.516
ggplot(data = data.frame(x = c(0, 10)), aes(x = x)) + 
  stat_function(
    fun = dlnorm,
    args = list(meanlog = lnorm_param_mu, sdlog = lnorm_param_sigma)
  )

# Create tibble 
dat = tibble::as_tibble(
  list(
    peak = VGAM::qfrechet(u_gumbel[, 1], scale = frechet_param_x, shape = frechet_param_theta),
    vol = qgamma(u_gumbel[, 2], rate = gamma_param_beta, shape = gamma_param_lam),
    dur = qlnorm(u_gumbel[, 3], meanlog = lnorm_param_mu, sdlog = lnorm_param_sigma)
  )
)




# 2.0 Data visualization and descriptives ---------------------------------
# Marginal distributions - histograms
marginal_peak = ggplot(dat, aes(x = peak)) +
  geom_histogram(fill = "blue", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Peak Discharge"),
    x = latex2exp::TeX("$m^3/s$", output = "expression"),
    y = "Number of Observations"
  ) + 
  theme_minimal()

marginal_dur = ggplot(dat, aes(x = dur)) +
  geom_histogram(fill = "green", alpha = 0.3, color = "black") +
  labs(
    title = latex2exp::TeX("Duration"),
    x = latex2exp::TeX("$days$", output = "expression"), 
    y = ""
  ) + 
  theme_minimal()
  
marginal_vol = ggplot(dat, aes(x = vol)) +
  geom_histogram(fill = "orange", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Total Volume"),
    x = latex2exp::TeX("$m^3$", output = "expression"),
    y = ""
  ) + 
  theme_minimal()

# Joint plot
pairs = GGally::ggpairs(dat, 
                upper = list(
                  continuous = GGally::wrap("points", alpha = 0.3)
                ),
                lower = list(
                  continuous = GGally::wrap(GGally::ggally_cor, method = "kendall")
                ),
                columnLabels = c("Peak", "Volume", "Duration")
)
# Adjust diagonal
pairs[1, 1] = marginal_peak
pairs[2, 2] = marginal_vol
pairs[3, 3] = marginal_dur
# Display 
pairs

# 2.1) Margin identification ----------------------------------------------
# Empirical PIT / Empirical Copula into fitting (non-parametric)
# Get pseudo-observations:
pdat = tibble::as_tibble(copula::pobs(dat))
# plot(pdat)
# # Compare to true sample:
# plot(tibble::as_tibble(u_gumbel))

# PIT visualization
# Left histogram
hist_pobs = ggplot(pdat, aes(x = peak)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.3, color = "black") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  coord_flip()

# Bottom histogram
hist_obs = ggplot(dat, aes(x = peak)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.3, color = "black") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

# Middle CDF
cdf_plot = ggplot(data = data.frame(x = c(1000, 8000)), aes(x = x)) +
  stat_function(
    fun = VGAM::pfrechet,
    args = list(scale = frechet_param_x, shape = frechet_param_theta),
    color = "black"
  ) +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

# Arrange plots using patchwork
layout = hist_pobs + cdf_plot + plot_spacer() + hist_obs +
  plot_layout(ncol = 2, widths = c(1, 4), heights = c(4, 1)) + 
  plot_annotation(
    title = "Parametric PIT for Peak",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Show the plot
print(layout)

# Previous plot after "PIT" transformation
marginal_peak = ggplot(pdat, aes(x = peak)) +
  geom_histogram(fill = "blue", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Peak Discharge"),
    x = latex2exp::TeX("$m^3/s$", output = "expression"),
    y = "Number of Observations"
  ) + 
  theme_minimal()

marginal_dur = ggplot(pdat, aes(x = dur)) +
  geom_histogram(fill = "green", alpha = 0.3, color = "black") +
  labs(
    title = latex2exp::TeX("Duration"),
    x = latex2exp::TeX("$days$", output = "expression"), 
    y = ""
  ) + 
  theme_minimal()
  
marginal_vol = ggplot(pdat, aes(x = vol)) +
  geom_histogram(fill = "orange", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Total Volume"),
    x = latex2exp::TeX("$m^3$", output = "expression"),
    y = ""
  ) + 
  theme_minimal()

# Joint plot
pairs = GGally::ggpairs(pdat, 
                upper = list(
                  continuous = GGally::wrap("points", alpha = 0.3)
                ),
                lower = list(
                  continuous = GGally::wrap(GGally::ggally_cor, method = "kendall")
                ),
                columnLabels = c("Peak", "Volume", "Duration")
)
# Adjust diagonal
pairs[1, 1] = marginal_peak + scale_y_continuous(breaks = c(), labels = c())
pairs[2, 2] = marginal_vol
pairs[3, 3] = marginal_dur

# Display 
pairs




# 2.2) Copula fitting -----------------------------------------------------

# ML estimation as in @yan p. 9
# Show how likelihood looks in my case (it is depicted in zhang)


# !!! estimation_via_HAC.R file; cannot fit onacopula via copula package, require HAC package
cop = copula::onacopula("Gumbel", C(1, 3, C(1, c(1, 2))))
hac_cop = HAC::nacopula2hac(cop)
plot(hac_cop)

hac.fix = HAC::estimate.copula(pdat |> dplyr::rename("1" = peak, "2" = vol, "3" = dur), hac = hac_cop)
hac.fix
plot(hac.fix)

hac.flex = HAC::estimate.copula(pdat |> dplyr::rename("1" = peak, "2" = vol, "3" = dur), type = hac_cop$type)
hac.flex
plot(hac.flex)
c_fit = HAC::hac2nacopula(hac.flex)
c_fit

# 2.3) Diagnostics / Goodness of fit / Comparisons ------------------------

# Typical gof things: 

# Cramer-von Mises statistic
# Rosenblatt transform
# Anderson-Darling statistic
# Empircal comparison
# Simulated data from fitted vs actual data
# Check other paper
# Out of sample performance / validation?

# compare CDF's between empirical copula and copula based on parametrical assumptions
# They shood look somewhat similar I guess
# (HAC doku Abbildung 6)

# Comparing the empirical PIT or empirical copula to theoretical counterparts helps assess the goodness 
# of fit of multivariate models.
# Actually, visual and then comparing by likelihood should work, right? 

# General thought: 
# Can I not just look at the nested copulas individually (like the likelihood does?)
# i.e. 1st look at the fitted copula between more correlated
#      2nd look at the fitted copula between copula values for inner 2 and pseudo obs of 3rd variable
# Is this valid? 
# -> How do other paper approach? 
# -> How are the gof metrics defined?  
# ALSO if I do this, I can use this damn copula shit package!
# General thoughts on validity: 
# Hierarchical strucutre implies that we model relationship inner independent of the outer one
# -> partial exchangeability
# But for sure it is valid to assess wheather a sub-copula is valid. And if that is the case
# it should be valid approach generally. lol. reasoning. 
outer_theta = c_fit@copula@theta
inner_theta = c_fit@childCops[[1]]@copula@theta

c_outer = copula::gumbelCopula(outer_theta)
c_inner = copula::gumbelCopula(inner_theta)

copula::gofCopula(c_inner, pdat[1:100, 1:2])

# Copula values of fitted joint copula
pdat["inner_cdf"] = copula::pCopula(as.matrix(pdat |> dplyr::select(peak, vol)), c_inner)

copula::gofCopula(c_outer, pdat[1:100, 3:4])

# Checking what happens if I estimate stuff sequentially
test = copula::fitCopula(copula::gumbelCopula(dim = 2), pdat[, 1:2])
summary(test)

inner = copula::pCopula(as.matrix(pdat |> dplyr::select(peak, vol)), copula::gumbelCopula(param = test@estimate, dim = 2))
test_pdat = tibble::as_tibble(
  list(dur = pdat$dur, inner = inner)
)

test_outer = copula::fitCopula(copula::gumbelCopula(dim = 2), test_pdat)
summary(test_outer)

# I THINK THE ÜARAMETER CONDITION ONLY APPLY IF SAME FAMILY!!!!
# p. 2 @hofert "On strucutre..."
# TODO: 
# Use HAC to estimate nested bc it ensures this condition to be fulfilled
# THEN: use estimates for bivariate ACs? 
# Then I can use some gof-evaluation of the bivariate stuff (I think)
# Remaining: Issue of not having the variance of the estimate

# 2.4 MAYBE Simulation study thingy ---------------------------------------
# Maybe let run for 1k times, each copula, each sample size, each method and give distribution of the error or something
# BEFORE doing so, check HAC doku. There is some simulation stuff in the end



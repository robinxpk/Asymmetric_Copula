# Goals / Ideas -----------------------------------------------------------
# - Can I improve the fit for the vine vs nac comparison and apply to every station? 
# Packages ----------------------------------------------------------------
source("functions.R")
library(ggplot2)


# Parameter ---------------------------------------------------------------
# Considered rivers
considered = c("Isar", "Donau")
# Selected station
station = "München" # Isar
# Reference year: The one year I consider in more detail
ref_year = 2024
# Quantile-threshold we use for definition of a flood event
copula_threshold = 0.75
# Minimum number of years / observations required to fit a copula
min_num_obs = 30
# Considered HQs
HQs = c(2, 5, 10, 20, 50, 100, 200, 500) # Return periods
HQ_probs = 1/HQs # Corresponding probability for return period
# Number of synthetic data
n_syn = 1000


# Load Data ---------------------------------------------------------------
cop_df = get_copula_df(p_threshold = copula_threshold) |>  
  dplyr::filter(river %in% considered) |>   # Focus only on desired rivers (i.e. remove this one random small river. lol.)
  filter_cop_df(min_num_obs)

  
# Station specific data
id = (cop_df |> dplyr::filter(unit == station))[1, "id"]
load(paste("../data/output/rdata/threshold_dfs/", id, "_t0.75.Rdata", sep = "")) # loads "df"
ref_yeardf = df |> dplyr::filter(year == ref_year)
peak_info = cop_df |> dplyr::filter(year == ref_year, unit == station)
scop_df = cop_df |> dplyr::filter(unit == station)

# All stations
all_units = unique(cop_df$unit)

# Correlation table
# Only Mittenwald has negative correlation. But judging by the scatter plot, it is pretty unclear
#   This "unclear" is supported by insignificant p-value. Also, consider pseudo-obs. They look like no real dependence at all
cor_table = cop_df |> dplyr::summarise(
  n = dplyr::n(),
  tau_vd = cor(volume, duration_min, method = "kendall"),
  tau_vp = cor(volume, peak, method = "kendall"),
  tau_dp = cor(duration_min, peak, method = "kendall"),
  .by = c(river, unit)
)

scop_df = cop_df |> dplyr::filter(unit == station)



# Descriptives ------------------------------------------------------------

## Bavaria Map ------------------------------------------------------------


# Fit models --------------------------------------------------------------
nacs = fit_nacs(cop_df, unique(cop_df$unit))
vines = fit_vines(cop_df, unique(cop_df$unit))


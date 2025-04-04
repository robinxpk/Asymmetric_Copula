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
# Minimum number of years / observations required to fit a copula
min_num_obs = 30
# Considered HQs
HQs = c(2, 5, 10, 20, 50, 100, 200, 500) # Return periods
HQ_probs = 1/HQs # Corresponding probability for return period
# Number of synthetic data
n_syn = 1000


# Load Data ---------------------------------------------------------------
cop_df = get_copula_df() |>  
  dplyr::filter(river %in% considered) |>   # Focus only on desired rivers (i.e. remove this one random small river. lol.)
  filter_cop_df(min_num_obs)
slopes = get_slope_df("../data/slopes/considered_slopes.csv")
cop_df = cop_df |> add_slope_col(slopes)
paper_tau = data.frame(
  value = rep(c(0.295, 0.462, 0.776), 2),
  tau = rep(c("Duration - Peak", "Volume - Duration", "Volume - Peak"), 2),
  river = c(rep("Isar", 3), rep("Donau", 3))
)
  
# Station specific data
id = (cop_df |> dplyr::filter(unit == station))[1, "id"]
load(paste("../data/output/rdata/threshold_dfs/", id, "_t.Rdata", sep = "")) # loads "df"
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
  tau_vd = cor(volume, duration_days, method = "kendall"),
  tau_vp = cor(volume, peak, method = "kendall"),
  tau_dp = cor(duration_days, peak, method = "kendall"),
  .by = c(river, unit, id)
) |> 
  add_tau_order_col() |> 
  add_slope_col(slopes)

scop_df = cop_df |> dplyr::filter(unit == station)

plot_theme = theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.position = "none",
    strip.text = element_text(size = 15)
  ) 



# Descriptives ------------------------------------------------------------


#   # Slopes -----------------------------------------------------------------
ggplot(cor_table, aes(x = tau_vp)) + 
  geom_boxplot(aes(fill = slope_cat)) + 
  facet_wrap(~river)
table(slopes$slope_cat)

# Slope by river
ggplot(cor_table, aes(x = slope, fill = river)) + 
  geom_density() +
  facet_wrap(~river)
# -> Isar seems to be more shallow

#   # Corr x Slopes --------------------------------------------------------

cor_table |> 
  tidyr::pivot_longer(
    cols = c(tau_vd, tau_vp, tau_dp),
    names_to = "tau",
    values_to = "val"
  ) |> 
  dplyr::mutate(
    tau = dplyr::case_when(
      tau == "tau_dp" ~ "Duration - Peak",
      tau == "tau_vd" ~ "Volume - Duration",
      tau == "tau_vp" ~ "Volume - Peak"
    ),
    tau = as.factor(tau)
  ) |> 
  ggplot() + 
  geom_boxplot(aes(y = val, x = tau, color = tau)) + 
  geom_point(data = paper_tau, aes(y = value, x = tau), color = "black", alpha = 0.7) + # Add paper taus
  geom_hline(yintercept = 0, linetype = 2) + 
  plot_theme +
  labs(
    y = latex2exp::TeX("$ \\widehat{\\tau}$"),
    x = ""
  ) + 
  ylim(-0.12, 1) + 
  # theme(legend.position = "left") + 
  facet_grid(~river)
# Donau and Isar seem to have a distinct correlation structure
# Isar looks somewhat like the paper does while Doanu is pretty different
with(cor_table, table(river, tau_order))
# The one station where Donau does not follow its usual correlation: Schwabelweis - 10062000

# Effect of slope onto peak, volume and duration
cop_df |> 
  ggplot(aes(x = peak, y = volume)) + 
  geom_point() + 
  facet_wrap(~unit)
  


# Effect of slope onto correlation structure, controlling for the river bc the rivers seem to differ (Isar more shallow than Danube)
cor_table |> 
  tidyr::pivot_longer(
    cols = c(tau_vd, tau_vp, tau_dp),
    names_to = "tau",
    values_to = "val"
  ) |> 
  ggplot(aes(x = slope, y = val)) + 
  geom_point() + 
  facet_wrap(~tau, scales = "free")


## Bavaria Map ------------------------------------------------------------


# Fit models --------------------------------------------------------------
nacs = fit_nacs(cop_df, unique(cop_df$unit))
vines = fit_vines(cop_df, unique(cop_df$unit))


source("functions.R")
source("load_data.R")

load("../data/output/presentation/considered_rivers.Rdata")
considered

# Analysis ----------------------------------------------------------------
cop_df = get_copula_df()
cop_df = cop_df |> 
  dplyr::filter(river %in% names(considered)) |> 
  dplyr::mutate(
    pobs_dur = copula::pobs(duration_min),
    pobs_peak = copula::pobs(peak),
    pobs_vol= copula::pobs(volume),
    .by = unit
  ) 
cop_df = filter_infeasible_stations(cop_df)

cop_df |> 
  dplyr::summarise(
    years = dplyr::n(),
    .by = c(river, unit)
  )

cop_df |> 
  dplyr::summarise(
    years = dplyr::n(),
    .by = c(river, unit)
  ) |> 
  dplyr::select(river) |> 
  table()

# One station -------------------------------------------------------------
# Focus on one station (munich <3) to consider fit and all
# In the end, draw some numerical GoF value
# This one can then be considered among all others, too? 
cop_df



# TODO: Can I always use empirical copula or do I ever require a parametric distributional assumption? 
#   That is, how do packages solve this? Adapt their approach (especially VineCopula and gamCopula package!)
station = cop_df |> dplyr::filter(unit == "München")
station



# All stations ------------------------------------------------------------
# Splines and all?


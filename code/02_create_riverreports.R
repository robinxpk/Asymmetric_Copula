# Load data into this file and then use the riverreport template to create a report for every river
source("functions.R")
# Load all copula data
copula_threshold = 0.75
# With: 
#   1) Reduce to considered rivers
considered = c("Isar", "Donau")
#   2) Remove stations with too little observations
min_num_obs = 30
cop_df = get_copula_df(p_threshold = copula_threshold) |>  
  dplyr::filter(river %in% considered) |>   # Focus only on desired rivers (i.e. remove this one random small river. lol.)
  filter_cop_df(min_num_obs)




# Rivers contained in the copula df
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

# Reduce computationale time for now
lapply(
  unique(cop_df$river),
  function(name) render_rivertemplate(
    df = cop_df |> dplyr::filter(river == name),
    out_dir = "../riverreports/"
  )
)


# # Some of the stations have VERY little observations...
# nobs_unit = cop_df |> dplyr::summarise(
#   n = dplyr::n(),
#   .by = unit
# ) |>
#   dplyr::mutate(
#     n_for_table = as.factor(dplyr::if_else(n > 30, ">30", as.character(n)))
#   )
# 
# table(nobs_unit$n_for_table)
# # Estimating (only) the dependence strucutre / copula, moderate number of obs required
# # -> Some stations have too few obs...
# # To estimate copula AND GLM, I can only use stations with MANY observations
# # -> Even fewer stations
# # Thus:
# # 1) Estimate copula and create the Bavaria plot based on these estimates. Remove those stations with 5 obs or smth
# # 2) Fit GLM(M) for the stations with many obs and interpret beta, no further bavaria plot here
# 
# 
# # Focus on one station to fit copula
# # For one station I would like to thoroughly look into the results. After that, I take what is selcted by the packages
# # This saves space and time when creating the river reports
# # Munich Isar station <3
# # station = cop_df |> dplyr::filter(unit == "München")
# # nrow(station)
# 
# # NAC (HAC package)
# # List of all copulas
# hac_list = lapply(
#   hac_types,
#   function(t){
#     HAC::estimate.copula(
#       as.matrix(station |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)),
#       method = 3, # aparently method 3 is most accurate, but infeasible for high dims (see HAC p6. iii))
#       type = t
#     )
#   }
# )
# hac_list
# 
# # # Copula suggested by hac-package (WHAT METHOD DID THEY USE?)
# # hac = HAC::estimate.copula(
# #   as.matrix(station |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)),
# #   method = 3 # aparently method 3 is most accurate, but infeasible for high dims (see HAC p6. iii))
# # )
# # print(hac)
# # plot(hac)
# # 
# # # Simulate data from these and compare with observed
# # rdraw = HAC::rHAC(n = 5000, hac = hac)
# # 
# # ggplot(mapping = aes(x = pobs_dur, y = pobs_vol)) +
# #   geom_point(data = rdraw, colour = "lightblue") +
# #   geom_point(data = cop_df)
# # 
# # ggplot(mapping = aes(x = pobs_dur, y = pobs_peak)) +
# #   geom_point(data = rdraw, colour = "lightblue") +
# #   geom_point(data = cop_df)
# # 
# # ggplot(mapping = aes(x = pobs_vol, y = pobs_peak)) +
# #   geom_point(data = rdraw, colour = "lightblue") +
# #   geom_point(data = cop_df)
# # 
# # # Vines
# # # gamCopula::gamVineSeqFit()
# # # gamCopula::gamVineCopSelect()
# # vine = gamCopula::gamVineStructureSelect(udata = as.matrix(station |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)), simplified = FALSE)
# # 
# # rdraw_vine = gamCopula::gamVineSimulate(n = 5000, GVC = vine)
# # 
# # ggplot(mapping = aes(x = pobs_dur, y = pobs_vol)) +
# #   geom_point(data = rdraw_vine, colour = "lightblue") +
# #   geom_point(data = cop_df)
# # 
# # ggplot(mapping = aes(x = pobs_dur, y = pobs_peak)) +
# #   geom_point(data = rdraw_vine, colour = "lightblue") +
# #   geom_point(data = cop_df)
# # 
# # ggplot(mapping = aes(x = pobs_vol, y = pobs_peak)) +
# #   geom_point(data = rdraw_vine, colour = "lightblue") +
# #   geom_point(data = cop_df)
# # 
# # # Bavaria Headmap
# 
# 
# 
# 
# 

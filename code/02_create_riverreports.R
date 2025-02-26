# Load data into this file and then use the riverreport template to create a report for every river
source("functions.R")
# Implemented HAC types
# • 1 = HAC Gumbel
# • 2 = AC Gumbel
# • 3 = HAC Clayton
# • 4 = AC Clayton
# • 5 = HAC Frank
# • 6 = AC Frank
# • 7 = HAC Joe
# • 8 = AC Joe
# • 9 = HAC Ali-Mikhail-Haq
# • 10 = AC Ali-Mikhail-Haq
# For fitting all HACs:
hac_types = list(
  "HAC Gumbel" = 1,
  "AC Gumbel" = 2,
  "HAC Clayton" = 3,
  "AC Clayton" = 4,
  "HAC Frank" = 5,
  "AC Frank" = 6,
  "HAC Joe" = 7,
  "AC Joe" = 8
)

# Load all copula data
cop_df = get_copula_df()
# Extend by pseudo observations
cop_df = cop_df |> 
  dplyr::mutate(
    pobs_dur = copula::pobs(duration_min),
    pobs_peak = copula::pobs(peak),
    pobs_vol= copula::pobs(volume),
    .by = unit
  ) 

# Rivers contained in the copula df
unique(cop_df$river)

# Reduce computationale time for now
# lapply(
#   unique(cop_df$river),
#   function(name) render_rivertemplate(
#     df = cop_df |> dplyr::filter(river == name),
#     out_dir = "../riverreports/"
#   )
# )


# Some of the stations have VERY little observations... 
nobs_unit = cop_df |> dplyr::summarise(
  n = dplyr::n(),
  .by = unit
) |> 
  dplyr::mutate(
    n_for_table = as.factor(dplyr::if_else(n > 30, ">30", as.character(n)))
  )
  
table(nobs_unit$n_for_table)
# Estimating (only) the dependence strucutre / copula, moderate number of obs required
# -> Some stations have too few obs...
# To estimate copula AND GLM, I can only use stations with MANY observations
# -> Even fewer stations
# Thus: 
# 1) Estimate copula and create the Bavaria plot based on these estimates. Remove those stations with 5 obs or smth
# 2) Fit GLM(M) for the stations with many obs and interpret beta, no further bavaria plot here


# Focus on one station to fit copula
# For one station I would like to thoroughly look into the results. After that, I take what is selcted by the packages 
# This saves space and time when creating the river reports
# Munich Isar station <3 
station = cop_df |> dplyr::filter(unit == "München")
nrow(station)

# NAC (HAC package)
# List of all copulas
hac_list = lapply(
  hac_types,
  function(t){
    HAC::estimate.copula(
      as.matrix(station |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)),
      method = 3, # aparently method 3 is most accurate, but infeasible for high dims (see HAC p6. iii))
      type = t
    )
  }
)
hac_list

# Copula suggested by hac-package (WHAT METHOD DID THEY USE?)
hac = HAC::estimate.copula(
  as.matrix(station |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)),
  method = 3 # aparently method 3 is most accurate, but infeasible for high dims (see HAC p6. iii))
)
print(hac)
plot(hac)

# Simulate data from these and compare with observed
rdraw = HAC::rHAC(n = 5000, hac = hac)

ggplot(mapping = aes(x = pobs_dur, y = pobs_vol)) + 
  geom_point(data = rdraw, colour = "lightblue") + 
  geom_point(data = cop_df) 

ggplot(mapping = aes(x = pobs_dur, y = pobs_peak)) + 
  geom_point(data = rdraw, colour = "lightblue") + 
  geom_point(data = cop_df) 

ggplot(mapping = aes(x = pobs_vol, y = pobs_peak)) + 
  geom_point(data = rdraw, colour = "lightblue") + 
  geom_point(data = cop_df) 

# Vines
# gamCopula::gamVineSeqFit()
# gamCopula::gamVineCopSelect()
vine = gamCopula::gamVineStructureSelect(udata = as.matrix(station |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)), simplified = FALSE)
  
rdraw_vine = gamCopula::gamVineSimulate(n = 5000, GVC = vine)

ggplot(mapping = aes(x = pobs_dur, y = pobs_vol)) + 
  geom_point(data = rdraw_vine, colour = "lightblue") + 
  geom_point(data = cop_df) 

ggplot(mapping = aes(x = pobs_dur, y = pobs_peak)) + 
  geom_point(data = rdraw_vine, colour = "lightblue") + 
  geom_point(data = cop_df) 

ggplot(mapping = aes(x = pobs_vol, y = pobs_peak)) + 
  geom_point(data = rdraw_vine, colour = "lightblue") + 
  geom_point(data = cop_df) 

# Bavaria Headmap







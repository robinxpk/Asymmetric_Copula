library(ggplot2)
# Used to load the data
source("load_data.R")

cop_df = get_copula_df()

# Plots
ggplot(cop_df, aes(x = peak, y = volume)) + 
  geom_point() + 
  facet_wrap(~unit)

ggplot(cop_df, aes(x = duration_min, y = volume)) + 
  geom_point() + 
  facet_wrap(~unit)

ggplot(cop_df, aes(x = duration_min, y = peak)) + 
  geom_point() + 
  facet_wrap(~unit)

# TODO: 
# Analyze just one station for now
# Later on, put all of this into a function
# Let function create Rmd(?) file with "report" for a given station

############## Single station analysis (munich<3)

df = cop_df |> dplyr::filter(unit == "München")




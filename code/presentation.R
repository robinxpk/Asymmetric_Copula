# Code to generate plots for presentation 
# Packages and Dependencies -----------------------------------------------
source("functions.R")
library(ggplot2)

# Params
# When (re)running the script, should plots be saved
save_plots = FALSE
# Selected station
# station = "München" # Isar
station = "Hofkirchen" # Donau
# station = "Sylvenstein"
ref_year = 2024
# Considered rivers 

# Load data frames
# Copula df with threshold of choice
# copula_threshold = 0.9
# With: 
#   1) Reduce to considered rivers
considered = c("Isar", "Donau")
#   2) Remove stations with too little observations
min_num_obs = 30
cop_df = get_copula_df() |>  
  dplyr::filter(river %in% considered)  # Focus only on desired rivers (i.e. remove this one random small river. lol.)
# Of which have more than 30 observations
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
cop_df = cop_df |> dplyr::filter(unit %in% considered_stations$unit) 
  
# > pos
# unit              east   north         river
# Neu Ulm, Bad Held 573054 5360044         Donau
pos = cop_df |> dplyr::select(unit, east, north, river) |> unique()

# Copula df with all thresholds
# all_cops = get_copula_df(all = TRUE) |> dplyr::filter(river %in% considered)

# Station specific data
id = (cop_df |> dplyr::filter(unit == station))[1, "id"]
load(paste("../data/output/rdata/threshold_dfs/", id, "_t.Rdata", sep = "")) # loads "df"
ref_yeardf = df |> dplyr::filter(year == ref_year)
peak_info = cop_df |> dplyr::filter(year == ref_year, unit == station)
scop_df = cop_df |> dplyr::filter(unit == station)

# All stations
all_units = unique(cop_df$unit)
# Number of stations by river
table(considered_stations$river)

# Grimaldi paper p. 1164 gives their observed tau values
#   Sadly, they do not really specify which of these rivers it refers to or if its an average or smth
paper_tau = data.frame(
  value = rep(c(0.295, 0.462, 0.776), 2),
  tau = rep(c("Duration - Peak", "Volume - Duration", "Volume - Peak"), 2),
  river = c(rep("Isar", 3), rep("Donau", 3))
)

# Correlation table
# Only Mittenwald has negative correlation. But judging by the scatter plot, it is pretty unclear
#   This "unclear" is supported by insignificant p-value. Also, consider pseudo-obs. They look like no real dependence at all
cor_table = cop_df |> dplyr::summarise(
  n = dplyr::n(),
  tau_vd = cor(volume, duration_days, method = "kendall"),
  # p_vd = cor.test(volume, duration_min, method = "kendall")$p.value,
  # rej_vd = p_vd < 0.01,
  tau_vp = cor(volume, peak, method = "kendall"),
  # p_vp = cor.test(volume, peak, method = "kendall")$p.value,
  # rej_vp = p_vp < 0.01,
  tau_dp = cor(duration_days, peak, method = "kendall"),
  # p_dp = cor.test(duration_min, peak, method = "kendall")$p.value,
  # rej_dp = p_dp < 0.01,
  .by = c(river, unit)
)

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

# Number of synthetic data points drawn
n_syn = 1000
scop_df = cop_df |> dplyr::filter(unit == station)

# Probabilities for HQs
HQs = c(2, 5, 10, 20, 50, 100, 200, 500) # Return periods
HQ_probs = 1/HQs # Corresponding probability for return period

# Our considered got drastically reduced, for the presentation at least. So reduce rivers for now
# TODO: Define this function and let it run during load_data when all data available
# create_and_save_position(
#   in_dir = 
# )
# df = get_long_df("../data/output/rdata/extended_dfs/")
# pos = df |> dplyr::summarise(
#   unit = unit[1],
#   east = pos_east[1],
#   north = pos_north[1],
#   river = river[1],
#   .by = unit
# )
# save(pos, file = "../data/output/presentation/pos.Rdata")


# Map Data ----------------------------------------------------------------
load("../data/output/presentation/rivers.Rdata")
pos_sf = gkd2gg(pos, coord_cols = c("east", "north")) |> 
  dplyr::filter(river %in% considered) |> 
  dplyr::mutate(
    river_station = dplyr::case_when(
      river == "Isar" ~ "Isar_Station",
      river == "Donau" ~ "Donau_Station"
    )
  )


# Get bavaria and its rivers 
germany = rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
bayern = germany |> dplyr::filter(name == "Bayern")
bayern_bbx = sf::st_bbox(bayern) # Create bounding box for openstreetmap

# # See 2.1 at https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# rivers = osmdata::opq(bayern_bbx, timeout = 180) |> 
#   osmdata::add_osm_feature(key = "waterway", value = "river") |>      
#   # osmdata::add_osm_feature(key = "width", value = "100", value_exact = FALSE) |>
#   # osmdata::add_osm_feature(
#   #   key = "name",
#   #   value = c("Danube", "Main", "Isar", "Inn", "Schwarzach", "Vils"),
#   #   match_case = FALSE,
#   #   key_exact = FALSE,
#   #   value_exact = FALSE
#   # ) |>
#   osmdata::osmdata_sf()
# # save(rivers, file = "../data/output/presentation/rivers.Rdata")

# Create sf object based on the queried osm object
rivers_sf = rivers$osm_lines

# Cut rivers so they fit into Bayern
rivers_st = sf::st_transform(rivers_sf, sf::st_crs(bayern))
rivers_bayern = sf::st_intersection(rivers_st, bayern)
# save(rivers_bayern, file = "../data/output/presentation/rivers_bayern.Rdata")

# Add identificator for selection of relevant rivers
rivers_bayern = rivers_bayern |> 
  dplyr::mutate(
    river = dplyr::case_when(
      name == "Isar" ~ "Isar",
      name == "Donau" ~ "Donau",
      TRUE ~ "Other"
    )
  )


# Presentation - Data -----------------------------------------------------
# Bavaria plots -----------------------------------------------------------

# NOTE: Plots may look super bad in RStudio. I save them using savegg. 
#   Check the saved file or its look in PowerPoint to judge visual
# Selected rivers and all stations 
bayern_plot = ggplot(rivers_bayern) + 
  # Displaying the actual data
  # 1) Make background look like map
  ggspatial::annotation_map_tile(type = "osm", zoom = 8) + 
  # 2) Add the rivers twice. 
  #   1st in black to give a outline for each river
  #   (combination of fill and color does not work for geom_sf)
  #   2nd the actual rivers in desired colors
  geom_sf(color = "black", linewidth = 2) + # Outlines for the rivers
  geom_sf(aes(color = river, fill = river), linewidth = 1.5) +
  # 3) Add the shape of Bayern on top to highlight the relevant area
  geom_sf(data = bayern, fill = "white", alpha = 0.4, linewidth = .8) + 
  # 4) Add station
  geom_sf(data = pos_sf, aes(color = river_station, shape = river_station, fill = river_station), size = 4) + 
  # Formatiing of Data
  theme_void() + 
  theme(
    # Remove the x and y ticks / labels
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # Adjust the legend position and font size
    legend.position = c(1, 1),  # Move legend to top-right
    legend.justification= c(1, 1),  # Align legend inside the top-right
    legend.background = element_rect(fill = "white", color = "black", linewidth = .5),
    legend.box = "vertical",
    legend.margin = margin(1, 1, 1, 1),  # (top, right, bottom, left) padding
    legend.text = element_text(size = 20),  # Adjust size of legend labels
    legend.title = element_text(size = 24, face = "bold")
  ) + 
  # The color aes is actually what colors the rivers and stations, but the legend is super ugly
  # So I use the fill legend and do not display the color legend
  scale_color_manual(
    name = "River",
    values = c(
      "Donau" = "#7CAE00", "Isar" = "#F8766D", "Other" = "#00BfC4", # River colors
      "Donau_Station" = "darkgreen", "Isar_Station" = "darkred" # Station colors
    ), 
    guide = "none" # Do not show color legend
  ) +
  scale_fill_manual(
    name = "River",
    values = c("Donau" = "#7CAE00", "Isar" = "#F8766D", "Other" = "#00BfC4"),
    labels = c("Donau" = "Donau", "Isar" = "Isar", "Other" = "Other")
  ) +
  scale_shape_manual(
    name = "Station",
    values = c("Donau_Station" = 16, "Isar_Station" = 17),
    labels = c("Donau_Station" = "Donau", "Isar_Station" = "Isar")
  ) +
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(
      color = c("darkgreen", "darkred")
      ),
      order = 2
    )
  )
bayern_plot
if(save_plots) savegg("bayern_rivers")

# Select one station to explain the data process on
bayern_plot + 
  geom_sf(data = pos_sf |> dplyr::filter(unit == station), color = "black", size = 10, shape = 1, alpha = 1) + 
  geom_sf_label(
    data = pos_sf |> dplyr::filter(unit == station), 
    aes(label = unit),
    nudge_x = .8,
    size = 10,
    fill = "white"
  ) 

# ggplot(rivers_bayern) + 
#   # Displaying the actual data
#   # 1) Make background look like map
#   ggspatial::annotation_map_tile(type = "osm", zoom = 8) + 
#   # 2) Add the rivers twice. 
#   #   1st in black to give a outline for each river
#   #   (combination of fill and color does not work for geom_sf)
#   #   2nd the actual rivers in desired colors
#   geom_sf(color = "black", linewidth = 2) + # Outlines for the rivers
#   geom_sf(aes(color = river, fill = river), linewidth = 1.5) +
#   # 3) Add the shape of Bayern on top to highlight the relevant area
#   geom_sf(data = bayern, fill = "white", alpha = 0.4, linewidth = .8) + 
#   # 4) Add station
#   geom_sf(data = pos_sf, aes(color = river_station, shape = river_station, fill = river_station), size = 4) + 
#   # Add highlight for selected station
#   geom_sf(data = pos_sf |> dplyr::filter(unit == station), color = "black", size = 10, shape = 1, alpha = 1) + 
#   geom_sf_label(
#     data = pos_sf |> dplyr::filter(unit == station), 
#     aes(label = unit),
#     nudge_x = .8,
#     size = 10,
#     fill = "white"
#   ) + 
#   # Prettify
#   theme_void() + 
#   theme(
#     # Remove the x and y ticks / labels
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     # Adjust the legend position and font size
#     legend.position = c(1, 1),  # Move legend to top-right
#     legend.justification= c(1, 1),  # Align legend inside the top-right
#     legend.background = element_rect(fill = "white", color = "black", linewidth = .5),
#     legend.box = "vertical",
#     legend.margin = margin(1, 1, 1, 1),  # (top, right, bottom, left) padding
#     legend.text = element_text(size = 20),  # Adjust size of legend labels
#     legend.title = element_text(size = 24, face = "bold")
#   ) + 
#   # The color aes is actually what colors the rivers and stations, but the legend is super ugly
#   # So I use the fill legend and do not display the color legend
#   scale_color_manual(
#     name = "River",
#     values = c(
#       "Donau" = "#7CAE00", "Isar" = "#F8766D", "Other" = "#00BfC4", # River colors
#       "Donau_Station" = "darkgreen", "Isar_Station" = "darkred" # Station colors
#     ), 
#     guide = "none" # Do not show color legend
#   ) +
#   scale_fill_manual(
#     name = "River",
#     values = c("Donau" = "#7CAE00", "Isar" = "#F8766D", "Other" = "#00BfC4"),
#     labels = c("Donau" = "Donau", "Isar" = "Isar", "Other" = "Other")
#   ) +
#   scale_shape_manual(
#     name = "Station",
#     values = c("Donau_Station" = 16, "Isar_Station" = 17),
#     labels = c("Donau_Station" = "Donau", "Isar_Station" = "Isar")
#   ) +
#   guides(
#     fill = guide_legend(order = 1),
#     shape = guide_legend(override.aes = list(
#       color = c("darkgreen", "darkred")
#       ),
#       order = 2
#     )
#   )
if(save_plots) savegg("bayern_river_station_highlighted")


# Data Process Explained - Munich -----------------------------------------

# Data process explained
# Descriptives for selected station
# Number of years
# As we see later, these are the number of observations we have per station
length(unique(df$year))

# We split hydrograph into years
ggplot(df) +
  geom_rect(
    data = df |> 
      dplyr::filter(month == 1, day == 1, min == 0, hour == 0) |> 
      dplyr::select(date) |> 
      dplyr::mutate(
        start = dplyr::lag(date),
        color = as.factor(dplyr::if_else(lubridate::year(start) %% 2 == 0, "white", "darkgray"))
      ) |> 
      dplyr::rename(end = date) |>  
      dplyr::filter(!is.na(start)),
    mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = color),
    alpha = 0.5
  ) + 
  geom_line(aes(x = date, y = discharge)) +
  scale_fill_identity() + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 20)
  ) + 
  scale_x_datetime(
    breaks = c(
      seq.POSIXt(min(df$date), max(df$date), by = "5 year"),
      as.POSIXct("2025-01-01 01:00:00")
    ),
    date_labels = "%Y",
    limits = range(df$date)
  ) + 
  labs(
    title = "Full-Period-Hydrograph Munich", 
    x = expression(Date),
    y = expression(Discharge ~ ( m^3 / s))
  )  
if(save_plots) savegg("station_full_hydrograph", width = 10, height = 5)
# # Hydrograph per year - stacked
# ggplot(df, aes(x = doy, y = discharge, group = year)) +
#   geom_line()
# Use munich 2024 and remind of the crazy isar time

# Hydropgraph for reference year
# ggplot(df |> dplyr::filter(year == ref_year), aes(x = doy, y = discharge)) +
ggplot(ref_yeardf, aes(x = doy, y = discharge)) +
  geom_line() + 
  scale_x_continuous(breaks = c(0, 100, 200, 300, 365)) +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 20)
  ) + 
  labs(
    title = "Hydrograph Munich 2024", 
    x = "Day of the Year",
    y = expression(Discharge ~ ( m^3 / s))
  )
if(save_plots) savegg("hydrograph_munich_2024", width = 10, height = 5)
# We consider the flood event with the highest peak only (following paper here)
# Flood start and end are obtained by straight line method
# Identifying using quantile of yearly distribution
# ggplot(mapping = aes(x = doy, y = discharge)) +
#   geom_line(data = ref_yeardf, color = "darkgreen") +
#   geom_line(data = ref_yeardf |> dplyr::filter(peak_flood == TRUE), color = "red") +
#   geom_hline(yintercept = ref_yeardf$threshold[[1]])
# Visual description of the variable we consider
ggplot(mapping = aes(x = doy, y = discharge)) +
  geom_line(data = ref_yeardf, color = "darkgreen") +
  geom_line(data = ref_yeardf |> dplyr::filter(peak_flood == TRUE), color = "red") +
  geom_point(data = ref_yeardf |> dplyr::filter(discharge == peak_info$peak) |> head(n = 1)) + 
  geom_ribbon(
    data = ref_yeardf |> dplyr::filter(peak_flood == TRUE), 
    aes(ymin = rep(0, ref_yeardf |> dplyr::filter(peak_flood == TRUE) |> nrow()), ymax = discharge), 
    alpha = 0.2,
    fill = "red"
  ) + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 20)
  ) + 
  scale_x_continuous(breaks = c(0, 100, 200, 300, 365)) +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +
  labs(
    title = "Straight Line Method", 
    x = "Day of the Year",
    y = expression(Discharge ~ ( m^3 / s))
  )
if(save_plots) savegg("straight_line_method", width = 10, height = 5)

# Values seen:
cop_df |> dplyr::filter(unit == station, year == ref_year) |> 
  dplyr::select(
    duration_days, peak, volume
  ) |> 
  dplyr::mutate(
    volume_inmio = volume / 1e6
  )

# Mention that not all hydrographs were as nice and our filter criteria
# Potentially also, how we derived them. Maybe put the corresponding plots from README into the appendix and tell the others to ask 
# -> i.e. questions for asking session

# Plot Scatter, Bar and tau for Station
marginal_peak = ggplot(scop_df, aes(x = peak)) +
  geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Munich Station: Peak Discharge"),
    x = latex2exp::TeX("Peak ($m^3/s$)"),
    y = "Number of Observations"
  ) + 
  theme_minimal()

marginal_dur = ggplot(scop_df, aes(x = duration_days)) +
  geom_histogram(fill = "green", alpha = 0.3, color = "black") +
  labs(
    title = latex2exp::TeX("Munich Station: Duration"),
    x = latex2exp::TeX("Duration ($Days$)", output = "expression"), 
    y = ""
  ) + 
  theme_minimal()
  
marginal_vol = ggplot(scop_df, aes(x = volume / 1e6)) +
  geom_histogram(fill = "orange", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Munich Station: Total Volume"),
    x = latex2exp::TeX("Volume (Mio. $m^3$)", output = "expression"),
    y = ""
  ) + 
  theme_minimal()


pairs = GGally::ggpairs(
  cop_df |> 
    dplyr::filter(unit == station) |> 
    dplyr::mutate(
      duration_day = duration_days,
      volume_mio = volume / 1e6
    ) |> 
    dplyr::select(duration_day, volume_mio, peak), 
  lower = list(
  continuous = GGally::wrap("points", alpha = 0.8)
  ),
  upper = list(
  continuous = GGally::wrap(GGally::ggally_cor, method = "kendall")
  ),
  columnLabels = c("Duration (day)", "Volume (Mio)", "Peak")
  ) + theme(
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20)
  )
# Adjust diagonal
pairs[1, 1] = marginal_dur + 
  theme(axis.ticks.y = element_line())
pairs[2, 2] = marginal_vol
pairs[3, 3] = marginal_peak + 
  theme(axis.ticks.x = element_line())
# Display 
pairs
if(save_plots) savegg("munich_marginals", width = 10, height = 5)



# All Stations - Table ----------------------------------------------------
# Majority of stations have 54 observations, only 2 have below 50: 44 and 48 observations
table(considered_stations$n)

print(paste("Total number of flood events:", cop_df |> nrow()))

l_in_bathtub = 302
l_therme_erdingen = 3.3e6
cop_df |> 
  dplyr::summarise(
    max_peak = max(peak),
    max_vol = max(volume),
    max_dur = max(duration_days),
    .by = river
  ) |> dplyr::mutate(
    max_peak_tub = max_peak / l_in_bathtub,
    max_vol_in_therme_erdingen = max_vol / l_therme_erdingen,
    max_vol_in_mio_l = max_vol / 1e6
  )
# Duration of the maximum volume floodings
cop_df |> dplyr::filter(volume %in% c(6824363400, 1808050500)) |> dplyr::mutate(duration_days = duration_days)
cop_df |> dplyr::mutate(duration_days = duration_days) |> dplyr::filter(duration_days > 87)


# All Stations - Corr Boxplots --------------------------------------------
# -> Taus Boxplots (correlation tiles already hinted towards this behavior)
# Boxplot
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
  theme(
    legend.position = "none",
    strip.text = element_text(size = 15)
  ) + 
  labs(
    y = latex2exp::TeX("$ \\widehat{\\tau}$"),
    x = ""
  ) + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20)
  ) + 
  ylim(-0.12, 1) + 
  facet_wrap(~ river )
if(save_plots) savegg("tau_boxplots", width = 15, height = 5)

# Dependence structure by threshold
all_cops |> dplyr::summarise(
  n = dplyr::n(),
  tau_vd = cor(volume, duration_days, method = "kendall"),
  tau_vp = cor(volume, peak, method = "kendall"),
  tau_dp = cor(duration_days, peak, method = "kendall"),
  .by = c(river, unit, p_threshold)
) |> 
  dplyr::filter(n > 30, river == "Isar") |> 
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
  geom_line(aes(y = val, x = p_threshold, color = tau)) + 
  # geom_point(data = paper_tau, aes(y = value, x = p_), color = "black", alpha = 0.7) + # Add paper taus
  geom_hline(yintercept = 0, linetype = 2) + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 15)
  ) + 
  labs(
    y = latex2exp::TeX("$ \\widehat{\\tau}$"),
    x = ""
  ) + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20)
  ) +
  ylim(-.5,  1) + 
  facet_wrap(~ unit)
if (save_plots) savegg("tau_for_different_thresholds_Isar", width = 10, height = 5)


# Methods -----------------------------------------------------------------
# Generator function plots
tau = list(low = 0.1, high = 0.7)
source("functions.R")
showcase_copula_contours(tau$low, title = latex2exp::TeX(paste("Copula Families ($\\tau =", tau$low, "$)")), textsize_lab = 20, textsize_tick = 10, textsize_strip = 10)
if(save_plots) savegg("CopFams_lowtau", width = 10, height = 5)
showcase_copula_contours(tau$high, title = latex2exp::TeX(paste("Copula Families ($\\tau =", tau$high, "$)")), textsize_lab = 20, textsize_tick = 10, textsize_strip = 10)
if(save_plots) savegg("CopFams_hightau", width = 10, height = 5)



# Simulation --------------------------------------------------------------

# NAC Sim -----------------------------------------------------------------
all_res_nacs = read_dep_files(true_dep = "nac", in_dir = "../data/simulation/")

# 
with(all_res_nacs, table(cop, n, river))


# KLD plot for all copula families bc separating by copula family did not make any difference
all_res_nacs |> 
  dplyr::select(n, contains("_kl")) |> 
  tidyr::pivot_longer(
    cols = contains("_kl"),
    names_to = "dep",
    values_to = "kld"
  ) |>
  dplyr::mutate(dep = as.factor(stringr::str_remove(dep, "_kl"))) |>
  ggplot() + 
  geom_density(aes(x = kld, fill = dep), color = "black", alpha = 0.3) +
  geom_vline(xintercept = 0, color = "black") + 
  facet_wrap(~ n, scale = "free", axis.labels = "margins") + # Cannot remove margins because scales are free
  labs(
    title = "NAC DGP: KLD by Sample Size",
    x = "Kullback Leibler Divergence",
    y = "Density"
  ) +
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 15)  # Increase legend text size
  ) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(name = "Model", values = c("ac" = "red", "nac" = "green", "vine" = "blue"))
if (save_plots) savegg("sim_NACDGP_KLD", width = 10, height = 5)
# Note: each n plot contains 
all_res_nacs |> dplyr::filter(n == 15) |> nrow()
# That is, 3 * 3000 (3k observations for each copula family --> 750 obs per correlation structure)

# Validation of what paper found
all_res_nacs |>
  dplyr::mutate(seed = as.numeric(seed)) |>
  dplyr::group_by(river, n) |>
  dplyr::mutate(x = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(paper_ratio = 1 / 3 * (true_tau_inner - true_tau_outer) + true_tau_outer) |>  # l* rearrange s.t. tau_sym = f(l*)
  ggplot() +
  geom_ribbon(aes(ymin = true_tau_inner, ymax = true_tau_outer, x = x), fill = "lightblue", alpha = 0.2, color = "black") +
  geom_line(aes(y = ac_tau, x = x), color = "blue") +
  geom_line(aes(y = paper_ratio, x = x), color = "red") + 
  theme_minimal() +
  facet_grid(river ~ n, scale = "free_x", axes = "margins")
if (save_plots) savegg("paper ratio confirmed", width = 10, height = 5)
# Each column contains 9k observations
# Length of river differ: 
#   I drew correlation structure on random as it was easier to implement. Especially in the vine case 
#   (Instead of 3 nested for loops, I just drew 3 random numbers / families)
#   Since these draws were uniform, I expect ~9k observation for each river. Realized values:
# with(all_res_nacs, table(river))
    # Donau        Isar LowHighHigh  LowMedHigh 
    # 8676        9708        8604        9012 
# Also, since I used a seed to ensure reproducablity and drew the correlation structure first, each n has the same distribution of correlation structure
addmargins(with(all_res_nacs, table(river, n)))
# Each entry contains ~ 3 * 750 observations (due to the choice of my iterations per setup...)


# Vine Sim ----------------------------------------------------------------
all_res_vines = read_dep_files(true_dep = "vine", in_dir = "../data/simulation/") |>
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

all_res_vines |>
    dplyr::select(n, river, contains("_kl")) |>
    tidyr::pivot_longer(
      cols = contains("_kl"),
      names_to = "dep",
      values_to = "kld"
    ) |>
  dplyr::filter(dep != "ac_kl") |> # Remove AC density cause nobody cares for AC
  ggplot() +
  geom_density(aes(x = kld, fill = dep), color = "black", alpha = 0.3) +
  facet_wrap(~ river, scale = "free") +
  geom_vline(xintercept = 0, color = "black") + 
  labs(
    title = "Vine DGP: KLD by Correlation Values",
    x = "Kullback Leibler Divergence",
    y = "Density"
  ) +
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 15)  # Increase legend text size
  ) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    name = "Model", 
    values = c("ac_kl" = "red", "nac_kl" = "green", "vine_kl" = "blue"),
    labels = c("ac_kl" = "ac", "nac_kl" = "nac", "vine_kl" = "vine")
  )
if (save_plots) savegg("sim_VineDGP_kld", width = 10, height = 5)



all_res_vines |> 
  dplyr::mutate(
    x = dplyr::row_number(),
    .by = c(river, n)
  ) |> 
  ggplot() +
  geom_line(aes(y = true_tau_12, x = x)) + 
  geom_line(aes(y = true_tau_23, x = x)) + 
  geom_line(aes(y = true_tau_13, x = x)) + 
  geom_line(aes(y = nac_tau_outer, x = x), color = "blue", alpha = 0.4) + 
  geom_line(aes(y = nac_tau_inner, x = x), color = "red", alpha = 0.4) + 
  facet_grid(river ~ n, scale = "free_x")
if (save_plots) savegg("VineDGP_NACs applied Lineplot", width = 10, height = 5)


all_res_vines |> 
  dplyr::mutate(
    inner_large = nac_tau_inner / true_tau_12, # tau_12 is largest (tree: 1 - 2 - 3)
    outer_med = nac_tau_outer / true_tau_23, # tau_23 is 2nd largest / medium dependence strength
    outer_small = nac_tau_outer / true_tau_13,
    .b = river
  ) |> 
  tidyr::pivot_longer(
    cols = c(inner_large, outer_med, outer_small),
    values_to = "val",
    names_to = "nest"
  ) |> 
  ggplot() + 
  geom_boxplot(aes(y = val, color = nest)) + 
  geom_hline(yintercept = 1, alpha = .7, linetype = 2) + 
  facet_grid(river ~ n)
if (save_plots) savegg("VineDGP_NACs applied Boxplot", width = 10, height = 5)

# Results -----------------------------------------------------------------
# NACs
nacs = fit_nacs(cop_df, all_units)
# Vines
vines = fit_vines(cop_df, all_units)

# Density plot df
x = seq(from = 0.01, to = 0.99, length.out = 50)
y = seq(from = 0.01, to = 0.99, length.out = 50)
density_grid = expand.grid(x = x, y = y)
plot_dfs = lapply(
  all_units,
  function(name) get_density_values(vines[[name]], density_grid, name)
)
plot_df = plot_dfs |> dplyr::bind_rows()

# Synthetic data
syn_dfs = lapply(
  all_units,
  function(name) get_synthetic_data(vines[[name]], n_syn, name)
)
syn_df = syn_dfs |> dplyr::bind_rows()

### Station specfic

# Marginal GEV fits
gev_peak = marginal_fit(scop_df$peak, type = "GEV")
plot(gev_peak, type = "density")
gev_vol = marginal_fit(scop_df$volume, type = "GEV")
plot(gev_vol, type = "density")
gev_dur = marginal_fit(scop_df$duration_days, type = "GEV")
plot(gev_dur, type = "density")
summary(scop_df$duration_days)

# countour and syn data dfs
splot_df = plot_df |> dplyr::filter(unit == station)
ssyn_df = syn_df |> 
  dplyr::filter(unit == station) |> 
  dplyr::mutate(
    dur = qmarginal(pobs_dur, gev_dur),
    vol = qmarginal(pobs_vol, gev_vol),
    peak = qmarginal(pobs_peak, gev_peak)
  )
  

# Last recorded flood for the station of choice
last_flood = scop_df |> 
  dplyr::filter(year == max(scop_df$year))


# Visual GoF judging by SynData -------------------------------------------
vine = vines[[station]]
# Contour plots
p1 = get_contour(
  rel = "z12", 
  splot_df = splot_df, 
  sdf = scop_df, 
  varx = "pobs_dur", 
  vary = "pobs_peak", 
  title = "Peak - Duration [C]", 
  y_lab = "P.Obs. Peak",
  # x_lab = "P.Obs. Duration",
  x_lab = "",
  bwidth = 0.1,
  contour_alpha = 0.7
) 
p2 = get_contour(
  rel = "z23", 
  splot_df = splot_df, 
  sdf = scop_df, 
  varx = "pobs_peak", 
  vary = "pobs_vol", 
  title = "Volume - Peak [SG]", 
  y_lab = "P.Obs. Volume",
  # x_lab = "P.Obs. Duration",
  x_lab = "",
  bwidth = 0.1,
  contour_alpha = 0.7
) 
p3 = get_contour(
  rel = "z13_2", 
  splot_df = splot_df, 
  sdf = scop_df, 
  varx = "pobs_dur", 
  vary = "pobs_vol", 
  title = "Volume - Duration | Peak [SG]", 
  y_lab = "P.Obs. Volume",
  # x_lab = "P.Obs. Duration",
  x_lab = "",
  bwidth = 0.2,
  contour_alpha = 0.7
) 
# Synthetic data on [0, 1]
p4 = get_syn_scatter(
  ssyn_df = ssyn_df, 
  varx_syn = "pobs_dur", 
  vary_syn = "pobs_peak", 
  sdf = scop_df, 
  varx_df = "pobs_dur", 
  vary_df = "pobs_peak", 
  x_lab = "P.Obs. Duration",
  y_lab = "P.Obs. Peak",
  syn_alpha = 0.8,
  x_min = 0,
  x_max = 1,
  y_min = 0,
  y_max = 1
)
p5 = get_syn_scatter(
  ssyn_df = ssyn_df, 
  varx_syn = "pobs_peak", 
  vary_syn = "pobs_vol", 
  sdf = scop_df, 
  varx_df = "pobs_peak", 
  vary_df = "pobs_vol", 
  x_lab = "P.Obs. Peak",
  y_lab = "P.Obs. Volume",
  syn_alpha = 0.8,
  x_min = 0,
  x_max = 1,
  y_min = 0,
  y_max = 1
)
p6 = get_syn_scatter(
  ssyn_df = ssyn_df, 
  varx_syn = "pobs_dur", 
  vary_syn = "pobs_vol", 
  sdf = scop_df, 
  varx_df = "pobs_dur", 
  vary_df = "pobs_vol", 
  x_lab = "P.Obs. Duration",
  y_lab = "P.Obs. Volume",
  syn_alpha = 0.8,
  x_min = 0,
  x_max = 1,
  y_min = 0,
  y_max = 1
)
# Synthetic data on marginals
p7 = get_syn_scatter(
  ssyn_df = ssyn_df, 
  varx_syn = "dur", 
  vary_syn = "peak", 
  scop_df |> dplyr::mutate(duration_d = duration_days), 
  varx_df = "duration_d", 
  vary_df = "peak",
  x_lab = "Duration (days)",
  y_lab = latex2exp::TeX("Peak ($m^3/s$)")
)
p8 = get_syn_scatter(
  ssyn_df = ssyn_df, 
  varx_syn = "peak", 
  vary_syn = "vol", 
  scop_df |> dplyr::mutate(vol = volume / 1e6), 
  varx_df = "peak", 
  vary_df = "vol",
  x_lab = latex2exp::TeX("Peak ($m^3/s$)"),
  y_lab = latex2exp::TeX("Volume (Mio. $m^3$)")
)
p9 = get_syn_scatter(
  ssyn_df = ssyn_df, 
  varx_syn = "dur", 
  vary_syn = "vol", 
  scop_df |> dplyr::mutate(vol = volume / 1e6, dur_d = duration_days), 
  varx_df = "dur_d", 
  vary_df = "vol",
  x_lab = latex2exp::TeX("Duration (days)"),
  y_lab = latex2exp::TeX("Volume (Mio. $m^3$)")
)

p = (p1 | p2 | p3) / 
    (p4 | p5 | p6) + 
    # (p7 | p8 | p9) + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 15)  # Increase legend text size
  )
plot(p)
if (save_plots) savegg("visFit", width = 10, height = 5)
# Do not apply any further hypothesis tests:
#   a) Judging by AIC, the fitted copula is the best fitting copula
#   Thus, we already selected the best fit we can possible fit. Independent of what a GoF test would tell us, we will use it in the following
#   b) A better reason tho is that we use the AIC to select a model. 
#   Hypothesis tests do not remain valid after selection after the data is used to select the best model
# --> We default to a visual judgement and proceed

# Tail dependence analysis ------------------------------------------------
# > Taildependencies in results
# > Taildependence in intro: 
#   Difference Taildependence and heavy tail in distribution
# 1: pobs_dur 
# 2: pobs_peak
# 3: pobs_vol
get_copname = function(int){
  cop_fams = list(
    "3" = "Clayton",
    "4" = "Gumbel",
    "5" = "Frank",
    "13" = "rotated Clayton",
    "14" = "rotated Gumbel"
  )
  return(cop_fams[[as.character(int)]])
}

taildep_df = lapply(
  names(vines),
  function(name) grab_taildeps(name, vines, cop_df)
) |> dplyr::bind_rows()  
taildep_df

taildep_pos_sf = gkd2gg(taildep_df, coord_cols = c("east", "north")) |> 
  dplyr::filter(river %in% considered) |> 
  dplyr::mutate(
    river_station = dplyr::case_when(
      river == "Isar" ~ "Isar_Station",
      river == "Donau" ~ "Donau_Station"
    )
  ) |> 
  # Because I only used Gumbel, Frank and Clayton: A station has either upper or lower or neigher taildependence, but never both
  dplyr::mutate(
    # Let lower taildependence have negative sign FOR DISPLAY PURPSE
    # Let upper tp have positive sign FOR DISPLAY PURPOSE
    signed_taildep =  upper - lower  
  ) 


taildep_bayern = ggplot(rivers_bayern) + 
  # Displaying the actual data
  # 1) Make background look like map
  ggspatial::annotation_map_tile(type = "osm", zoom = 8) + 
  # 2) Add the rivers twice. 
  #   1st in black to give a outline for each river
  #   (combination of fill and color does not work for geom_sf)
  #   2nd the actual rivers in desired colors
  # geom_sf(color = "black", linewidth = 2, alpha = 0.5) + # Outlines for the rivers
  # geom_sf(linewidth = 1.5, color = "#00BfC4", alpha = 0.5) +
  geom_sf(data = rivers_bayern |> dplyr::filter(river == "Isar"), linewidth = 2, color = "black") +
  geom_sf(data = rivers_bayern |> dplyr::filter(river == "Isar"), linewidth = 1.5, color = "#F8766D") +
  geom_sf(data = rivers_bayern |> dplyr::filter(river == "Donau"), linewidth = 2, color = "black") +
  geom_sf(data = rivers_bayern |> dplyr::filter(river == "Donau"), linewidth = 1.5, color = "#7CAE00") +
  # 3) Add the shape of Bayern on top to highlight the relevant area
  geom_sf(data = bayern, fill = "white", alpha = 0.6, linewidth = .8) + 
  # 4) Add station
  # geom_sf(data = pos_sf, aes(color = river_station, shape = river_station, fill = river_station), size = 4) + 
  # Formatiing of Data
  theme_void() + 
  theme(
    # Remove the x and y ticks / labels
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # Adjust the legend position and font size
    legend.position = "bottom",  # Move legend to top-right
    legend.margin = margin(1, 1, 1, 1),  # (top, right, bottom, left) padding
    # legend.text = element_text(size = 20),  # Adjust size of legend labels
    legend.title = element_text(size = 15, face = "bold"),
    strip.text.x = element_text(size = 18)  # Adjust size of facet labels
  ) 

taildep_bayern + 
  geom_sf(data = taildep_pos_sf, aes(color = signed_taildep), size = 4) + 
  scale_color_gradient2(
    low = "black",   
    mid = "white",    
    high = "#7B3294",  
    midpoint = 0,
    limits = c(-1, 1),
    name = "Signed Tail.Dep."
  ) + 
  # scale_shape_manual(
  #   name = "Copula",
  #   values = c("Gumbel" = 15, "Clayton" = 16, "Frank" = 13, "rotated Gumbel" = 18, "rotated Clayton" = 17),
  # ) + 
  guides(
    fill = guide_legend(order = 1),
    color = guide_colorbar(barwidth = 10, barheight = 0.8, title.position = "top"),
    shape = guide_legend(nrow = 2, byrow = TRUE)  # Arrange legend neatly in 2 rows
  ) + 
  theme(strip.text = element_text(size = 10)) + 
  facet_wrap(~variables, labeller = 
               as_labeller(c(
                 "D - P" = "Duration - Peak",
                 "D - V" = "Duration- Volume",
                 "P - V" = "Peak - Volume"
               )))
if (save_plots) savegg("Taildep_Bayern", width = 10, height = 5)

  

# Extension of Hydrology lecture to trivariate ----------------------------
ggplot(
    data = auc_plot_df <- data.frame(
      x = seq(from = 0, to = max(scop_df$peak)),
      y = dmarginal(seq(from = 0, to = max(scop_df$peak)), gev_peak)
    ),
    mapping = aes(x = x, y = y)
  ) + 
  geom_histogram(data = scop_df, mapping = aes(x = peak, y = after_stat(density)), fill = "white", alpha = 0.4, color = "black") + 
  geom_line(color = "blue", linewidth = 1) + 
  geom_area(data = auc_plot_df |> dplyr::filter(x >= last_flood$peak), fill = "red", alpha = 0.4) + 
  geom_vline(xintercept = last_flood$peak, color = "red") +
  labs(
    x = latex2exp::TeX("Peak ($m^3/s$)"),
    y = "Density",
    title = "Munich station - GEV Fit on Peak"
  ) + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 20)
  )
if (save_plots) savegg("marginal peak fit", width = 10, height = 5)
# Compared to if only marginal
# Approach from hydrology lecture: extReme package and fit gevd 
# According to univariate approach:
#   Probability of this event or a more severe flood occuring
1 - pmarginal(last_flood$peak, gev_peak)
# -> Big difference in probability --> Underline relevance of trivariate analysis 

cdfs = lapply(
  all_units,
  function(name) get_cdf_values(vines[[name]], density_grid, name)
) |> dplyr::bind_rows() |> dplyr::select(cdf) |> unlist()
plot_df$cdf = cdfs 
splot_df = plot_df |> dplyr::filter(unit == station)

p1 = get_contour(
  rel = "z12", 
  splot_df = splot_df, 
  sdf = scop_df, 
  varx = "pobs_dur", 
  vary = "pobs_peak", 
  title = "Peak - Duration [C]", 
  y_lab = "P.Obs. Peak",
  x_lab = "P.Obs. Duration",
  bwidth = 0.1,
  contour_alpha = 0.7
) +
  geom_point(data = last_flood, aes(x = pobs_dur, y = pobs_peak), color = "red") + 
  geom_segment(data = last_flood, aes(x = 0, xend = 1, y = pobs_peak, yend = pobs_peak), color = "red") +
  geom_segment(data = last_flood, aes(x = pobs_dur, xend = pobs_dur, y = 0, yend = 1), color = "red") +
  geom_rect(data = last_flood, aes(xmin = pobs_dur, xmax = 1, ymin = pobs_peak, ymax = 1), fill = "red", alpha = 0.3)
p2 = get_contour(
  rel = "z23", 
  splot_df = splot_df, 
  sdf = scop_df, 
  varx = "pobs_peak", 
  vary = "pobs_vol", 
  title = "Volume - Peak [SG]", 
  y_lab = "P.Obs. Volume",
  x_lab = "P.Obs. Duration",
  bwidth = 0.1,
  contour_alpha = 0.7
)  + 
   geom_point(data = last_flood, aes(x = pobs_peak, y = pobs_vol), color = "red") + 
  geom_segment(data = last_flood, aes(x = 0, xend = 1, y = pobs_vol, yend = pobs_vol), color = "red") +
  geom_segment(data = last_flood, aes(x = pobs_peak, xend = pobs_peak, y = 0, yend = 1), color = "red") +
  geom_rect(data = last_flood, aes(xmin = pobs_peak, xmax = 1, ymin = pobs_vol, ymax = 1), fill = "red", alpha = 0.3)
p3 = get_contour(
  rel = "z13_2", 
  splot_df = splot_df, 
  sdf = scop_df, 
  varx = "pobs_dur", 
  vary = "pobs_vol", 
  title = "Volume - Duration | Peak [SG]", 
  y_lab = "P.Obs. Volume",
  x_lab = "P.Obs. Duration",
  bwidth = 0.1,
  contour_alpha = 0.7
)  + 
   geom_point(data = last_flood, aes(x = pobs_dur, y = pobs_vol), color = "red") + 
  geom_segment(data = last_flood, aes(x = 0, xend = 1, y = pobs_vol, yend = pobs_vol), color = "red") +
  geom_segment(data = last_flood, aes(x = pobs_dur, xend = pobs_dur, y = 0, yend = 1), color = "red") +
  # geom_hline(yintercept = last_flood$pobs_peak, color = "red") + 
  # geom_vline(xintercept = last_flood$pobs_dur, color = "red") + 
  geom_rect(data = last_flood, aes(xmin = pobs_dur, xmax = 1, ymin = pobs_vol, ymax = 1), fill = "red", alpha = 0.3)
  
p = (p1 | p2 | p3)
plot(p)
if (save_plots) savegg("trivariateProb", width = 10, height = 5)

# P(X>x, Y>y, Z>z)
syn_vals = ssyn_df |> dplyr::select(pobs_dur, pobs_peak, pobs_vol)  
thresh_vals = c(last_flood$pobs_dur, last_flood$pobs_peak, last_flood$pobs_vol)
# Idea: Use threshold from last flood
# For every entry in synthetic data, check if value is larger corresponding threshold
# If all three (rowsum) are larger than their thresholds, they are an occurance of X>x, Y>y, Z>z
# Determine frequence of these events
prob = sum(rowSums(syn_vals >= thresh_vals) == 3) / nrow(ssyn_df)

print(paste("Probability of this event P(P <= p, V <= v, D <= d)", prob))




# Volume - Duration pairs for HQ values -----------------------------------
# Return period value plot 
data.frame(
  Peak  = qmarginal(1 - HQ_probs, gev_peak),
  rperiod = 1 / HQ_probs
) |> 
  ggplot() + 
  geom_line(aes(x = rperiod, y = Peak)) + 
  geom_label(aes(x = rperiod, y = Peak, label = rperiod), size = 5) + 
  labs(title = "Munich station - Return Periods", x = "Return Period (years)") + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 90),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 20)
  )
if (save_plots) savegg("univariateReturnPeriodsPeak", width = 10, height = 5)


# IMPORTANT: This analysis is STATION specific
# Marginals:

# Use HQ return period bzw. resulting probabilities as pobs_peak to condition on

# Initial values:
# München: nac: vol = 400, dur = 20 --- vines: vol = 500, dur = 20
con_nac_smry = lapply(
  HQ_probs,
  function(hq_prob) get_most_probable_voldur(
      hq_prob = hq_prob,
      initial_vol = 500,
      initial_dur = 10,
      gev_vol = gev_vol,
      gev_dur = gev_dur,
      gev_peak = gev_peak,
      mdl = nacs[[station]],
      mdl_type = "nac",
      trace = 0
    )
) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(type = "nac")
con_nac_smry

con_vine_smry = lapply(
  HQ_probs,
  function(hq_prob) get_most_probable_voldur(
      hq_prob = hq_prob,
      initial_vol = 3000,
      initial_dur = 40,
      gev_vol = gev_vol,
      gev_dur = gev_dur,   
      gev_peak = gev_peak,
      mdl = vines[[station]],
      mdl_type = "vine",
      trace = 0
    )
) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(type = "vine")
con_vine_smry

con_smry = rbind(
  con_vine_smry,
  con_nac_smry
) |> dplyr::mutate(HQ = 1 / hq_prob, .after = hq_prob)
con_smry
# 
# # Check somewhat of bivariate quantile
# #   Sort observed data by volume and duration
# #   Select empirical quantiles of this and plot it to get a feeling of which one is a better fit
# checkpoints = scop_df |> 
#   dplyr::arrange(
#     # peak
#     volume
#   ) |> 
#   dplyr::mutate(
#     idx = dplyr::row_number()
#   ) |> 
#   dplyr::filter(
#     idx %in% ceiling((1 - HQ_probs) * nrow(scop_df))
#   ) |> 
#   dplyr::mutate(
#     hq = pmax(round(1 - (idx / nrow(scop_df)), 2), 1 / 500),
#     hq_years = round(1 / hq, 0)
#   ) |>
#   dplyr::select(-c(id, east, north, river))
# checkpoints
# My prognosis is sucks giga hard as soon as the quantile method fails
#     Idea: 1) Flood starts with stark rise in slop in hydrograph
#             -> Calc slope using triangle and then use quantile slop to define when flood starts
#               For now: Check a plot of slope. How does that look?
#               i.e. Find peak --> Hydrograph (as now)
#                   Find start of flood using quantiles of slope around that peak
#                   Use indices to translate slop data at point with discharge values at point; 
#                   -> Grab index of peak, Grab indices of start and end of flood --> Habe the 3 indices needed; Basically straight line method, but on the slopes
#           2) Two flood events are considered the same event if there are only x hours between them 

# 
# scop_df |> 
#   dplyr::arrange(
#     # peak, volume, duration_days
#     volume, duration_days
#   ) |> 
#   dplyr::select(year, duration_days, peak, volume, dplyr::contains("pobs")) |> 
#   dplyr::mutate(
#     idx = dplyr::row_number(),
#     .before = year
#   ) |> 
#   dplyr::arrange(desc(peak)) |> 
#   ggplot() + 
#   geom_point(aes(y = duration_days, x = volume, color = peak)) + 
#   # geom_point(data = checkpoints, aes(y = duration_min, x = volume), color = "red")
#   geom_text(data = checkpoints, mapping = aes(x = volume, y = duration_days, label = ifelse(is.infinite(1 / hq), 500, round(1 / hq))), color = "red") 

# Issue:
# The copula fit is fine, but the marginal fit is an issue
# I underestimate duration for pretty much all stations. Reason ist seen in the syn data points after using marginal to re-transform
#     --> There are data points far below the observed minimum and even around 0
#   Thus: We have to improve marginal fit! 
#   Else, our prediction is useless
#   AND: I cannot really use inverse empirical PIT because then my data is thresholded by the observed data points
#     i.e. I cannot really predict something that is outside of my data because empirical PIT basically bins all the data 
#     and whenever a value is within a bin, it is assigned the observed value
#   Good example: Hofkirchen
# Another Issue:
# Our estimation of most probable pair is numerically instable (yet)
ggplot(con_smry) + 
  geom_point(data = scop_df, aes(y = duration_days, x = volume, color = peak)) + 
  geom_line(aes(x = vol, y = dur)) +
  geom_point(aes(x = vol, y = dur)) +
  geom_label(aes(x = vol, y = dur, label = 1 / hq_prob), size = 5) +
  # geom_point(data = checkpoints, mapping = aes(x = volume , y = duration_days), color = "red") +
  # geom_text(data = checkpoints, mapping = aes(x = volume, y = duration_days, label = hq_years), color = "red", nudge_y = 2, size = 5) +
  facet_wrap(~type) + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 15)
  ) + 
  labs(
    title = paste(station, "station - Most Likely Conditional Vol - Dur Pairs"),
    x = latex2exp::TeX("Volume (Mio. $m^3$)"),
    y = "Duration (days)"
  )
if (save_plots) savegg(paste("500HQ", station), width = 10, height = 5)
# Vines capture underlying data way better than NACs. 
# According to NACs, our data contains multiple flood events that were more severe than a 
# flood with a HQ200 which triplet of (vol, peak, dur) would only appear every 200 years
# BUT also vines have some odd observations where we have seemed to observe a triplet that would onyl appear every 500 years
# The flood event is:
scop_df |> dplyr::filter(volume == max(volume))
# A potential reason for this might be that our model does not capture the effect of climate change
# It assumes a time constant dependence structure / copula and thus may underestimate probabilities of 
# extreme events if they become more probable over time (due to climate change)
# Check flood behavior over time:
scop_df |> 
  ggplot() + 
  geom_line(aes(x = year, y = volume / 1e6)) 
# Nope, there is no real time trend I'd say....






# Beobachtete Daten, um Werte zu checken:
summary(scop_df$volume)
summary(scop_df$duration_days)
summary(scop_df$peak)


cop_df |> 
  dplyr::summarise(
    vol_quant50 = quantile(volume, 0.5) / 1e6,
    .by = c(river, unit)
  ) 


# TODO: Run this for all stations and explain why average and different sample method
#   Then, go into how bad the model performs due to marginal duration fit 
# Bc  approach so far is numerically instable, consider the average / median (/mode only if I have time) for stations for vines
# This approach is actually numerically stable!
# Draw random sample from conditional copula and get inverse to unconditional
cons_vine = lapply(
  HQ_probs,
  function(HQ_prob) rcond_vine_draws(HQ_prob, vines[[station]]) |>
    dplyr::mutate(
      peak = qmarginal(pobs_peak, gev_peak),
      mio_vol= qmarginal(pobs_vol, gev_vol),
      d_dur = qmarginal(pobs_dur, gev_dur)
    )
)  |>
  dplyr::bind_rows()

# Duration has an almost uniform margin which makes sense considering that there is barely any dependence
# Volume however does move to larger values for an increased peak
# IMPORTANT: I cannot take average / median / mode of the UNIVARIATE / marginal distributions
# These do not consider how the other variables moves
# Thus, I need to find a bivariate solution for this
ggplot(cons_vine, aes(x = pobs_vol, y = pobs_dur)) + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~pobs_peak)

con_vine_smry = cons_vine |>
  dplyr::summarise(
    avg_vol = mean(mio_vol),
    med_vol = median(mio_vol),
    mod_vol = calc_bivariate_mode(x = mio_vol, y = d_dur)["x"],
    avg_dur = mean(d_dur),
    med_dur = median(d_dur),
    mod_dur = calc_bivariate_mode(x = mio_vol, y = d_dur)["y"],
    cop = "vine",
    .by = hq_prob
  )
con_vine_smry
ggplot(con_vine_smry) + 
  geom_point(data = scop_df, aes(y = duration_days, x = volume, color = peak)) + 
  geom_line(aes(x = mod_vol, y = mod_dur)) +
  geom_point(aes(x = mod_vol, y = mod_dur)) +
  geom_label(aes(x = mod_vol, y = mod_dur, label = 1 / hq_prob), size = 5) + 
  # geom_text(data = checkpoints, mapping = aes(x = volume, y = duration_days, label = ifelse(is.infinite(1 / hq), 500, round(1 / hq))), color = "red", nudge_y = 2, size = 5) +
  # geom_point(data = checkpoints, mapping = aes(x = volume, y = duration_days), color = "red") + 
  theme(
    title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 15)
  ) + 
  labs(
    title = paste(station, "station - Most Likely Conditional Vol - Dur Pairs"),
    x = latex2exp::TeX("Volume (Mio. $m^3$)"),
    y = "Duration (days)"
  )
if (save_plots) savegg("500HQStableAverage", width = 10, height = 5)
# Appendix ----------------------------------------------------------------
# Munich station scatterplots
# y: Volume, x: Peak
plot_vp = ggplot(cop_df |> dplyr::filter(unit == "München"), aes(y = volume / 1e6, x = peak)) +
  geom_point() + 
  labs(
    title = "Munich: Volume - Peak",
    x = expression(Peak ~ ( m^3 / s)),
    y = latex2exp::TeX("Volume (Mio. $m^3$)")
  )

# y: Volume, x: Duration
plot_vd = ggplot(cop_df |> dplyr::filter(unit == "München"), aes(y = volume / 1e6, x = duration_days)) +
  geom_point() + 
  labs(
    title = "Munich: Volume - Duration",
    x = "Duration (days)",
    y = latex2exp::TeX("Volume (Mio. $m^3$)")
  )

# y: Peak, x: Duration
plot_pd = ggplot(cop_df |> dplyr::filter(unit == "München"), aes(y = peak, x = duration_days)) +
  geom_point() + 
  labs(
    title = "Munich: Peak - Duration",
    x = "Duration (days)",
    y = expression(Peak ~ ( m^3 / s)),
  )

plot_all3 = plot_vp | plot_vd | plot_pd
plot_all3
if(save_plots) savegg("Appendix_munichstation_scatter", width = 10, height = 5)

# Munich station scatterplots
# y: Volume, x: Peak
plot_vp = ggplot(cop_df |> dplyr::filter(unit == "Mittenwald"), aes(y = volume / 1e6, x = peak)) +
  geom_point() + 
  labs(
    title = "Mittenwald: Volume - Peak",
    x = expression(Peak ~ ( m^3 / s)),
    y = latex2exp::TeX("Volume (Mio. $m^3$)")
  )

# y: Volume, x: Duration
plot_vd = ggplot(cop_df |> dplyr::filter(unit == "Mittenwald"), aes(y = volume / 1e6, x = duration_days)) +
  geom_point() + 
  labs(
    title = "Mittenwald: Volume - Duration",
    x = "Duration (days)",
    y = latex2exp::TeX("Volume (Mio. $m^3$)")
  )

# y: Peak, x: Duration
plot_pd = ggplot(cop_df |> dplyr::filter(unit == "Mittenwald"), aes(y = peak, x = duration_days)) +
  geom_point() + 
  labs(
    title = "Mittenwald: Peak - Duration",
    x = "Duration (days)",
    y = expression(Peak ~ ( m^3 / s)),
  )

plot_all3 = plot_vp | plot_vd | plot_pd
plot_all3
if(save_plots) savegg("Appendix_mittenwaldstation_scatter", width = 10, height = 5)

# Marginal fits using extReme-package
plot(gev_peak, type = "density")
plot(gev_vol, type = "density")
plot(gev_dur, type = "density")


# TODO
# > Appendix: Hydrographen für die Tabellen-Events
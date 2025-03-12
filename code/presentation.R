# Code to generate plots for presentation 



# Packages and Dependencies -----------------------------------------------
source("functions.R")
source("load_data.R")
library(ggplot2)

load("../data/output/presentation/pos.Rdata")
load("../data/output/presentation/rivers.Rdata")

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

cop_df = get_copula_df()
df = get_long_df()



# color y x
color_vol_peak = "lightblue"
color_vol_dur = "orange"
color_dur_peak = "lightgreen"


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
considered = c("Isar", "Donau")
cop_df = cop_df |> dplyr::filter(river %in% considered)
pos_sf = gkd2gg(pos, coord_cols = c("east", "north")) |> 
  dplyr::filter(river %in% considered) |> 
  dplyr::mutate(
    river_station = dplyr::case_when(
      river == "Isar" ~ "Isar_Station",
      river == "Donau" ~ "Donau_Station"
    )
  )
# save(pos, file = "../data/output/presentation/pos.Rdata")

# TODO: Also define a function to get the river stuff and bavaria shape during load_data
# Here, I only want to read in some files and be done

# Parameter ---------------------------------------------------------------


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

# Data --------------------------------------------------------------------

# NOTE: Plots may look super bad in RStudio. I save them using savegg. 
#   Check the saved file or its look in PowerPoint to judge visual
# Selected rivers and all stations 
ggplot(rivers_bayern) + 
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
savegg("bayern_rivers")

# Select one station to explain the data process on
station = "München"
    
ggplot(rivers_bayern) + 
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
  # Add highlight for selected station
  geom_sf(data = pos_sf |> dplyr::filter(unit == station), color = "black", size = 10, shape = 1, alpha = 1) + 
  geom_sf_label(
    data = pos_sf |> dplyr::filter(unit == station), 
    aes(label = unit),
    nudge_x = .8,
    size = 10,
    fill = "white"
  ) + 
  # Prettify
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
savegg("bayern_river_station_highlighted")

# Data process explained
ref_unit = station
ref_year = 2024
peak_info = cop_df |> dplyr::filter(year == ref_year, unit == ref_unit)

id = (cop_df |> dplyr::filter(unit == ref_unit))[1, "id"]
load(paste("../data/output/rdata/threshold_dfs/", id, "_t0.75.Rdata", sep = "")) # loads "df"
ref_yeardf = df |> dplyr::filter(year == ref_year)

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
savegg("station_full_hydrograph", width = 10, height = 5)
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
savegg("hydrograph_munich_2024", width = 10, height = 5)
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
  geom_hline(yintercept = ref_yeardf$threshold[[1]]) + 
  scale_x_continuous(breaks = c(0, 100, 200, 300, 365)) +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +
  labs(
    title = "Straight Line Method", 
    x = "Day of the Year",
    y = expression(Discharge ~ ( m^3 / s))
  )
savegg("straight_line_method", width = 10, height = 5)

cop_df |> dplyr::filter(unit == station, year == ref_year) |> 
  dplyr::select(
    duration_min, peak, volume
  ) |> 
  dplyr::mutate(
    duration_h = duration_min / 60,
    duration_d = duration_h / 24,
    .after = duration_min
  ) |> 
  dplyr::mutate(
    volume_inmio = volume / 1e6
  )

# Mention that not all hydrographs were as nice and our filter criteria
# Potentially also, how we derived them. Maybe put the corresponding plots from README into the appendix and tell the others to ask 
# -> i.e. questions for asking session




scop_df = cop_df |> dplyr::filter(unit == station)

marginal_peak = ggplot(scop_df, aes(x = peak)) +
  geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.3, color = "black") + 
  labs(
    title = latex2exp::TeX("Munich Station: Peak Discharge"),
    x = latex2exp::TeX("Peak ($m^3/s$)"),
    y = "Number of Observations"
  ) + 
  theme_minimal()

marginal_dur = ggplot(scop_df, aes(x = duration_min / 60 / 24)) +
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
      duration_day = duration_min / 60 / 24,
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
savegg("munich_marginals", width = 10, height = 5)

# That for every station
# -> Descriptives over all stations
# Rivers
unique(cop_df$river)
length(unique(cop_df$river))

# Total number Stations
length(unique(cop_df$unit))
# Of which have more than 30 observations
thresh = 30
considered_stations = cop_df |> 
  dplyr::summarise(
    n = dplyr::n(),
    .by = c(river, unit)
  ) |> 
  dplyr::mutate(
    nlarge = n > thresh
  ) |> 
  dplyr::filter(nlarge == TRUE)


# cop_df only contains stations with more than threshold number of observations
cop_df = cop_df |> dplyr::filter(unit %in% considered_stations$unit) 

# Number of stations by river
table(considered_stations$river)
# Majority of stations have 54 observations, only 2 have below 50: 44 and 48 observations
table(considered_stations$n)


print(paste("Total number of flood events:", cop_df |> nrow()))

l_in_bathtub = 302
l_therme_erdingen = 3.3e6
cop_df |> 
  dplyr::summarise(
    max_peak = max(peak),
    max_vol = max(volume),
    max_dur = max(duration_min) / 60 / 24,
    .by = river
  ) |> dplyr::mutate(
    max_peak_tub = max_peak / l_in_bathtub,
    max_vol_in_therme_erdingen = max_vol / l_therme_erdingen,
    max_vol_in_mio_l = max_vol / 1e6
  )
# Duration of the maximum volume floodings
cop_df |> dplyr::filter(volume %in% c(6824363400, 1808050500)) |> dplyr::mutate(duration_days = duration_min / 60 / 24)


# -> Taus Boxplots (correlation tiles already hinted towards this behavior)
# Correlation table
# Only Mittenwald has negative correlation. But judging by the scatter plot, it is pretty unclear
#   This "unclear" is supported by insignificant p-value. Also, consider pseudo-obs. They look like no real dependence at all
cor_table = cop_df |> dplyr::summarise(
  n = dplyr::n(),
  tau_vd = cor(volume, duration_min, method = "kendall"),
  p_vd = cor.test(volume, duration_min, method = "kendall")$p.value,
  rej_vd = p_vd < 0.01,
  tau_vp = cor(volume, peak, method = "kendall"),
  p_vp = cor.test(volume, peak, method = "kendall")$p.value,
  rej_vp = p_vp < 0.01,
  tau_dp = cor(duration_min, peak, method = "kendall"),
  p_dp = cor.test(duration_min, peak, method = "kendall")$p.value,
  rej_dp = p_dp < 0.01,
  .by = c(river, unit)
)
# Boxplot
cor_table |> 
  tidyr::pivot_longer(
    cols = c(tau_vd, tau_vp, tau_dp),
    names_to = "tau",
    values_to = "val"
  ) |> 
  dplyr::mutate(
    tau = dplyr::case_when(
      tau == "tau_dp" ~ "Duration-Peak",
      tau == "tau_vd" ~ "Volume - Duration",
      tau == "tau_vp" ~ "Volume - Peak"
    ),
    tau = as.factor(tau)
  ) |> 
  ggplot() + 
  geom_boxplot(aes(y = val, x = tau, color = tau)) + 
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
savegg("tau_boxplots", width = 15, height = 5)



# Methods -----------------------------------------------------------------
# Generator function plots
tau = list(low = 0.1, high = 0.7)
source("functions.R")
showcase_copula_contours(tau$low, title = latex2exp::TeX(paste("Copula Families ($\\tau =", tau$low, "$)")), textsize_lab = 20, textsize_tick = 10, textsize_strip = 10)
savegg("CopFams_lowtau", width = 10, height = 5)
showcase_copula_contours(tau$high, title = latex2exp::TeX(paste("Copula Families ($\\tau =", tau$high, "$)")), textsize_lab = 20, textsize_tick = 10, textsize_strip = 10)
savegg("CopFams_hightau", width = 10, height = 5)




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
plot_vd = ggplot(cop_df |> dplyr::filter(unit == "München"), aes(y = volume / 1e6, x = duration_min / 60 / 24)) +
  geom_point() + 
  labs(
    title = "Munich: Volume - Duration",
    x = "Duration (days)",
    y = latex2exp::TeX("Volume (Mio. $m^3$)")
  )

# y: Peak, x: Duration
plot_pd = ggplot(cop_df |> dplyr::filter(unit == "München"), aes(y = peak, x = duration_min / 60 / 24)) +
  geom_point() + 
  labs(
    title = "Munich: Peak - Duration",
    x = "Duration (days)",
    y = expression(Peak ~ ( m^3 / s)),
  )

plot_all3 = plot_vp | plot_vd | plot_pd
plot_all3
savegg("Appendix_munichstation_scatter", width = 10, height = 5)

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
plot_vd = ggplot(cop_df |> dplyr::filter(unit == "Mittenwald"), aes(y = volume / 1e6, x = duration_min / 60 / 24)) +
  geom_point() + 
  labs(
    title = "Mittenwald: Volume - Duration",
    x = "Duration (days)",
    y = latex2exp::TeX("Volume (Mio. $m^3$)")
  )

# y: Peak, x: Duration
plot_pd = ggplot(cop_df |> dplyr::filter(unit == "Mittenwald"), aes(y = peak, x = duration_min / 60 / 24)) +
  geom_point() + 
  labs(
    title = "Mittenwald: Peak - Duration",
    x = "Duration (days)",
    y = expression(Peak ~ ( m^3 / s)),
  )

plot_all3 = plot_vp | plot_vd | plot_pd
plot_all3
savegg("Appendix_mittenwaldstation_scatter", width = 10, height = 5)

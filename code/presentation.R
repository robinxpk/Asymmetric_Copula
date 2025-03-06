# Code to generate plots for presentation 



# Packages and Dependencies -----------------------------------------------
source("functions.R")
source("load_data.R")
library(ggplot2)

load("../data/output/presentation/pos.Rdata")
load("../data/output/presentation/all_rivers.Rdata")
load("../data/output/presentation/considered_rivers.Rdata")

cop_df = get_copula_df()

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
pos_sf = gkd2gg(pos, coord_cols = c("east", "north")) |> dplyr::filter(river %in% names(considered))
# save(pos, file = "../data/output/presentation/pos.Rdata")

# TODO: Also define a function to get the river stuff and bavaria shape during load_data
# Here, I only want to read in some files and be done

# Parameter ---------------------------------------------------------------


# Get bavaria and its rivers 
germany = rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
bayern = germany |> dplyr::filter(name == "Bayern")
bayern_bbx = sf::st_bbox(bayern) # Create bounding box for openstreetmap

# See 2.1 at https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
rivers = osmdata::opq(bayern_bbx, timeout = 180) |> 
  osmdata::add_osm_feature(key = "waterway", value = "river") |>      
  # osmdata::add_osm_feature(key = "width", value = "100", value_exact = FALSE) |>
  # osmdata::add_osm_feature(
  #   key = "name",
  #   value = c("Danube", "Main", "Isar", "Inn", "Schwarzach", "Vils"),
  #   match_case = FALSE,
  #   key_exact = FALSE,
  #   value_exact = FALSE
  # ) |>
  osmdata::osmdata_sf()
# save(rivers, file = "../data/output/presentation/rivers.Rdata")

# Create sf object based on the queried osm object
rivers_sf = rivers$osm_lines

# Cut rivers so they fit into Bayern
rivers_st = sf::st_transform(rivers_sf, sf::st_crs(bayern))
rivers_bayern = sf::st_intersection(rivers_st, bayern)
# save(rivers_bayern, file = "../data/output/presentation/rivers_bayern.Rdata")

# Add identificator for selection of relevant rivers
rivers_bayern = rivers_bayern |> 
  dplyr::mutate(
    relevant = dplyr::if_else(
      name %in% names(considered),
      TRUE,
      FALSE
    ))

# All rivers
rivers_bayern |> 
  ggplot() + 
    geom_sf(data = bayern, fill = "grey", color = "black") + 
    geom_sf(color = "blue", size = 0.5) +
    theme_minimal() 

# Selected rivers and all stations 
rivers_bayern |> 
  dplyr::mutate(river_color = dplyr::if_else(relevant, "red", "blue")) |>
  ggplot() + 
    geom_sf(data = bayern, fill = "grey", color = "black") + 
    geom_sf(aes(color = river_color), size = 0.5) +
    geom_sf(data = pos_sf) + 
    theme_minimal() +
    scale_color_identity()

# Select one station to explain the data process on
# TODO

# Data process explained
ref_unit = "München"
ref_year = 2024
ref_yeardf = df |> dplyr::filter(year == ref_year)
peak_info = cop_df |> dplyr::filter(year == ref_year, unit == ref_unit)

id = (cop_df |> dplyr::filter(unit == ref_unit))[1, "id"]
load(paste("../data/output/rdata/threshold_dfs/", id, "_t0.75.Rdata", sep = "")) # loads "df"
# Hydrograph for whole time span
ggplot(df, aes(x = date, y = discharge)) +
  geom_line()
# We split hydrograph into years
ggplot(df, aes(x = date, y = discharge)) +
  geom_line() +
  geom_vline(xintercept = as.POSIXct(paste(1972:2024, "-01-01 00:00:00", sep = "")), colour = "red", linetype = 2)
# # Hydrograph per year - stacked
# ggplot(df, aes(x = doy, y = discharge, group = year)) +
#   geom_line()
# Use munich 2024 and remind of the crazy isar time

# Hydropgraph for reference year
ggplot(df |> dplyr::filter(year == ref_year), aes(x = doy, y = discharge)) +
  geom_line()
# We consider the flood event with the highest peak only (following paper here)
# Flood start and end are obtained by straight line method
# Identifying using quantile of yearly distribution
ggplot(mapping = aes(x = doy, y = discharge)) +
  geom_line(data = ref_yeardf, color = "darkgreen") +
  geom_line(data = ref_yeardf |> dplyr::filter(peak_flood == TRUE), color = "red") +
  geom_hline(yintercept = ref_yeardf$threshold[[1]])
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
  geom_hline(yintercept = ref_yeardf$threshold[[1]])
# Mention that not all hydrographs were as nice and our filter criteria
# Potentially also, how we derived them. Maybe put the corresponding plots from README into the appendix and tell the others to ask 
# -> i.e. questions for asking session

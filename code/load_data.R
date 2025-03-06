library(ggplot2)
# GKD Data:
# See: https://www.gkd.bayern.de/en/rivers/discharge/tables
# bzw.: https://www.gkd.bayern.de/en/downloadcenter/wizard?
# NOTE: Because simpler, I will only use data until 31.12.24 (Bc that data is all within one csv)

load_data <- function(rel_path, 
                      header_main = T, 
                      sep = ";", 
                      skip = 9, 
                      col_names = c("date", "discharge", "status"), 
                      rows_meta = 10, 
                      header_meta = FALSE,
                      tz_list = list(MEZ = "MET"),
                      date_format = "%Y-%m-%d %H:%M",
                      nonNA_cols = c("date", "status"),
                      logfile = "../data/output/rdata/extended_dfs/issues.log"
                      ){
  "
  Use a relative path to read in a csv file from GKD-website.
  Path is relative to Code folder.
  
  Discharge data starts at row 10 in all CSVs
  Before, there is only meta data info
  Goal: Extract actual discharge data and add meta data to data frame
  
  Returns a data frame with 3 columns (date, discharge, status) and meta data attributes.
  "
  
  # Maindata:
  dat <- read.csv(
    file = rel_path,
    skip = skip,
    sep = sep,
    header = header_main 
  )
  names(dat) = col_names
  
  # Metadata
  # NOTE: When reading in the data, it is forced into 2 columns. Think this is bc csv file starts with two columns only
  # Leads to varying shape in rows, e.g.:
  # V1                                                  V2
  # 1                Quelle: Bayerisches Landesamt für Umwelt, www.gkd.bayern.de
  # 2      Datenbankabfrage:                                    18.02.2025 14:33
  # 3             Zeitbezug:                                                 MEZ
  # 4      Messstellen-Name:                                          Mittenwald
  # 5       Messstellen-Nr.:                                            16000708
  # 6              Gewässer:                                                Isar
  # 7               Ostwert:                                              671208
  # 8              Nordwert:                                             5256930
  # 9  ETRS89 / UTM Zone 32N                                                    
  # 10  Pegelnullpunktshöhe:                                902,92 m NN (DHHN12) 
  
  # Attributes:
  # row 1: Source
  attr1 = "source"
  # row 2: Data base
  attr2 = "data base"
  # row 3: Reference time
  attr3 = "time zone"
  # row 4: Measurement unit
  attr4 = "measurement unit"
  # row 5: Measurement unit ID
  attr5 = "measurement id"
  # row 6: Body of water
  attr6 = "body of water"
  # row 7: East coordinate
  attr7 = "east"
  # row 8: North coordinate
  attr8 = "north"
  # row 9: kinda useless --> skip
  attr9 = "skip"
  # row 10: Point zero ("Pegelnullpunkt")
  attr10 = "point zero"
  attrs = c(attr1, attr2, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10)
  
  # Read in the first rows containing the meta data
  meta <- read.csv(
    file = rel_path,
    nrows = rows_meta,
    sep = sep,
    header = header_meta
  )
  # Assign meta-data attributes to the data frame
  for (i in 1:length(attrs)){
    # trimws: Function to remove the white space on both sides
    attr(dat, attrs[i]) <- trimws(meta[i, 2], which = "both")
  }
  
  # Data 
  # Date: Super weird! There is some issue with winter / summer time. Using lubridate fixed it. 
  # Using Posix caused NAs even BEFORE time zone is applied...
  dat$date = lubridate::parse_date_time(dat$date, orders = date_format)
  # Discharge 
  # NAs possible: Some are just empty --> NAs
  # If that is the case, let user know:
  if (anyNA(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE)))){
    totalNAs = sum(is.na(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE))))
    emptyDis = sum(dat$discharge[is.na(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE)))] == "", na.rm = TRUE)
    NADis = sum(is.na(dat$discharge[is.na(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE)))]), na.rm = TRUE)
    otherDis = totalNAs - emptyDis - NADis
    msg = paste(
        "Note: 
        Transformation for column 'discharge' in data set '", 
        attr(dat, "measurement unit"), 
        "-", 
        attr(dat, "measurement id"), 
        "'[station - id] resulted in NAs:
        ", totalNAs, " NAs of which:
        - ", emptyDis, " due to empty discharge entries
        - ", NADis, " due to NA discharge entries
        - ", otherDis, " non-'' or non-NA values turned to NA. Check those!",
        sep = "")
    write_log(logfile, msg)
    message(msg)
  }
  # Transform into numerical values
  dat$discharge = as.numeric(sub(",", ".", dat$discharge, fixed = TRUE))
  
  # Check if any of the columns contains NAs. Should not be the case
  for (name in nonNA_cols){
    if (anyNA(dat[name])) {
      msg = paste(
        "Error: Column '", 
        name, 
        "' in data set for '", 
        attr(dat, "measurement unit"), 
        "-", 
        attr(dat, "measurement id"), 
        "'[station - id] contains NA values!",
        sep = "")
      write_log(logfile, msg)
      stop(msg)
    }
  }
  
  
  return(dat)
}

extend_columns = function(dat){
  "
  Basically, take the slim data frame from load_data and turn it into a large data frame.
  It is a lot easier to work with that format. 
  "
  dat |> 
    dplyr::mutate(
      day = lubridate::day(date),
      month = lubridate::month(date),
      doy = lubridate::yday(date),# day of year, used to have some x-axis between years
      year = lubridate::year(date),
      hour = lubridate::hour(date),
      min = lubridate::minute(date),
      unit = attr(dat, "measurement unit"),
      id = attr(dat, "measurement id"),
      river = attr(dat, "body of water"),
      pos_east = attr(dat, "east"),
      pos_north = attr(dat, "north")
    )
}

# NOTE: ThoughtI would need straight line method with identification of flood event using slope
# Think I do not need it... But wont delete in case I change my mind?

# apply_slm_slope <- function(df, plot = T) {
#   # Issue with quantile flood indicator: sometimes quite off.  (See readme)
#   # Idea: Maybe use the slope as indicator? 
#   # Like "at steepest point, flood event beginns" 
#   #     -> Value is threshold 
#   #     -> flood ends when threshold reached again. 
#   # Potential issue I see here is when the steepest slope is at a very low threshold. 
#   # Maybe I can use: Steepest slope as indicator when flood begins, 
#   #   but then I use mean / quantile as threshold for duration of flood event?  
#   df = df |> dplyr::mutate(slope = discharge - dplyr::lag(discharge, default = 0))
#   
#   if (plot) plot(ggplot(df, aes(y = discharge, x = doy)) + geom_line() + labs(title = "Raw data"))
#   if (plot) plot(ggplot(df, aes(y = slope, x = doy)) + geom_line() + labs(title = "Slope"))
#   
# }

apply_slm_quants <- function(dat, p_threshold, plot = T){
  "
  Logic behind the straight line method.
  Idea: 
  Within one year for a station:
  1) Identify the peak discharge -> peak 
  2) Identify flood event 'around' it:
    Use a specified threshold. Because rivers differ in their discharge, we use the discharge quantile as threshold.
    Thus, the function takes p_threshold which denotes the p-th quantile of the discharge distribution in a year.
    
    Note: We do not mind any differences in scale because we are interested in the dependence strucutre which is scale invariant. 
    That means, absolute threshold values that define the flood might differ between rivers (small vs. large river), however, the 
    copula / dependence strucutre between the variables is compareable between different discharge thresholds.
  "
  # Straight line method explained:
  # https://serc.carleton.edu/hydromodules/steps/baseflow_separa.html
  # In our case, I just assume the same threshold for start and end of flood, 
  #   thus, the we have the constant threshold identifying start and stop of flood
  
  # Convert p_threshold into quantile value
  threshold = quantile(dat$discharge, probs = p_threshold, na.rm = TRUE)
  
  # Plot of discharge in year
  if (plot) plot(ggplot(dat, aes(x = doy, y = discharge)) + geom_line())
  # Plot of discharge in year with horizontal threshold value (corresponding to quantile)
  if (plot) plot(ggplot(dat, aes(x = doy, y = discharge)) + geom_line() + geom_hline(yintercept = threshold, colour = "red"))
  
  # The f is rleid doing exactly? Bit confused...
  # -> rleid somehow assigns indices according to the given condition
  # What it does here is basically identifying when the threshold is crossed
  floods = dat |> dplyr::mutate(flood_id = as.factor(data.table::rleid(discharge >= threshold)))
  
  # Determine the highest discharge --> most extreme flood in the current year
  #   If multiple with highest value, take 1st
  peak = floods |> dplyr::filter(discharge == max(dat$discharge, na.rm = TRUE)) |> head(1)
  
  # Start and end of flood is determined by using a threshold:
  # When rising above -> flood starts
  # When falling below -> flood ends
  floods = floods |> dplyr::mutate(peak_flood = as.factor(flood_id == peak$flood_id))
  if (plot) plot(ggplot(floods, aes(x = doy, y = discharge, colour = peak_flood)) + geom_line() + geom_hline(yintercept = threshold))
  
  return(floods |> dplyr::select(-flood_id) |> dplyr::mutate(threshold = threshold, .after = p_threshold))
}

file_to_df = function(rel_path,
                      header_main = T, 
                      sep = ";", 
                      skip = 9, 
                      col_names = c("date", "discharge", "status"), 
                      rows_meta = 10, 
                      header_meta = FALSE,
                      tz_list = list(MEZ = "MET"),
                      date_format = "%Y-%m-%d %H:%M",
                      nonNA_cols = c("date", "status"),
                      excluded_years = c(2025),
                      drop_first_year = TRUE,
                      completenessThreshold = 0.85,
                      logfile = "../data/output/rdata/extended_dfs/issues.log"
  ){
  "
    This function takes in a path to a CSV file and return a data frame (Initally part of 'create_and_save_dfs').
    The returned df is the extended one, i.e. the one with all the rows, no attributes.
    
    Some additional filter steps are possible, too.
  "
  # Read in CSV and truncate it according to load_data (e.g. first 10 rows are kinda empty)
  df = load_data(rel_path, header_main, sep, skip, col_names, rows_meta, header_meta, tz_list, date_format, nonNA_cols)
  # Enlargen the slim data frame to simplify working with it
  df = extend_columns(df)
  
  # Due to the data strucutre, there are some different filters by year allowed:
  # Filter first observed year. Reason is that the data here is usually pretty odd...
  first_year = min(df$year)
  if (drop_first_year) df = df |> dplyr::filter(year != first_year)
  # Allow manuel exclusion of selected years. Like 2025, somehow it is still included in the data even 
  # tho the data is supposed to only include 31.12.24
  df = df |> dplyr::filter(!year %in% excluded_years)
  # Apply a filter according to some completeness threshold. That is, any year that has a smaller 
  # ratio of complete cases than the threshold is dropped.
  # Reason: No need to consider the most extreme flood event if there are only 100 observations
  # Where 1 observation is a 15min timeframe of a whole year. lol. 
  years_below_thresh = calcCompRatio(df, filterby = c("year")) |> 
    dplyr::filter(ratio < completenessThreshold) |> 
    dplyr::select(year)
  df = df |> dplyr::filter(!year %in% years_below_thresh$year)
  if (length(years_below_thresh$year > 0)) {
    msg = paste(attr(df, "measurement id"), "Due to threshold on complete cases per year, removed", length(years_below_thresh$year), "years.")
    message(msg)
    write_log(logfile, msg) 
  }
  
  return(df)
}

create_and_save_dfs <- function(
    in_dir = "../data/isar data/bis311224/",
    out_dir = "../data/output/rdata/extended/",
    header_main = T, 
    sep = ";", 
    skip = 9, 
    col_names = c("date", "discharge", "status"), 
    rows_meta = 10, 
    header_meta = FALSE,
    tz_list = list(MEZ = "MET"),
    date_format = "%Y-%m-%d %H:%M",
    nonNA_cols = c("date", "status")
  ){
  "
  Create and save all (extended) data frames. (i.e. the full data frames).
  
  This function takes all input-CSVs and creates the data frames containing the columns:
  date, discharge, status, day, month, doy, year, hour, min, unit, id, river, pos_east, pos_north
  -> date: date and time in Posixct format based on lubridate package
  -> discharge: discharge values (numeric)
  -> status: status of the discharge value; provided by the GKD and shows if value has been checked for validity or not 
  -> day: day of the date
  -> month: month of the date
  -> year: year of the date
  -> hour: hour of the date
  -> min: min of the date
  -> unit: name of the measurement station
  -> id: id of the measurement station
  -> river: name of the river the station is located at
  -> pos_east, pos_north: coordinates of station
  "
  # Get files in the input directory
  filenames = list.files(in_dir)
  
  # Iterate through all file names
  for (filename in filenames){
    
    # Create df from CSV
    df = file_to_df(
      paste(in_dir, filename, sep = ""),
      header_main, sep, skip, col_names, rows_meta, header_meta,
      tz_list, date_format, nonNA_cols
    )
    
    # Filename to save df in
    df_filename = paste(unique(df$id), ".Rdata", sep = "")
    # Save df in output path
    save(df, file = paste(out_dir, df_filename, sep = ""))
  }
}

write_log = function(logfilepath, logmessage){
  cat(logmessage, file = logfilepath, append = TRUE, sep = "\n")
}

apply_and_save_slm = function(
    in_dir = "../data/output/rdata/extended_dfs/",
    out_dir = "../data/output/rdata/threshold_dfs/",
    p_threshold = c(.5, .75, .95) # TODO: Make it deal with multiple thresholds; just create different dfs and safe as DF_tXX.Rdata
  ){
  "
  This function applies the logic in the straight line method to multiple data frames to identify the year's most extreme flood event.
  
  Input to this functions are the data frame as Rdata file from the previous step (i.e.from 'create_and_save_dfs')!
  date, discharge, status, day, month, doy, year, hour, min, unit, id, river, pos_east, pos_north, p_threshold, threshold, peak_flood
  -> date: date and time in Posixct format based on lubridate package
  -> discharge: discharge values (numeric)
  -> status: status of the discharge value; provided by the GKD and shows if value has been checked for validity or not 
  -> day: day of the date
  -> month: month of the date
  -> year: year of the date
  -> hour: hour of the date
  -> min: min of the date
  -> unit: name of the measurement station
  -> id: id of the measurement station
  -> river: name of the river the station is located at
  -> pos_east, pos_north: coordinates of station
  -> p_threshold: p-th quantile used as threshold
  -> threshold: The exact threshold value
  -> peak_flood: Boolean: Is current observation part of the peak flood?
  
  Note: Separating these steps and even saving the Rdata helps working on intermediate steps. Probably not most efficient.
  "
  # Get the names of the files in the input dir
  filenames = list.files(in_dir, pattern = "*data")
  
  # Iterate through all filenames
  for (filename in filenames){
    # Load the df in the Rdata files. These are the data frame produced by create_and_save_dfs
    load(paste(in_dir, filename, sep = ""))
    
    # Add p to identify quantile value
    df = df |> dplyr::mutate(p_threshold = p_threshold)
    # A df contains all observations 
    # Here, we split the observations by year and determine the most extreme flood event
    df = df |>
      dplyr::group_by(year) |>
      # This part takes each sub-data.frame (grouped by year) and applies the straight line method (via function apply_slm_quants)
      # Theapply_slm_quants return a df containing the original columns and the column peak_flood indicating if flood was most extreme
      dplyr::group_modify(~ apply_slm_quants(.x, p_threshold = p_threshold, plot = F)) |>
      # Join sub-data.frames into one big one containing all years
      dplyr::ungroup()
    
    # Save the data frames with the addition _t followed by the p used to determine the quantile
    #   _t: means "threshold" and the following p denotes the used p-th quantile used to identify the most extreme flood
    df_filename = paste(unique(df$id), "_t", p_threshold, ".Rdata", sep = "")
    save(df, file = paste(out_dir, df_filename, sep = ""))
  }
}

create_hydrograph = function(df){
  "
  This function simply plots the hydrograph for one given year and a given station.
  "
  # For the x-axis to be compareable, fix it to the same length
  days_in_year = c(0, 366)
  
  plt = ggplot(df, aes(doy, y = discharge, colour = peak_flood)) + 
    # Hydrograph-line
    geom_line() + 
    # Threshold line
    geom_hline(yintercept = unique(df$threshold)) + 
    # Fix x-axis
    xlim(days_in_year) +
    # In the title, note:
    # station: Name of the station
    # year: Year the data refers to
    # p: p-th quantile used as threshold quantile 
    labs(
      title = paste("Hydrograph - station:", unique(df$unit), "- year:", unique(df$year), "- p:", unique(df$p_threshold))
    ) 
  
  return(plt)
}

create_and_save_hydrographs <- function(
    in_dir = "../data/output/rdata/threshold_dfs/",
    out_dir = "../data/output/graphs/hydrographs/"
  ){
  "
  Create the hydrograph for all the data frames contained in the input folder.
  Uses the data frames created by apply_and_save_slm. That is, these plots require the knowledge of the most extreme flood event.
  
  Each hydrograph is for one station and one year. 
  Helps to assess if threshold values are working and if data looks realistic, i.e. sanity check.
  "
  # Filenames of Rdata files 
  filenames = list.files(in_dir)
  
  # Iterate through input files, i.e. data frames
  for (filename in filenames){
    # load df
    load(paste(in_dir, filename, sep = "")) # Loads data frame called df
    
    # Create plot for each year
    # This is done by splitting the df of a station by year and then applying the create_hydrograph to each of these sub-data.frames
    plots = df |> dplyr::group_split(year) |> purrr::map(create_hydrograph) 
    
    # Assign graph names to each output graph
    graphnames = paste0(out_dir, unique(df$id), "_", 1:length(plots), ".png", sep = "")
    # We have a list of graphs and a list of names. Use both and the ggsave function to save the graphs in the output dir
    purrr::walk2(graphnames, plots, ggsave)
  }
}

calcCompRatio = function(df, filterby = c("id", "year")){
  # I an observation every 15min. Determine ratio by using ratio of the observed 15min intervals and the 15min contained in a year
  # Hinweis: Falls ratio = 1.00274, we have a leap year where 366 days in a year
  timeInYear = 365 * 24 * 60 / 15
  return(df |> dplyr::summarise(ratio = sum(!is.na(discharge)) / timeInYear, .by = dplyr::all_of(filterby)))
}

evaluateCompleteness = function(in_dir = "../data/output/rdata/extended_dfs/", returndf = FALSE, threshold = 0.85){
  "
  For some years in some stations, there are barely any observations or even none at all
  e.g. 10032009, years: 1977, 1978, ..., 1986, 1988, ..., 1997
  Note: Check via df |> dplyr::summarise(allna = all(is.na(discharge)), .by = year)
  Solution: Remove years where a certain threshold of number observations is totally not achieved
  Thus, evaluate completeness of data before hard coding some bs.
  "
  # Load all Rdata files in the in_dir
  df = get_long_df(in_dir)
  
  # Get ratio of observed time relative to total time in year
  completeness = calcCompRatio(df)
  # Check distribution; this is not feasible for the whole flipping df
  p = ggplot(completeness, aes(x = as.factor(year), y = ratio)) + 
    geom_boxplot() + 
    geom_hline(yintercept = threshold, colour = "blue") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    labs(title = "Boxplots fraction of complete cases") + 
    ylim(0, max(completeness$ratio))
  plot(p)
  if (returndf) return(completeness)
}

apply_summary_stats = function(
    in_dir = "../data/output/rdata/extended_dfs/",
    quantile_p = 0.95
  ){
  "
  More of a helper function. 
  I was interested in the distribution of discharge values 
  -> Turns out, discharge highly depends on river bzw. on location at which the river is measured (obviously)
    Thus, discharge threshold must vary for each station to capture different possible discharge levels
  I was interested in the distribution of observations for each year
  -> Turns out, number of observations is sometimes odd. Fixed it by removing the first year and 2025 where only 1 observation was included.
  "
  # Create one data.frame to save all the following in
  sum_df_all = data.frame(
    year = c(),
    mean = c(),
    quant = c(),
    peak = c()
  )
  
  # Read in the data.frames (the ones without the peak_flood info, but should not matter, really)
  filenames = list.files(in_dir, patter = "*data")
  
  # Iterate through all these filenames
  for (filename in filenames){
    # Load df
    load(paste(in_dir, filename, sep = "")) # Loads data frame called df
    
    # Summarise df 
    sum_df = df |> 
      dplyr::summarise(
        mean = mean(discharge, na.rm = T),
        quant = quantile(discharge, probs = quantile_p, na.rm = T),
        peak = max(discharge, na.rm = T),
        n = dplyr::n(),
        .by = year
      )
    # Add unit, id and river info to summary
    sum_df = sum_df |> 
      dplyr::mutate(
        unit = attr(df, "measurement unit"), 
        id = attr(df, "measurement id"), 
        river = attr(df, "body of water")
      )
    # Append df to overall df 
    sum_df_all = rbind(sum_df_all, sum_df)
  }
  # Create plot within the function
  #   Not really necessary bc the summary df is output anyway
  p = ggplot(sum_df_all, aes(x = id)) +
    geom_boxplot(aes(y = peak)) +
    # geom_boxplot(aes(y = mean)) + 
    # geom_boxplot(aes(y = quant)) +
    labs(
      title = paste("Station over years"),
      x = "id"
    ) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  plot(p)
  
  return(sum_df_all)
}

create_copula_df = function(df){
  "
  Create the data frame used to identify the copula / dependence structure.
  Columns: year, duration, peak, volume, id, unit, river, n, p_threshold MORE?
  -> year: year of observation
  -> n: number observations in year
  -> duration: duration of the flood event in minutes [min]
  -> peak: peak discharge of the flood event [m^3 / s]
  -> volume: total volume discharged during flood event [m^3]
  -> id: id of station
  -> unit: name of station
  -> river: name of river
  -> p_threshold: p as in p-quantile used as threshold to determine when flood starts / ends
  
  Note on calculating the (total) volume: 
  Every 15min, an observation is made. To calculate the total volume, we 
  a) assume constant discharge over the 15mins --> unrealistic and did not do that
  b) assume linear change in discharge over the 15mins. i.e. connect the two discharge values with a line
      Area is then given by area under the line (extrapolation).
      Mathematically, it is equivalent to determine 
      area under the line between the two discharge values
      or 
      area under the average of the two discharge values 
  This is done in 2 step approach:
  1) Create df with 15min volume sequences
  2) summarise the previous data frame for each year
  "
  # TODO: This is not really clean. This function should only determine the copula shape. I think the logic of volume should be saved in the threshold dfs when most extreme flood event is determined
  copula_df = df |> 
    #### 1) Build data frame to summarise later on containing the parts required for area under hydrograph:
    # Only consider the most extreme flood event...
    dplyr::filter(peak_flood == TRUE) |> 
    # ... for each year
    dplyr::group_by(year) |> 
    # Use lag operator to determine the average discharge between current and the one 15minutes ago
    # NOTE: This sets the lag of the first observation to NA which is perfect:
    #   The first observation has no "area under the curve", i.e. I do not want to calc area there
    #   The average between first observation and NA becomes NA and is then not considered when summing up the volumes
    # Also, use lag operator on date to account for cases where past observation is not 15minutes ago, but maybe longer
    dplyr::mutate(
      lag_discharge = dplyr::lag(discharge),
      lag_date = dplyr::lag(date)
    ) |> 
    # Build rowwise average between discharge and lagged discharge (i.e. the discharge 15min ago)
    dplyr::rowwise() |>
    dplyr::mutate(
      avg_discharge = mean(c(discharge, lag_discharge)),
      # Quantify how long past obsevation is ago
      # NOTE: Units is seconds to mulitply with discharge which is m^3/s
      time_difference = as.numeric(
        lubridate::as.duration(date - lag_date), 
        "seconds"
      ),
      # Drawing straight line between time points, calculate the (extrapolated) volume = area = avg * duration
      vol_extra = avg_discharge * time_difference
    ) |> 
    # Undo rowwise consideration
    dplyr::ungroup() |>
    #### 2) Summarising previous data frame:
    # Build summary per year i.e. the copula data frame structure
    dplyr::summarise(
      n_floodevent = dplyr::n(),
      duration_min = as.numeric(
        lubridate::as.duration(max(date) - min(date)),
        "minutes"
      ),
      # m^3 / s (Hover "Abfluss" in https://www.lfu.bayern.de/wasser/wasserstand_abfluss/abfluss/index.htm)
      peak = max(discharge),
      # I would like to calc volume NOT assuming constant discharge for 15 min
      # I'f prefer some linear method i.e. connecting 15min points and calc area below connecting line
      # --> Use average discharge between time points
      volume = sum(vol_extra, na.rm = T),
      # p_threshold = p_threshold,
      # id = id,
      # unit = unit,
      # river = river,
      .by = year
    )
  
  # Add meta data: id, unit, river, p_threshold
  copula_df = copula_df |> 
    dplyr::mutate(
      id = unique(df$id),
      unit = unique(df$unit),
      river = unique(df$river),
      p_threshold = unique(df$p_threshold)
    )
  
  return(copula_df)
}

create_and_save_copula_dfs = function(
    in_dir = "../data/output/rdata/threshold_dfs/",
    out_dir = "../data/output/rdata/copula_dfs/"
  ){
  # Get files in the input directory
  filenames = list.files(in_dir)
  
  # Iterate through all file names
  for (filename in filenames){
    
    # Create copula df from df where peak flood is identified
    load(paste(in_dir, filename, sep = "")) # Loads data frame called df
    cop_df = create_copula_df(df)
    
    # Filename to save df in
    df_filename = paste(unique(df$id), "_copula.Rdata", sep = "")
    # Save df in output path
    save(cop_df, file = paste(out_dir, df_filename, sep = ""))
  }
}

load_rdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
}

get_copula_df = function(
    in_dir = "../data/output/rdata/copula_dfs/"
  ){
  "
  Read all copula dfs and join them to one large copula df
  "
  filenames = paste(in_dir, list.files(in_dir, pattern = "*data"), sep = "")
  
  return(purrr::map_dfr(filenames, load_rdata))
}

get_long_df = function(
    in_dir = "../data/output/rdata/threshold_dfs/"
  ){
  #TODO: SAME as get_copula_df, but with different path. lol. Just have one function....!
  "
  Read all long ('extended') data frames and join to one large one
  "
  filenames = paste(in_dir, list.files(in_dir, pattern = "*data"), sep = "")
  
  return(purrr::map_dfr(filenames, load_rdata))
}

# TODO: Did not run this function yet... ever. Will take quite some time!
create_dfs = function(
    data_path = "../data/0 input data/",
    extended_dfs_path = "../data/output/rdata/extended_dfs/",
    threshold_dfs_path = "../data/output/rdata/threshold_dfs/",
    hydrograph_path = "../data/output/graphs/hydrographs/",
    copula_dfs_path = "../data/output/rdata/copula_dfs/",
    p_threshold = c(.75),
    hydros = F,
    evalCompleteness = F, # ONLY set TRUE if joint input dfs are reasonably large!!
    debug = F
  ){
  if (debug) browser()
  
  # Create extended data frames for all CSVs
  create_and_save_dfs(in_dir = data_path, out_dir = extended_dfs_path)
  # Plot of complete cases
  if (evalCompleteness) evaluateCompleteness(in_dir = extended_dfs_path)
  
  # Apply straight line method to identify the most extreme flood event in each year
  # IMPORTANT: Flood event threshold uses QUANTILE of yearly distribution of discharge. Thus, threshold is p-th quantile
  apply_and_save_slm(in_dir = extended_dfs_path, out_dir = threshold_dfs_path, p_threshold = p_threshold)
  
  # Create hydrograph plots for every station and every year so I can go through them and check if it worked
  if (hydros) create_and_save_hydrographs(in_dir = threshold_dfs_path, out_dir = hydrograph_path)
  
  # Create and save all the data frames containing the info for copula determination
  create_and_save_copula_dfs(in_dir = extended_dfs_path, out_dir = copula_dfs_path)
  
  if (debug){
    # Summary statistics (discharge distribution)
    sum_df = apply_summary_stats(in_dir = extended_dfs_path)
    # Distribution number of observations per year 
    ggplot(sum_df, aes(x = unit)) + geom_boxplot(aes(y = n))
  }
}

# # Isar data only
# # create_and_save_dfs(in_dir = "../data/isar data/bis311224/", out_dir = "../data/output/rdata/extended_dfs/")
# # All data in input folder
# create_and_save_dfs(in_dir = "../data/0 input data/", out_dir = "../data/output/rdata/extended_dfs/")
# 
# # ONLY run evaluate completeness with a reasonable amount of files in the in_dir
# # Else it probably will take ages
# evaluateCompleteness(in_dir = "../data/output/rdata/extended_dfs/")
# 
# # Summary statistics (discharge distribution)
# sum_df = apply_summary_stats(in_dir = "../data/output/rdata/extended_dfs/")
# 
# # Apply straight line method to identify the most extreme flood event in each year
# # IMPORTANT: Flood event threshold uses QUANTILE of yearly distribution of discharge. Thus, threshold is p-th quantile
# apply_and_save_slm(in_dir = "../data/output/rdata/extended_dfs/", out_dir = "../data/output/rdata/threshold_dfs/", p_threshold = c(.75))
# 
# # Create hydrograph plots for every station and every year so I can go through them and check if it worked
# create_and_save_hydrographs(in_dir = "../data/output/rdata/threshold_dfs/", out_dir = "../data/output/graphs/hydrographs/")
# 
# # Create and save all the data frames containing the info for copula determination
# create_and_save_copula_dfs(in_dir = "../data/output/rdata/threshold_dfs/", out_dir = "../data/output/rdata/copula_dfs/")

# create_dfs(
#     data_path = "../data/0 input data/",
#     extended_dfs_path = "../data/output/rdata/extended_dfs/",
#     threshold_dfs_path = "../data/output/rdata/threshold_dfs/",
#     hydrograph_path = "../data/output/graphs/hydrographs/",
#     copula_dfs_path = "../data/output/rdata/copula_dfs/",
#     p_threshold = c(.75),
#     evalCompleteness = F,
#     hydros = F,
#     debug = F
#   )


##### SINGLE FILE RUN
# Single file for präsi; Explains approach:
# rel_path = "../data/isar data/fluesse-abfluss/16000708_beginn_bis_31.12.2024_ezw_0.csv"
# load("../data/output/rdata/threshold_dfs/16000708_t0.75.Rdata")
# # Hydrograph for whole time span
# # > As we see, not always all data present
# # For missing data, we use what we have FOR NOW
# ggplot(df, aes(x = date, y = discharge)) + 
#   geom_line()
# # We split hydrograph into years
# ggplot(df, aes(x = date, y = discharge)) + 
#   geom_line() + 
#   geom_vline(xintercept = as.POSIXct(paste(1972:2024, "-01-01 00:00:00", sep = "")), colour = "red", linetype = 2)
# # Hydrograph per year - stacked
# ggplot(df, aes(x = doy, y = discharge, group = year)) + 
#   geom_line()
# # Hydropgraph one year - single
# ref_year = 2021
# ggplot(df |> dplyr::filter(year == ref_year), aes(x = doy, y = discharge)) + 
#   geom_line()
# # We consider the flood event with the highest peak only (following paper here)
# # Identifying using quantile of yearly distribution:
# thresh = df |> dplyr::filter(year == ref_year) 
# thresh= unique(thresh$threshold)
# ggplot(df |> dplyr::filter(year == ref_year), aes(x = doy, y = discharge, colour = peak_flood)) + 
#   geom_line() + 
#   geom_hline(yintercept = thresh)
# # TODO: In this plot: Mark how duration, volume and peak is determined 




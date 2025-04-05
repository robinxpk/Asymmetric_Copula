library(ggplot2)
library(patchwork)

# Load Data ---------------------------------------------------------------
get_copula_df = function(
    # p_threshold = NULL,
    in_dir = "../data/output/rdata/copula_dfs/"
    # all = FALSE
  ){
  "
  Read all copula dfs and join them to one large copula df
  "
  # if (is.null(p_threshold)) assertthat::assert_that(all == TRUE) # If not all dataframes, then p_threhsold must be given
  # if (!is.null(p_threshold)) assertthat::assert_that(all != TRUE) # If all dataframes, then no p_threshold must be given
  
  pattern = "*.Rdata"
  # if (!all) pattern = paste("_", p_threshold, "_copula.Rdata", sep = "")
  filenames = paste(in_dir, list.files(in_dir, pattern = pattern), sep = "")
  
  cop_df = purrr::map_dfr(filenames, load_rdata)
  
  cop_df = cop_df |> 
    dplyr::mutate(
      pobs_peak = copula::pobs(peak),
      pobs_dur = copula::pobs(duration_days),
      pobs_vol= copula::pobs(volume),
      .by = unit
    ) 
  
  return(cop_df)
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

load_rdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
}


#   # load_data-------------------------------------------------------------------------
# GKD Data:
# See: https://www.gkd.bayern.de/en/rivers/discharge/tables
# bzw.: https://www.gkd.bayern.de/en/downloadcenter/wizard?
# NOTE: Because simpler, I will only use data until 31.12.24 (Bc that data is all within one csv)

load_data = function(rel_path, 
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

grab_flood_events = function(df, return_plot = F){
  # Based on: https://onlinelibrary.wiley.com/doi/epdf/10.1002/hyp.14563 
  # Find break points only using days average to reduce noise and faster model fitting
  # When applying this to 15min data, assume that break points refer to the beginning of a day
  reduced_df = df |> dplyr::summarise(
    discharge = mean(discharge),
    .by = doy
  ) 
  
  baseflow = hydroEvents::baseflowB(reduced_df$discharge) # Water flow idependent of event
  streamflow = reduced_df$discharge - baseflow$bf # Water flow due to event
  reduced_df = reduced_df |> 
    dplyr::mutate(
      baseflow = baseflow$bf,
      streamflow = streamflow
    )
  
  events = hydroEvents::eventBaseflow(streamflow) |> 
    dplyr::mutate(
      discharge = max + baseflow$bf[which.max]
    )
  peak = events |> dplyr::filter(max == max(events$max))
  
  # Graph based on which most extreme flood is identified
  event_plot = ggplot(reduced_df, aes(x = doy, y = discharge)) + 
    geom_line(color = "blue") +
    geom_line(aes(x = doy, y = baseflow)) +
    geom_point(data = events, aes(x = which.max, y = discharge), color = "darkgreen") + 
    geom_point(data = peak, aes(x = which.max, y = discharge), color = "red") + 
    geom_vline(xintercept = c(peak$srt, peak$end))
  
  if (return_plot) return(event_plot)
  return(events)
}

grab_most_extreme_flood_event = function(df){
  events = grab_flood_events(df)
  
  peak = events |> dplyr::filter(max == max(events$max))
  return(peak)
}

apply_flood_detection <- function(df, debug = F) {
  # Remove all discharge NAs in the data. They are meaningless anyway to determine the peak flood, but ensure package works fine
  peak = grab_most_extreme_flood_event(df[!is.na(df$discharge), ])
  df = df |> dplyr::mutate(
    peak_flood = doy %in% peak$srt:peak$end
  )
  
  return(df)
}

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

apply_and_save_flood_detection = function(
    in_dir = "../data/output/rdata/extended_dfs/",
    out_dir = "../data/output/rdata/threshold_dfs/",
    pattern = "*.Rdata",
    debug = F
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
  filenames = list.files(in_dir, pattern = pattern)
  
  # Run parallelized
  foreach(
    filename = filenames
  ) %dopar% {
    # Load the df in the Rdata files. These are the data frame produced by create_and_save_dfs
    load(paste(in_dir, filename, sep = ""))

    # Add p to identify quantile value
    # df = df |> dplyr::mutate(p_threshold = p_threshold)
    # A df contains all observations
    # Here, we split the observations by year and determine the most extreme flood event
    df = df |>
      dplyr::group_by(year) |>
      # This part takes each sub-data.frame (grouped by year) and applies the straight line method (via function apply_slm_quants)
      # Theapply_slm_quants return a df containing the original columns and the column peak_flood indicating if flood was most extreme
      # dplyr::group_modify(~ apply_slm_quants(.x, p_threshold = p_threshold, plot = F)) |>
      dplyr::group_modify(~ apply_flood_detection(.x)) |>
      # Join sub-data.frames into one big one containing all years
      dplyr::ungroup()

    # Save the data frames with the addition _t followed by the p used to determine the quantile
    #   _t: means "threshold" and the following p denotes the used p-th quantile used to identify the most extreme flood
    df_filename = paste(unique(df$id), "_t", ".Rdata", sep = "")
    save(df, file = paste(out_dir, df_filename, sep = ""))
  }
  
  # Iterate through all filenames
  # for (filename in filenames){
  #   # Load the df in the Rdata files. These are the data frame produced by create_and_save_dfs
  #   load(paste(in_dir, filename, sep = ""))
  #   
  #   # Add p to identify quantile value
  #   # df = df |> dplyr::mutate(p_threshold = p_threshold)
  #   # A df contains all observations 
  #   # Here, we split the observations by year and determine the most extreme flood event
  #   df = df |>
  #     dplyr::group_by(year) |>
  #     # This part takes each sub-data.frame (grouped by year) and applies the straight line method (via function apply_slm_quants)
  #     # Theapply_slm_quants return a df containing the original columns and the column peak_flood indicating if flood was most extreme
  #     # dplyr::group_modify(~ apply_slm_quants(.x, p_threshold = p_threshold, plot = F)) |>
  #     dplyr::group_modify(~ apply_flood_detection(.x, debug = debug)) |>
  #     # Join sub-data.frames into one big one containing all years
  #     dplyr::ungroup()
  #   
  #   # Save the data frames with the addition _t followed by the p used to determine the quantile
  #   #   _t: means "threshold" and the following p denotes the used p-th quantile used to identify the most extreme flood
  #   df_filename = paste(unique(df$id), "_t", ".Rdata", sep = "")
  #   save(df, file = paste(out_dir, df_filename, sep = ""))
  # }
}

create_hydrograph = function(df){
  "
  This function simply plots the hydrograph for one given year and a given station.
  "
  library(patchwork)
  
  # For the x-axis to be compareable, fix it to the same length
  days_in_year = c(0, 366)

  ## Clean Hydrograph Plot with Flood Indicator ------------------------------
  hydro = ggplot(df, aes(x = doy, y = discharge)) + 
    # Hydrograph-line
    geom_line(color = "darkgreen") + 
    geom_line(data = df |> dplyr::filter(peak_flood == TRUE), color = "red") + 
    # Fix x-axis
    xlim(days_in_year) +
    # In the title, note:
    # station: Name of the station
    # year: Year the data refers to
    # p: p-th quantile used as threshold quantile 
    labs(
      title = paste("Hydrograph - station:", unique(df$unit), "- year:", unique(df$year), "- p:", unique(df$p_threshold))
    ) 
  
  events = grab_flood_events(df[!is.na(df$discharge), ], return_plot = T)
  
  plts =  hydro |events 
  
  return(plts)
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
  
  foreach(
    filename = filenames
  ) %dopar% {
  # Iterate through input files, i.e. data frames
  # for (filename in filenames){
    # load df
    load(paste(in_dir, filename, sep = "")) # Loads data frame called df
    
    # Create plot for each year
    # This is done by splitting the df of a station by year and then applying the create_hydrograph to each of these sub-data.frames
    plots = df |> dplyr::group_split(year) |> purrr::map(create_hydrograph) 
    
    # Assign graph names to each output graph
    graphnames = paste0(out_dir, df$id[[1]], "_", unique(df$year), ".png", sep = "")
    # We have a list of graphs and a list of names. Use both and the ggsave function to save the graphs in the output dir
    purrr::walk2(
      graphnames, 
      plots, 
      ~{
        message("Saving:", .x)
        ggsave(filename = .x, plot = .y, dpi = 400, width = 10, height = 5)
      }
    )
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
  # # TODO: This is not really clean. This function should only determine the copula shape. I think the logic of volume should be saved in the threshold dfs when most extreme flood event is determined
  # copula_df = df |> 
  #   #### 1) Build data frame to summarise later on containing the parts required for area under hydrograph:
  #   # Only consider the most extreme flood event...
  #   dplyr::filter(peak_flood == TRUE) |> 
  #   # ... for each year
  #   dplyr::group_by(year) |> 
  #   # Use lag operator to determine the average discharge between current and the one 15minutes ago
  #   # NOTE: This sets the lag of the first observation to NA which is perfect:
  #   #   The first observation has no "area under the curve", i.e. I do not want to calc area there
  #   #   The average between first observation and NA becomes NA and is then not considered when summing up the volumes
  #   # Also, use lag operator on date to account for cases where past observation is not 15minutes ago, but maybe longer
  #   dplyr::mutate(
  #     lag_discharge = dplyr::lag(discharge),
  #     lag_date = dplyr::lag(date)
  #   ) |> 
  #   # Build rowwise average between discharge and lagged discharge (i.e. the discharge 15min ago)
  #   dplyr::rowwise() |>
  #   dplyr::mutate(
  #     avg_discharge = mean(c(discharge, lag_discharge)),
  #     # Quantify how long past obsevation is ago
  #     # NOTE: Units is seconds to mulitply with discharge which is m^3/s
  #     time_difference = as.numeric(
  #       lubridate::as.duration(date - lag_date), 
  #       "seconds"
  #     ),
  #     # Drawing straight line between time points, calculate the (extrapolated) volume = area = avg * duration
  #     vol_extra = avg_discharge * time_difference
  #   ) |> 
  #   # Undo rowwise consideration
  #   dplyr::ungroup() |>
  #   #### 2) Summarising previous data frame:
  #   # Build summary per year i.e. the copula data frame structure
  #   dplyr::summarise(
  #     # n_floodevent = dplyr::n(),
  #     duration_days = as.numeric(
  #       lubridate::as.duration(max(date) - min(date)),
  #       # "minutes"
  #       "days"
  #     ),
  #     # m^3 / s (Hover "Abfluss" in https://www.lfu.bayern.de/wasser/wasserstand_abfluss/abfluss/index.htm)
  #     peak = max(discharge, na.rm = TRUE),
  #     # I would like to calc volume NOT assuming constant discharge for 15 min
  #     # I'f prefer some linear method i.e. connecting 15min points and calc area below connecting line
  #     # --> Use average discharge between time points
  #     volume = sum(vol_extra, na.rm = T) / 1e6,
  #     # p_threshold = p_threshold,
  #     # id = id,
  #     # unit = unit,
  #     # river = river,
  #     .by = year
  #   )
  
  # Base cop_df on daily average:
  # It does barely change anything for duration and volume, BUT for peak
  # There are some peak values in the data where I am not sure if its a measurement error or not
  # And since actual peak value is of interest, I want one that is somewhat robust towards measurement error
  # Nice side effect: 
  # A LOT faster in computation
  copula_df = df |> 
    dplyr::summarise(
      discharge = mean(discharge, na.rm = T), 
      peak_flood = mean(peak_flood, na.rm = T) == 1,
      .by = c(year, doy)
    ) |> 
    dplyr::filter(peak_flood == TRUE) |> 
    dplyr::summarise(
      peak = max(discharge),
      duration_days = max(doy) - min(doy) + 1, # +1 so the last day is also counted as day where flood took place
      volume = sum(discharge) * 60^2 * 24 / 1e6, 
      .by = year
    )
  
  # Add meta data: id, unit, river
  copula_df = copula_df |> 
    dplyr::mutate(
      id = df$id[1],
      unit = df$unit[1],
      east = df$pos_east[1],
      north = df$pos_north[1],
      river = df$river[1],
      .before = peak
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
    df_filename = paste(df$id[[1]], "_copula.Rdata", sep = "")
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

create_dfs = function(
    data_path = "../data/0 input data/",
    extended_dfs_path = "../data/output/rdata/extended_dfs/",
    threshold_dfs_path = "../data/output/rdata/threshold_dfs/",
    hydrograph_path = "../data/output/graphs/hydrographs/",
    copula_dfs_path = "../data/output/rdata/copula_dfs/",
    pattern = "*.Rdata",
    hydros = F,
    evalCompleteness = F, # ONLY set TRUE if joint input dfs are reasonably large!!
    debug = F
  ){
  if (debug) browser()
  
  # Create extended data frames for all CSVs
  # create_and_save_dfs(in_dir = data_path, out_dir = extended_dfs_path)
  
  # Plot of complete cases
  # ONLY run evaluate completeness with a reasonable amount of files in the in_dir
  # Else it probably will take ages
  if (evalCompleteness) evaluateCompleteness(in_dir = extended_dfs_path)
  
  # Apply straight line method to identify the most extreme flood event in each year
  # IMPORTANT: Flood event threshold uses QUANTILE of yearly distribution of discharge. Thus, threshold is p-th quantile
  apply_and_save_flood_detection(in_dir = extended_dfs_path, out_dir = threshold_dfs_path, pattern = pattern)
  
  # Create hydrograph plots for every station and every year so I can go through them and check if it worked
  # Would be a lot nicer to have all these in one RMD-file with tabs to switch through the years or smth.
  # This would reduce the amount of plots saved alot!
  if (hydros) create_and_save_hydrographs(in_dir = threshold_dfs_path, out_dir = hydrograph_path) 
  
  # Create and save all the data frames containing the info for copula determination
  create_and_save_copula_dfs(in_dir = threshold_dfs_path, out_dir = copula_dfs_path)
  
  if (debug){
    # Summary statistics (discharge distribution)
    sum_df = apply_summary_stats(in_dir = extended_dfs_path)
    # Distribution number of observations per year 
    ggplot(sum_df, aes(x = unit)) + geom_boxplot(aes(y = n))
  }
}
















# Data Analysis -----------------------------------------------------------
get_slope_df = function(file_path = "../data/slopes/considered_slopes.csv", sep = ","){
  slopes = read.csv2(file_path, sep = sep) |> 
    dplyr::select(id, Slope_Percent) |> 
    dplyr::rename(slope = Slope_Percent) |> 
    dplyr::mutate(
      id = as.character(id), 
      slope = as.numeric(slope),
      slope_cat = factor(
        dplyr::case_when(
          slope <= 5 ~ "s",
          slope <= 10 ~ "m",
          slope <= 20 ~ "l",
          slope > 20 ~ "xl",
          TRUE ~ "ERROR"
        ),
        levels = c("s", "m", "l", "xl"),
        ordered = TRUE
      )
    )
  return(slopes)
}

add_slope_col = function(df, slopes, key = "id"){
  joint = df |> dplyr::left_join(slopes, by = key)
  return(joint)
}

add_tau_order_col = function(df){
  df$tau_order = unlist(
    lapply(
      1:nrow(df), 
      function(i) with(df, get_tau_order(tau_vec = c(tau_vd = tau_vd[i], tau_vp = tau_vp[i], tau_dp = tau_dp[i])))
    )
  )
  return(df)
}

get_tau_order = function(tau_vec){
  return(paste0(names(sort(tau_vec)), collapse = "<"))
}

# Rendering River Templates -----------------------------------------------
render_rivertemplate = function(
    df,
    out_dir = "../riverreports/"
  ){
  report_name = paste(out_dir, "Report_", unique(df$river), "_t", unique(df$p_threshold), ".html", sep = "")
  
  rmarkdown::render(
    input = "02_riverreport_template.Rmd",
    output_file = report_name,
    params = list(
      df = df,
      generate = TRUE
    )
  )
}


analyse_dependence = function(df){
  
  vol_dur = ggplot(df, aes(x = volume, y = duration_days)) + 
    geom_point()
  
  vol_peak = ggplot(df, aes(x = volume, y = peak)) + 
    geom_point()
  
  dur_peak = ggplot(df, aes(x = duration_days, y = peak)) + 
    geom_point()
  
  combined_plot = (vol_dur | vol_peak | dur_peak) 
  print(combined_plot)
  
  print(paste("Vol-Dur: ", cor(df$volume, df$duration_days, method = "kendall")))
  print(paste("Vol-Peak:", cor(df$volume, df$peak, method = "kendall")))
  print(paste("Dur-Peak:", cor(df$duration_days, df$peak, method = "kendall")))
}

# Simulation --------------------------------------------------------------

get_nac_tau = function(tauvec){
  # Based on 3 taus, return largest and build average above the other two
  # Assumption: 3rd is largest tau
  tau = c(mean(tauvec[1:2]), tauvec[3])
  return(tau)
}



nac_tau2theta = function(family_index, tau){
  return(
    c(
      HAC::tau2theta(tau = tau[1], type = family_index),
      HAC::tau2theta(tau = tau[2], type = family_index)
    )
  )
}
nac_theta2tau = function(family_index, theta){
  theta = unname(theta)
  return(
    c(
      HAC::theta2tau(theta = theta[1], type = family_index),
      HAC::theta2tau(theta = theta[2], type = family_index)
    )
  )
}

fit_ac = function(mat, cops, debug = FALSE){
  # HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
  # Solution: copula Package: Can fit ONLY non-nested ACs for more than 2 variables
  # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
  # Due to the implementation in this package: 1) fit possible models 2) Use AIC to select the best one 
  if (debug) browser()
  ac_fits = lapply(cops, function(cop) copula::emle(u = mat, cop = copula::onacopula(family = cop, nacStructure = C(1, 1:3))))
  
  # Select the one with smallest AIC
  # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
  ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
  ac_best_fit = which.max(ac_lls)
  # if (is.integer(ac_best_fit) && length(ac_best_fit) == 0) ac_best_fit = sample(1:3, 1) # TODO: TEMP SOL: In case of it breaking down, select one copula at random
  ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
  
  # Actual copula
  ac = copula::onacopulaL(family = cops[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
  attr(ac, "logLik") = ac_lls[ac_best_fit]
  
  return(ac)
}

fit_nac = function(mat, families){
  # Estimate copula using implemented selection method
  # HAC package does not offer way to select the copula family
  # Solution: Fit all considered families and use AIC to select accordingly
  
  # Allowed copula families
  types = unlist(unname(families))
  
  # Select one with smallest AIC. NOTE: p const --> use logLik
  nac_fits = lapply(types, function(type) HAC::estimate.copula(mat, type = type))
  # If any of the NACs suggests a non-nested structure, remove it. 
  # Two reasons: 
  # 1) I already fit a non-nested structure
  # 2) The HAC logLik function breaks down for these functions. And since I already fit one anyway, I use the simplest way to deal with this issue
  # Non-nested NACs have a tree structure of length 4. Use this to filter them out:
  nac_fits = nac_fits[!lapply(nac_fits, function(nac) length(nac$tree)) == 4]
  
  nac_lls = lapply(nac_fits, function(nac) get_nac_loglik(mat, nac))
  nac_best_fit = which.max(nac_lls)
  
  # Best fit
  nac = nac_fits[[nac_best_fit]]
  
  attr(nac, "logLik") = nac_lls[nac_best_fit]
  attr(nac, "theta") = get_nac_estimates(nac)
  attr(nac, "tau") = nac_theta2tau(nac$type, attr(nac, "theta"))
  
  return(nac)
}

get_nac_loglik = function(mat, nac_mdl){
  return(HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE))
}

get_nac_estimates = function(nac_mdl){
  return(c(nac_mdl$tree[[3]], nac_mdl$tree[[1]][[3]]))
}

fit_vine = function(mat, families){
  # The VineCopula package is faster and applicable as long as we do not also estimate covariates
  # Families: 3, 4, 5 (see docu)
  vine = VineCopula::RVineStructureSelect(data = mat, rotations = FALSE, familyset = families)
  return(vine)
}

get_nac_name = function(idx, inverse_copula_families = list("1" = "Gumbel", "3" = "Clayton", "5" = "Frank")){
  return(inverse_copula_families[as.character(idx)][[1]])
}

get_ac_estimate = function(ac) unname(ac@copula@tau( ac@copula@theta ))

run_one_nac = function(
    seed,
    n,
    cop,
    taus,
    copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5) # Copula families contained in the package
    ){
  # Simulate from true model ------------------------------------------------
  # For reproducibility
  set.seed(seed)
  
  # Draw random tau vector and determine nac structure from this
  rtau = sample(taus, 1)
  river = names(rtau)
  tau = get_nac_tau(tauvec = rtau[[1]])
  theta = nac_tau2theta(family_index = copula_families[cop], tau = tau)
  
  # Create HAC-object
  mdl = HAC::hac.full(type = copula_families[cop], y = c("v3", "v2", "v1"), theta = theta)
  # Draw sample according to true_mdl (simulated sample)
  mat = HAC::rHAC(n, mdl) 
  
  # Fit models --------------------------------------------------------------
  ac = fit_ac(mat, names(copula_families))
  
  nac = fit_nac(mat, copula_families)
 
  vine = fit_vine(mat, families = c(3, 4, 5))
  
  # Save results ------------------------------------------------------------
  res = data.frame(
    list(
      # True values
      seed = seed,
      river = river,
      true_tau_outer = tau[1],
      true_theta_outer = theta[1],
      true_tau_inner = tau[2],
      true_theta_inner = theta[2],
      # Estimated values
      fit_cop = get_nac_name(nac),
      fit_theta_outer = attr(nac, "theta")[1],
      fit_theta_inner = attr(nac, "theta")[2],
      fit_tau_outer = attr(nac, "tau")[1],
      fit_tau_inner = attr(nac, "tau")[2],
      nac_aic = ll2aic(ll = attr(nac, "logLik")[[1]], p = 2),
      nac_kl = klMonteCarlo(mdl, nac),
      # Other copulas / Misspecifications
      # Results of AC fit
      ac_tau = get_ac_estimate(ac),
      ac_aic = ll2aic(ll = attr(ac, "logLik")[[1]], p = 1),
      ac_kl = klMonteCarlo(mdl, HAC::nacopula2hac(ac), est_mdl_AC = TRUE),
      # Results for Vine
      vine_aic = vine$AIC,
      vine_kl = klMonteCarlo(mdl, vine, est_mdl_vine = TRUE)
    )
  )
  return(res)
}




# For each sample size (n), each copula family, each dependence structure (AC, NAC, Vine), tau:
#   Repeat B times: 
#     Draw random sample  
#   For each sample, fit AC, NAC and Vine copula and evaluate
# run_one_n_ac = function(
#     seed, 
#     n,
#     cop,
#     dep,
#     copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5), # Copula families contained in the package
#     beta_a = 2.5, 
#     beta_b = 1.5
#   ){
#   # Simulate from true model ------------------------------------------------
#   # For reproducibility
#   set.seed(seed)
#   
#   # If symmetric, the values of the 'inner' and 'outer' tau bzw. copula parameter are simply identical
#   tau_inner = rbeta(n = 1, shape1 = beta_a, shape2 = beta_b)
#   tau_outer = tau_inner # base case is symmetric AC
#   if (dep == "nac") tau_outer = 1/2 * tau_inner # Simple solution to ensure outer parameter is smaller than inner
#   
#   # Define a selection of possible tau
#   # Derive a selection from the empirical observations? 
#   # AC: Take average of the observed tau
#   # NAC: Take most nested and average of other 2
#   # Vine: Take the observed taus
#   
#   # Package uses integer for copulas. Get this integer 
#   fam = copula_families[cop]
#   
#   # Create HAC-object
#   true_mdl = HAC::hac.full(
#     type = fam, 
#     y = c("v3", "v2", "v1"), 
#     theta = c(HAC::tau2theta(tau = tau_outer, type = fam), HAC::tau2theta(tau = tau_inner, type = fam))
#   )
#   
#   # Draw sample according to true_mdl (simulated sample)
#   mat = HAC::rHAC(n, true_mdl) 
#   attr(mat, "true_mdl") = true_mdl 
#   attr(mat, "tau") = c(outer = tau_outer, inner = tau_inner)
#   
#   # Fit models --------------------------------------------------------------
#   
#   # I) (symmetric) Archimedean copulas ----
#   # Funny enough, HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
#   # Lucky me, the copula package can fit ONLY non-nested ACs for more than 2 variables
#   # Copula packages are a mess, wtf...
#   # Also, due to the implementation in this package, I first fit all models, then use AIC to select the best one and finally create the best model
#   # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
#   ac_fits = lapply(names(copula_families), function(name) copula::emle(u = mat, cop = copula::onacopula(family = name, nacStructure = C(1, 1:3))))
#   # Select the one with smallest AIC
#   # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
#   ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
#   ac_best_fit = which.max(ac_lls)
#   ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
#   # Actual copula
#   ac_mdl = copula::onacopulaL(family = names(copula_families)[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
# 
#   
#   # II) Nested Archimedean copulas ----
#   # Estimate copula using implemented selection method
#   nac_mdl = HAC::estimate.copula(mat) # TODO There is no structure selection!! Again, solve this using AIC use type 1, 3, 5
#   # Calculate ll to evaluate AIC
#   nac_ll  = HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE)
#   
#   estimates = c(outer = nac_mdl$tree[[3]], inner = nac_mdl$tree[[1]][[3]])
#   
#   # III) Vine copulas ----
#   # The VineCopula package is faster and applicable as long as we do not also estimate covariates
#   vine_mdl = VineCopula::RVineStructureSelect(
#     data = mat, 
#     rotations = FALSE, familyset = c(3, 4, 5) # Only allow for the considered copula families
#   )
#   vine_mdl
#   
#   # Save results in df ----
#   res = data.frame(
#     list(
#       seed = seed,
#       true_tau_inner = tau_inner,
#       true_tau_outer = tau_outer,
#       # Results of AC fit
#       ac_selected_cop = names(copula_families)[ac_best_fit],
#       ac_theta = ac_mle,
#       ac_tau = HAC::theta2tau(ac_mle, type = copula_families[names(copula_families)[ac_best_fit]]),
#       ac_aic = ll2aic(ll = ac_lls[[ac_best_fit]], p = 1),
#       ac_bic = ll2bic(ll = ac_lls[[ac_best_fit]], p = 1),
#       ac_kl = klMonteCarlo(true_mdl, HAC::nacopula2hac(ac_mdl), est_mdl_AC = TRUE),
#       # Results of NAC fit
#       nac_selected_cop = HAC::hac2nacopula(nac_mdl)@copula@name,
#       nac_theta_outer = estimates["outer"][[1]],
#       nac_theta_inner = estimates["inner"][[1]],
#       nac_tau_outer = HAC::theta2tau(theta = estimates["outer"][[1]], type = nac_mdl$type),
#       nac_tau_inner = HAC::theta2tau(theta = estimates["inner"][[1]], type = nac_mdl$type),
#       nac_aic = ll2aic(ll = nac_ll, p = 2),
#       nac_bic = ll2bic(ll = nac_ll, p = 2),
#       nac_kl = klMonteCarlo(true_mdl, nac_mdl),
#       # Results of Vine fit
#       vine_aic = vine_mdl$AIC,
#       vine_bic = vine_mdl$BIC,
#       vine_kl = klMonteCarlo(true_mdl = true_mdl, est_mdl = vine_mdl, est_mdl_vine = TRUE)
#     )
#   )
# 
#   return(res)
# }
run_one_vine = function(
    seed,
    n,
    taus,
    vine_copula_families = list("Gumbel" = 4, "Clayton" = 3, "Frank" = 5), # Copula families contained in the VineCopula package
    hac_copula_families = list("Gumbel" = 2, "Clayton" = 3, "Frank" = 5), # Copula families contained in HAC copula package
    vine_colnames = c("1-3", "2-3", "1-2")
    ){
  # Simulate from true model ------------------------------------------------
  # For reproducibility
  set.seed(seed)

  # Draw random tau vector and determine nac structure from this
  rtau = sample(taus, 1)
  river = names(rtau)
  tau = unlist(rtau)
  names(tau) = vine_colnames
  
  # Vine matrix defining the (un)conditional copulas in the model
  # 1 - 2 - 3 
  vine_matrix = matrix(
    c(
      1, 0, 0,
      3, 2, 0,
      2, 3, 3
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )

  # Randomly drawing the combination of copula families
  fams = unlist(sample(vine_copula_families, size = 3, replace = T))
  names(fams) = vine_colnames
  # Save families in family copula matrix
  family_matrix = matrix(0, nrow = 3, ncol = 3)
  family_matrix[2, 1] = fams["1-3"]
  family_matrix[3, 1:2] = fams[c("1-2", "2-3")]

  params = matrix(0, nrow = 3, ncol = 3)
  theta = c(
    "1-3" = VineCopula::BiCopTau2Par(fams["1-3"], tau["1-3"]),
    "1-2" = VineCopula::BiCopTau2Par(fams["1-2"], tau["1-2"]),
    "2-3" = VineCopula::BiCopTau2Par(fams["2-3"], tau["2-3"])
  )
  params[2, 1] = theta["1-3"]
  params[3, 1:2] = c(theta["1-2"], theta["2-3"])

  rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
  mat = VineCopula::RVineSim(n, RVM = rvmat)

  attr(mat, "rvm") = rvmat
  colnames(mat) = c("v1", "v2", "v3")
  
  # Fit models --------------------------------------------------------------
  ac = fit_ac(mat, names(hac_copula_families))

  nac = fit_nac(mat, hac_copula_families)

  vine = fit_vine(mat, families = c(3, 4, 5))

  # Save results ------------------------------------------------------------
  res = data.frame(
    list(
      # True values
      seed = seed,
      river = river,
      true_tau_12 = tau["1-2"],
      true_fam_12 = fams["1-2"],
      true_theta_12 = theta["1-2"],
      true_tau_23 = tau["2-3"],
      true_fam_23 = fams["2-3"],
      true_theta_23 = theta["2-3"],
      true_tau_13 = tau["1-3"],
      true_fam_13 = fams["1-3"],
      true_theta_13 = theta["1-3"],
      # Estimated values
      fit_tau_12 = vine$tau[3, 1],
      fit_fam_12 = vine$family[3, 1],
      fit_theta_12 = vine$par[3, 1],
      fit_tau_23 = vine$tau[3, 2],
      fit_fam_23 = vine$family[3, 2],
      fit_theta_23 = vine$par[3, 2],
      fit_tau_13 = vine$tau[2, 1],
      fit_fam_13 = vine$family[2, 1],
      fit_theta_13 = vine$par[2, 1],
      vine_aic = vine$AIC,
      vine_kl = vine_klMonteCarlo(rvmat, vine, est = "vine"),
      # Other copulas / Misspecifications
      # NAC fit
      nac_tau_outer = attr(nac, "tau")[1],
      nac_tau_inner = attr(nac, "tau")[2], 
      nac_aic = ll2aic(ll = attr(nac, "logLik")[[1]], p = 2),
      nac_kl = vine_klMonteCarlo(rvmat, nac, est = "nac"),
      # AC fit
      ac_aic = ll2aic(ll = attr(ac, "logLik")[[1]], p = 1),
      ac_kl = vine_klMonteCarlo(rvmat, HAC::nacopula2hac(ac), est = "ac")
    )
  )
  return(res)
}

vine_klMonteCarlo = function(
    rvmat,
    est_mdl, 
    est = "vine", 
    values =  seq(from = 0.01, to = 0.99, length.out = 25) # Keep from (to) relatively high (low) simmplifying numerical stability. Downside: KL not considering full support..........lol.
  ){
  grid = expand.grid(v1 = values, v2 = values, v3 = values)
  
  true_d = VineCopula::RVinePDF(grid, rvmat)
  
  # ifelse cannot return matrices, aparently. Thus, keep it in two if-statements...
  if (est == "vine") est_d = VineCopula::RVinePDF(grid, est_mdl)
  if (est == "ac") for (i in 1:3) est_mdl$tree[[i]] = paste("v", i, sep = "")
  if (est != "vine") est_d = HAC::dHAC(as.matrix(grid), est_mdl)

  return(mean(log(true_d / est_d) ))
}
 
# TODO
# run_one_vine = function(
#     seed, 
#     n,
#     cop,
#     dep
#   ){
#   # Simulate from true model ------------------------------------------------
#   # Tau for the dependence structure
#   tau_13 = rbeta(n = 1, shape1 = beta_a, shape2 = beta_b)
#   tau_23 = 3/5 * tau_13
#   tau_12 = 1/3 * tau_13
#   
#   # Vine matrix defining the (un)conditional copulas in the model
#   # 1 - 2 - 3
#   vine_matrix = matrix(
#     c(
#       1, 0, 0, 
#       3, 2, 0,
#       2, 3, 3
#     ),
#     nrow = 3, ncol = 3,
#     byrow = TRUE
#   )
#   
#   # Randomly drawing the combination of copula families
#   fams = unlist(sample(copula_families, size = 3, replace = T))
#   # Save families in family copula matrix
#   family_matrix = matrix(0, nrow = 3, ncol = 3)
#   family_matrix[2, 1] = fams[1]
#   family_matrix[3, 1:2] = fams[2:3]
#   
#   params = matrix(0, nrow = 3, ncol = 3)
#   params[2, 1] = VineCopula::BiCopTau2Par(fams[1], tau_13)
#   params[3, 1:2] = c(
#     VineCopula::BiCopTau2Par(fams[2], tau_12),
#     VineCopula::BiCopTau2Par(fams[3], tau_23)
#   )
#   
#   rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
#   mat = VineCopula::RVineSim(n, RVM = rvmat)
#   
#   attr(mat, "rvm") = rvmat
#   colnames(mat) = c("v1", "v2", "v3")
#   
#   # Fit models --------------------------------------------------------------
#   
#   
#   # I) (symmetric) Archimedean copulas ----
#   # Funny enough, HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
#   # Lucky me, the copula package can fit ONLY non-nested ACs for more than 2 variables
#   # Copula packages are a mess, wtf...
#   # Also, due to the implementation in this package, I first fit all models, then use AIC to select the best one and finally create the best model
#   # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
#   ac_fits = lapply(names(copula_families), function(name) copula::emle(u = mat, cop = copula::onacopula(family = name, nacStructure = C(1, 1:3))))
#   # Select the one with smallest AIC
#   # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
#   ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
#   ac_best_fit = which.max(ac_lls)
#   ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
#   # Actual copula
#   ac_mdl = copula::onacopulaL(family = names(copula_families)[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
#   
#   # II) Nested Archimedean copulas ----
#   # Estimate copula using implemented selection method
#   nac_mdl = HAC::estimate.copula(mat)
#   # Calculate ll to evaluate AIC
#   nac_ll = HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE)
#   
#   # III) Vine copulas ----
#   # The VineCopula package is faster and applicable as long as we do not also estimate covariates
#   vine_mdl = VineCopula::RVineStructureSelect(
#     data = mat, 
#     rotations = FALSE, familyset = c(3, 4, 5) # Only allow for the considered copula families
#   )
#   vine_mdl
#   
#   # Save result df ----
#   res = data.frame(
#     list(
#       seed = seed,
#       # Results of AC fit
#       ac_aic = ll2aic(ll = ac_lls[[ac_best_fit]], p = 1),
#       ac_kl = klMonteCarlo(true_mdl, HAC::nacopula2hac(ac_mdl), est_mdl_AC = TRUE),
#       # Results of NAC fit
#       nac_aic = ll2aic(ll = nac_ll, p = 2),
#       nac_kl = klMonteCarlo(true_mdl, nac_mdl),
#       # Results of Vine fit
#       
#       vine_aic = vine_mdl$AIC,
#       vine_kl = klMonteCarlo(true_mdl = true_mdl, est_mdl = vine_mdl, est_mdl_vine = TRUE)
#     )
#   )
#   
#   return(res)
# }

klMonteCarlo = function(
    true_mdl, 
    est_mdl, 
    est_mdl_AC = FALSE, 
    est_mdl_vine = FALSE, 
    values =  seq(from = 0.01, to = 0.99, length.out = 25) # Keep from (to) relatively high (low) simmplifying numerical stability. Downside: KL not considering full support..........lol.
                                                           # This implies that I am "numerically blind" for differences below or above these thresholds. 
  ){
  "
  Function to numerically approximate the KL divergence.
  Not fully reliable tbh... have to work on it. But not now, I postpone this until later.
  "
  # Ensure correct names (Is an issue when dealing with copula to HAC transformed copula. Cannot deal with it any other way aparently)
  if (est_mdl_AC) for (i in 1:3) est_mdl$tree[[i]] = paste("v", i, sep = "")
  
  # Grid points where the copula density is evaluated on
  grid = expand.grid(v1 = values, v2 = values, v3 = values)
  
  # Densities
  true_d = HAC::dHAC(as.matrix(grid), true_mdl)
  # ifelse cannot return matrices, aparently. Thus, keep it in two if-statements...
  if (est_mdl_vine) est_d = VineCopula::RVinePDF(newdata = grid, RVM = est_mdl)
  if (!est_mdl_vine) est_d = HAC::dHAC(as.matrix(grid), est_mdl)

  # Numerically estimate E[log(p/q)] by mean(log(p/q)) 
  # The way I calculate KL now lead to NAs for:
  # tau_inner = 0.2011794
  # tau_outer = 0.1005897
  # Not sure why tho...
  return(mean(log(true_d / est_d) ))
}

ll2aic = function(ll, p){
  return(-2 * ll + 2 * p)
}


draw_model_beta = function(){
  "
  Linear model used to calculate the underlying - true - correlation.
  
  in 50% of the cases, I want the model to have a covariate included (i.e. beta != 0) and 
  in the other cases, no covariate in the true model (beta == 0 = intercept / constant model)
  "
  # Intercept affects 
  
  # Inclusion is 50:50
  include = rbinom(n = 1, size = 1, prob = 0.5)
  beta = ifelse(include, rnorm(n = 1, mean = 5, sd = 1), 0)
  return(beta)
}

read_dep_files = function(
    true_dep ,
    in_dir = "../data/simulation/"
  ){
  filenames = paste(
    in_dir, 
    list.files(in_dir, pattern = paste("dep", true_dep, sep = "")),
    sep = ""
  )
  
  return(purrr::map_dfr(filenames, load_depdata))
}

load_depdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
  
  # Append attributes to df. Necessary before merging them into one big df
  n = attr(env$res, "n")
  dep = attr(env$res, "dep")
  cop = attr(env$res, "cop")
  if (dep == "vine") cop = "vine"
  
  # Sanity messages
  message(paste("n:", n, "-- cop:", cop, "-- dep:", dep,"-- #seeds:", nrow(env$res)))
  
  return(env$res <- env$res |> dplyr::mutate(n = n, cop = cop, dep = dep, .after = seed))
}


# Simulation Analysis -----------------------------------------------------
klplots = function(
    df,
    cop_name,
    dens_alpha = 1,
    col_ac = "red",
    col_nac = "blue",
    col_vine = "green",
    scales = "fixed"
  ){
  p = df |> 
    dplyr::filter(cop == cop_name) |> 
    dplyr::select(n, contains("_kl")) |> 
    tidyr::pivot_longer(
      cols = contains("_kl"),
      names_to = "dep",
      values_to = "kld"
    ) |>
    dplyr::mutate(dep = as.factor(stringr::str_remove(dep, "_kl"))) |>
    ggplot() + 
    geom_density(aes(x = kld, color = dep), alpha = dens_alpha) +
    geom_vline(xintercept = 0, color = "black") + 
    facet_wrap(~ n, scale = scales) +
    labs(
      title = "Kullback Leibler by Sample Size and Fit",
      x = "Kullback Leibler Divergence",
      y = "Density"
    ) +
    theme(legend.position = "bottom") 
    
  return(p)
}

get_vine_famname = function(idx){
  vine_famlist = list("4" = "Gumbel", "3" = "Clayton", "5" = "Frank")
  return(unname(unlist(lapply(idx, function(i) vine_famlist[as.character(i)]))))
}



# Presentation Plotting ---------------------------------------------------
gkd2gg = function(
    df, 
    coord_cols, 
    current_crs = 25832, # ETRS25832
    into_crs = 4326 # EPSG4326
  ){
  "
  Takes positions given by GKD website (CooRdinateSystem[crs] = ETRS25832) and returns a coordinate system ggplot can work with (crs = EPSG4326).
  "
  return(
    sf::st_transform(
      sf::st_as_sf(df, coords = coord_cols, crs = current_crs),
      crs = into_crs
    )
  )
}







# Copula oder so ----------------------------------------------------------
filter_infeasible_stations = function(
    cop_df, min_nyears = 15 # Sim had min 15 obs, so stick to that
  ){
  feasible_stations = cop_df |> 
    dplyr::summarise(
      years = dplyr::n(),
      .by = unit
    ) |> 
    dplyr::filter(years > min_nyears) |> 
    dplyr::select(unit)
  
  return(cop_df |> dplyr::filter(unit %in% feasible_stations$unit))
}

get_density_values = function(vine, dgrid, unit_name){
  plot_df = as.data.frame(dgrid) |>
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    # 1-2: index [3, 1]
    # 1-3: index [2, 1]
    # 2-3: index [3, 2]
    dplyr::mutate(
      z12 = VineCopula::BiCopPDF(x, y, family = vine$family[3, 1], par = vine$par[3, 1]),
      z13_2 = VineCopula::BiCopPDF(x, y, family = vine$family[2, 1], par = vine$par[2, 1]),
      z23 = VineCopula::BiCopPDF(x, y, family = vine$family[3, 2], par = vine$par[3, 2])
    ) |>
    tidyr::pivot_longer(
      cols = contains("z"),
      names_to = "vars",
      values_to = "dens"
    ) |>
    # Standardize density values so scale does not matter
    dplyr::group_by(vars) |>
    dplyr::mutate(
      density = (dens - mean(dens)) / sd(dens)
    ) |> 
    dplyr::ungroup()
  # Add family 
  plot_df$fam = rep(c(f12 = vine$family[3, 1], f13 = vine$family[2, 1], f23 = vine$family[3, 2]), nrow(plot_df) / 3)
  plot_df$unit = unit_name
  return(plot_df)
}

get_cdf_values = function(vine, dgrid, unit_name){
  plot_df = as.data.frame(dgrid) |>
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    # 1-2: index [3, 1]
    # 1-3: index [2, 1]
    # 2-3: index [3, 2]
    dplyr::mutate(
      z12 = VineCopula::BiCopCDF(x, y, family = vine$family[3, 1], par = vine$par[3, 1]),
      z13_2 = VineCopula::BiCopCDF(x, y, family = vine$family[2, 1], par = vine$par[2, 1]),
      z23 = VineCopula::BiCopCDF(x, y, family = vine$family[3, 2], par = vine$par[3, 2])
    ) |>
    tidyr::pivot_longer(
      cols = contains("z"),
      names_to = "vars",
      values_to = "cdf"
    ) 
    # Standardize density values so scale does not matter
    # dplyr::group_by(vars) |>
    # dplyr::mutate(
    #   density = (cdf - mean(cdf)) / sd(cdf)
    # ) |> 
    # dplyr::ungroup()
  # Add family 
  # plot_df$fam = rep(c(f12 = vine$family[3, 1], f13 = vine$family[2, 1], f23 = vine$family[3, 2]), nrow(plot_df) / 3)
  plot_df$unit = unit_name
  return(plot_df)
}

get_tail_dependencies = function(vine, name){
  utdp = vine$taildep$upper
  ltdp = vine$taildep$lower
  data.frame(
    unit = name,
    ltdp_12 = ltdp[3, 1],
    ltdp_23 = ltdp[3, 2],
    ltdp_13_2 = ltdp[2, 1],
    utdp_12 = utdp[3, 1],
    utdp_23 = utdp[3, 2],
    utdp_13_2 = utdp[2, 1]
  )
}

get_synthetic_data = function(vine, n, unit_name){
  as.data.frame(VineCopula::RVineSim(n, RVM = vine)) |> dplyr::mutate(unit = unit_name)
}

inverse_ecdf <- function(u, data) {
  quantile(data, probs = u, type = 1)  # Use type = 1 for stepwise approximation
}

inverse_ecdf_unitwise <- function(syn, syn_col, df, df_col, unit_name) {
  syn_ = (unlist((syn |> dplyr::filter(unit == unit_name))[syn_col]))
  names(syn_) = df_col
  df_ = unname(unlist((df |> dplyr::filter(unit == unit_name))[df_col]))
  return(unname(sapply(syn_, inverse_ecdf, data = df_)))
}

get_contour = function(
    rel, 
    splot_df, 
    sdf, 
    varx, 
    vary, 
    bwidth = 0.1, 
    plt_pts = TRUE, 
    title = "TITLE",
    x_lab = "x",
    y_lab = "y",
    contour_alpha = 1,
    z = "density"
  ){
  p = ggplot() +
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    geom_contour(data = splot_df |> dplyr::filter(vars == rel), aes_string(x = "x", y = "y", z = z), binwidth = bwidth, alpha = contour_alpha) + 
    labs(
      title = title,
      y = y_lab,
      x = x_lab 
    ) + 
    scale_x_continuous(breaks = c(0, 1)) + 
    scale_y_continuous(breaks = c(0, 1))
  if (plt_pts) p = p + geom_point(data = sdf, mapping = aes_string(x = varx, y = vary), alpha = .8)
  return(p)
}

get_syn_scatter = function(
    ssyn_df, 
    varx_syn,  
    vary_syn, 
    sdf,  
    varx_df, 
    vary_df,  
    syn_color = "lightblue", 
    syn_alpha = 0.3,
    x_lab = "x",
    y_lab = "y",
    x_min = NULL,
    x_max = NULL,
    y_min = NULL,
    y_max = NULL
  ){
  p = ggplot() + 
    geom_point(data = ssyn_df, mapping = aes_string(x = varx_syn, y = vary_syn), color = syn_color, alpha = syn_alpha) + 
    geom_point(data = sdf, mapping = aes_string(x = varx_df, y = vary_df)) +
    labs(
      x = x_lab,
      y = y_lab
    ) 
    if (!is.null(c(x_min, x_max))) p = p + scale_x_continuous(breaks = c(x_min, x_max)) 
    if (!is.null(c(y_min, y_max))) p = p + scale_y_continuous(breaks = c(y_min, y_max))
  return(p)
}

marginal_fit = function(vec, type){
  return(extRemes::fevd(vec, type = type))
}

dmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  shape = FALSE
  if (type != "Gumbel") shape = mle[["shape"]]
  
  
  return(
    extRemes::devd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = shape,
      type = type 
    )
  )
}

pmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  shape = FALSE
  if (type != "Gumbel") shape = mle[["shape"]]
  
  return(
    extRemes::pevd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = shape,
      type = type 
    )
  )
}
qmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  
  return(
    extRemes::qevd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = mle[["shape"]], 
      type = type 
    )
  )
  
}

invPIT = function(name, df, u){
  vec = unname(unlist(df[, name]))
  fit = logspline::logspline(vec, lbound = 0)
  
  return(logspline::qlogspline(u, fit))
}




showcase_copula_contours = function(tau, n_gen_sim = 5000, title = "TODO: Title",
                                    textsize_lab = 20, textsize_tick = 10, textsize_strip = 10, textsize_title = 20){
  # Formulas according to Hofert "Elements of copula Modeling with R" p. 98
  gen_clayton = function(t, theta) (1 + t)^(-1 / theta)
  gen_frank = function(t, theta) -log(1 - exp(-t) * (1 - exp(-theta))) / theta
  gen_gumbel = function(t, theta) exp(-t^(1 / theta))
  
  cop_fams = c("clayton" = 3, "gumbel" = 4, "frank" = 5)
  vine_matrix = matrix(
    c(
      1, 0, 0,
      3, 2, 0,
      2, 3, 3
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )
  
  family_matrix = matrix(0, nrow = 3, ncol = 3)
  family_matrix[2, 1] = cop_fams["clayton"]
  family_matrix[3, 1:2] = cop_fams[c("gumbel", "frank")]
  
  theta = c(
    "clayton" = VineCopula::BiCopTau2Par(cop_fams["clayton"], tau),
    "gumbel" = VineCopula::BiCopTau2Par(cop_fams["gumbel"], tau),
    "frank" = VineCopula::BiCopTau2Par(cop_fams["frank"], tau)
  )
  
  params = matrix(0, nrow = 3, ncol = 3)
  params[2, 1] = theta["clayton"]
  params[3, 1:2] = c(theta["gumbel"], theta["frank"])
  
  rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
  
  # Contours
  x = seq(from = 0.001, to = 0.99, length.out = 200)
  y = seq(from = 0.001, to = 0.99, length.out = 200)
  density_grid = expand.grid(x = x, y = y)
  
  plot_df = get_density_values(rvmat, density_grid, "showcase") |>
    dplyr::mutate(
      vars = dplyr::case_when(
        vars == "z12" ~ "Gumbel",
        vars == "z13_2" ~ "Clayton",
        vars == "z23" ~ "Frank"
      )
    )
  
  contours = ggplot(plot_df) +
    geom_contour(aes(x = x, y = y, z = density), binwidth = 0.5) + 
    labs(x = latex2exp::TeX("$u_1$"), y = latex2exp::TeX("$u_2$")) + 
    theme(axis.text = element_text(size = textsize_tick), axis.title = element_text(size = textsize_lab), strip.text = element_text(size = textsize_strip)) +
    facet_wrap(~vars)
  
  # Generator plots
  t = seq(0, 9, length.out = 1000)
  # Clayton Generator
  gens = data.frame(
    # Clayton = copula::copClayton@psi(t, theta["clayton"]),
    Clayton = gen_clayton(t, theta["clayton"]),
    # Gumbel Generator
    # Gumbel = copula::copGumbel@psi(t, theta["gumbel"]),
    Gumbel = gen_gumbel(t, theta["gumbel"]),
    # Frank Generator
    # Frank = copula::copFrank@psi(t, theta["frank"]),
    Frank = gen_frank(t, theta["frank"]),
    t = t
  ) |> 
    tidyr::pivot_longer(
      cols = c(Clayton, Gumbel, Frank),
      names_to = "family",
      values_to = "generator"
    ) |> 
    ggplot() + 
    geom_line(aes(x = t, y = generator), linewidth = 0.8) +
    facet_wrap(~family) +
    xlim(0, 1) + 
    theme(axis.text = element_text(size = textsize_tick), axis.title = element_text(size = textsize_lab), strip.text = element_text(size = textsize_strip), title = element_text(size = textsize_title)) +
    labs(x = "", y = "Generator", title = title)  
    
  
  gen_cont = gens / contours
  return(gen_cont)
}

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

fit_nacs = function(cop_df, all_units){
  nac_fams = list("1" = "Gumbel", "3" = "Clayton", "5" = "Frank")
  
  nacs = lapply(
    all_units,
    function(name){
      fit_nac(mat = cop_df |> dplyr::filter(unit == name) |> dplyr::select(contains("pobs")) |> as.matrix(), families = c(1, 3, 5))
    }
  )
  names(nacs) = all_units
  
  return(nacs)
}


fit_vines = function(
    cop_df, 
    all_units,
    # Vine structure we assumed (Conditional copula is dur|peak - vol|peak)
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
  ){
  vines = lapply(
    all_units,
    function(name) {
      mat = cop_df |> dplyr::filter(unit == name) |> dplyr::select(contains("pobs")) |> as.matrix() 
      
      vine = VineCopula::RVineCopSelect(data = mat, Matrix = assumed_vine_structure, familyset = c(3, 4, 5))
    }
  )
  names(vines) = all_units
  
  return(vines)
}


rcond_vine_draws = function(HQ_prob, vine, n_syn = 1000, debug = FALSE){
  if (debug) browser()
  # Indices for AFTER drawing the samples
  idx_dur = 1
  idx_vol = 2
  p = 1 - HQ_prob # Use 1 - HQ_prob to use in CDF: CDF = P(X <= x), HQ = P(X > x)
  
  bicop_dp = VineCopula::BiCop(family = vine$family[3, 1], par = vine$par[3, 1])
  bicop_pv = VineCopula::BiCop(family = vine$family[3, 2], par = vine$par[3, 2])
  bicop_cond_dv = VineCopula::BiCop(family = vine$family[2, 1], par = vine$par[2, 1]) 
  
  # Draw samples of conditional probabilities
  #   Draw u_1|u_2 and u_3|u_2
  cond_sample = VineCopula::BiCopSim(N = n_syn, obj = bicop_cond_dv)
  # Inverse conditional probabilities
  #   Use BiCopHinv2: Inverse of u_1|u_2 --> obtain u_1
  dur = VineCopula::BiCopHinv2(u1 = cond_sample[, idx_dur], u2 = rep(p, n_syn), bicop_dp) 
  vol = VineCopula::BiCopHinv2(u1 = cond_sample[, idx_vol], u2 = rep(p, n_syn), bicop_pv)
  if (debug) plot(dur, vol)
  return(
    data.frame(hq_prob = rep(HQ_prob, n_syn), pobs_peak = rep(p, n_syn), pobs_dur = dur, pobs_vol = vol)
  )
}

cond_marginal_dens = function(vol_dur_vec, peak, marginal_vol, marginal_dur, marginal_peak, mdl, mdl_type, min = F, factor = 1e1){
  vol = vol_dur_vec["vol"]
  dur = vol_dur_vec["dur"]
  
  uMatrix = cbind(
    pmarginal(dur, marginal_dur),
    pmarginal(vol, marginal_vol),
    pmarginal(peak, marginal_peak)
  )
  colnames(uMatrix) = c("pobs_dur", "pobs_vol", "pobs_peak")
  
  # if ((uMatrix[1, "pobs_peak"] > 0.8) * (min == TRUE)) browser()
  
  if (mdl_type == "nac"){
    copula_density = HAC::dHAC(X = as.matrix(uMatrix), hac = mdl) # No idea why, but this "as.matrix" is still required. Damn annoying package...
  } else if (mdl_type == "vine") {
    pobs_dur = uMatrix[1, "pobs_dur"]
    pobs_peak = uMatrix[1, "pobs_peak"]
    pobs_vol = uMatrix[1, "pobs_vol"]
    dens_dp = VineCopula::BiCopPDF(u1 = pobs_dur, u2 = pobs_peak, family = vine$family[3, 1], par = vine$par[3, 1])
    dens_pv = VineCopula::BiCopPDF(u1 = pobs_peak, u2 = pobs_vol, family = vine$family[3, 2], par = vine$par[3, 2])
    dens_cond_dv = VineCopula::BiCopPDF(u1 = pobs_dur, u2 = pobs_vol, family = vine$family[2, 1], par = vine$par[2, 1]) 
    
    copula_density = dens_dp * dens_pv * dens_cond_dv
    # copula_density = VineCopula::RVinePDF(uMatrix, mdl)
  }
  
  # f(vol, dur | peak) = c(vol, dur, peak) f(vol) f(dur)
  conditional_marginal_density =  copula_density * dmarginal(vol, marginal_vol) * dmarginal(dur, marginal_dur)
  
  if (min) conditional_marginal_density = - factor * conditional_marginal_density
  return(conditional_marginal_density)
}

joint_copula_dens = function(pobs_vol_dur_vec, pobs_peak, mdl, mdl_type){
  pobs_vol = pobs_vol_dur_vec[1]$vol
  pobs_dur = pobs_vol_dur_vec[2]$dur
  
  # browser()
  uMatrix = cbind(pobs_dur, pobs_vol, pobs_peak)
  colnames(uMatrix) = c("pobs_dur", "pobs_vol", "pobs_peak")
  
  if (mdl_type == "nac"){
    return(HAC::dHAC(X = uMatrix, hac = mdl))
  } else if (mdl_type == "vine") {
    # "conditional copula density is just joint itself": p.34: https://www.columbia.edu/~rf2283/Conference/1Fundamentals%20(1)Seagers.pdf
    dens_dp = VineCopula::BiCopPDF(u1 = pobs_dur, u2 = pobs_peak, family = vine$family[3, 1], par = vine$par[3, 1])
    dens_pv = VineCopula::BiCopPDF(u1 = pobs_peak, u2 = pobs_vol, family = vine$family[3, 2], par = vine$par[3, 2])
    dens_cond_dv = VineCopula::BiCopPDF(u1 = pobs_dur, u2 = pobs_vol, family = vine$family[2, 1], par = vine$par[2, 1]) 
    return(dens_dp * dens_pv * dens_cond_dv)
  }
}

get_most_probable_voldur = function(
    hq_prob, 
    mdl, mdl_type,
    gev_vol, gev_dur, gev_peak, 
    grid_size = 10, grid_pobs_min = 0.01, grid_pobs_max = 0.999,
    optimizer = "L-BFGS-B", lower = 1e-5,
    trace = 1
    ){
  # 1) Create a grid to
    # 1a) Plot the contours of the conditional density
    # 1b) Select an initial value for the algorithm (i.e. that grid-point for which the density is max)
  # 2) Run the optimization algorithm using the previously found grid-maximizer as initial value
  # grid_df = get_grid(min = .01, max = .999, size = grid_size) |> as.data.frame() |> dplyr::rename(vol = x, dur = y)
  # browser()
  marginal_grid_df = get_grid(
    min_x = qmarginal(grid_pobs_min, gev_vol), 
    max_x = qmarginal(grid_pobs_max, gev_vol), 
    name_x = "vol",
    min_y = qmarginal(grid_pobs_min, gev_dur),
    max_y = qmarginal(grid_pobs_max, gev_dur),
    name_y = "dur",
    size = grid_size
  )
  marginal_grid_df$z = lapply(
    1:nrow(marginal_grid_df),
    function(i) cond_marginal_dens(
      vol_dur_vec = marginal_grid_df[i, ],
      peak = qmarginal(1 - hq_prob, gev_peak),
      marginal_vol = gev_vol,
      marginal_dur = gev_dur,
      marginal_peak = gev_peak,
      mdl = mdl,
      mdl_type = mdl_type
    )
  ) |> unlist() 
  marginal_grid_df = marginal_grid_df |> dplyr::mutate(z = (z - mean(z)) / sd(z))
  
  copula_grid_df = get_grid(
    min_x = grid_pobs_min, min_y = grid_pobs_min,
    max_x = grid_pobs_max, max_y = grid_pobs_max,
    size = grid_size,
    name_x = "vol", name_y = "dur"
  )
  copula_grid_df$z = lapply(
    1:nrow(copula_grid_df),
    function(i) joint_copula_dens(
      pobs_vol_dur_vec = copula_grid_df[i, ],
      pobs_peak = 1 - hq_prob,
      mdl = mdl,
      mdl_type = mdl_type
    )
  ) |> unlist() 
  copula_grid_df = copula_grid_df |> dplyr::mutate(z = (z - mean(z)) / sd(z))
  
  idx = which.max(copula_grid_df$z)
  initial_vol_pobs = .5
  # initial_vol_pobs = 1 - hq_prob # After some while, the optimization cannot follow and the initial point remains
  # initial_vol_pobs = copula_grid_df[idx, "vol"] # Would work find BUT copual is the JOINT copula, NOT the conditional!!
  initial_dur_pobs = .5
  # initial_dur_pobs = 1 - hq_prob
  # initial_dur_pobs = copula_grid_df[idx, "dur"] # Same reason as before
  initial_vol = qmarginal(initial_vol_pobs, gev_vol)
  initial_dur = qmarginal(initial_dur_pobs, gev_dur)
  
  # Find most probable combination of vol and dur given a peak value
  # by maximizing the conditional density (using numerical approach)
  out = optim(
    par = c(vol = initial_vol, dur = initial_dur), # Initial values
    fn = cond_marginal_dens,
    peak = qmarginal(1 - hq_prob, gev_peak),
    marginal_vol = gev_vol,
    marginal_dur = gev_dur,
    marginal_peak = gev_peak,
    mdl = mdl, 
    mdl_type = mdl_type,
    min = T,
    method = optimizer,
    # hessian = T,
    lower = lower,
    control = list(
      trace = trace,
      maxit = 1e6,
      factr = 1e1
    )
  )
  vol = out$par[1]
  dur = out$par[2]
  
  # Density contours
  marginal_contours = ggplot(marginal_grid_df, aes(x = vol, y = dur, z = z)) +
      geom_contour_filled() +
      geom_point(data = data.frame(vol = initial_vol, dur = initial_dur, z = 0)) +
      geom_point(data = data.frame(vol = vol, dur = dur, z = 0), color = "red") + 
      theme(legend.position = "none")
  
  copula_contours = ggplot(copula_grid_df, aes(x = vol, y = dur, z = z)) +
      geom_contour_filled() +
      geom_point(data = data.frame(vol = initial_vol_pobs, dur = initial_dur_pobs, z = 0)) +
      geom_point(data = data.frame(vol = pmarginal(vol, gev_vol), dur = pmarginal(dur, gev_dur), z = 0), color = "red") 
      # theme(legend.position = "none")
  
  contours = copula_contours | marginal_contours 
  plot(
    contours + plot_annotation(
      title = paste("Density Plots for HQ", 1/hq_prob, " [", hq_prob, "]", sep = ""),
      subtitle = "Joint Copula Density (left); Marginal Density (right)"
    )
  )
  
  return(
    data.frame(vol = vol, dur = dur, hq_prob = hq_prob)
  )
}

get_grid = function(min_x = 0.01, max_x = 0.99, min_y = 0.01, max_y = 0.999, size = 50, name_x = "x", name_y = "y"){
  x = seq(from = min_x, to = max_x, length.out = size)
  y = seq(from = min_y, to = max_y, length.out = size)
  xygrid = expand.grid(x = x, y = y) |> 
    as.data.frame() 
  colnames(xygrid) = c(name_x, name_y)
  
  return(xygrid)
}

get_marginal_hdi = function(
    vol_opt, dur_opt, hq_prob, 
    init_vol_min, init_vol_max,
    init_dur_min, init_dur_max,
    var_matrix,
    mdl, mdl_type,
    gev_vol, gev_dur, gev_peak,
    grid_pobs_min = 0.01, grid_pobs_max = 0.999, grid_size = 30
    ){
  # Get HDI for the most likely points
  # Densities are non-symmetric. Maybe I can use similar approach as in PCA? But that does not consider the conditional nature here, no? 
  # F this, just calc the symmetric one. That is way simpler. Or radius matrix times eigenvectors to kind of distort the shape?
  
  
  eigen_decomp = eigen(var_matrix)
  scale = -1 * sqrt(eigen_decomp$values)
  eigen_v = eigen_decomp$vectors
  eigen_df = data.frame(
        vol = c(vol_opt, vol_opt),
        dur = c(dur_opt, dur_opt),
        eigen_vec_vol = c(eigen_v[1, 1], eigen_v[1, 2]),
        eigen_vec_dur = c(eigen_v[2, 1], eigen_v[2, 2]),
        scale = c(scale[1], scale[2]),
        z = c(0, 0)
      ) |> 
    dplyr::mutate(
      vol_end = vol_opt + scale * eigen_vec_vol,
      dur_end = dur_opt + scale * eigen_vec_dur
    )
  
  # Current Density Plot and Fit of Eigenvector approach
  marginal_grid_df = get_grid(
    min_x = qmarginal(grid_pobs_min, gev_vol), 
    max_x = qmarginal(grid_pobs_max, gev_vol), 
    name_x = "vol",
    min_y = qmarginal(grid_pobs_min, gev_dur),
    max_y = qmarginal(grid_pobs_max, gev_dur),
    name_y = "dur",
    size = grid_size
  )
  marginal_grid_df$z = lapply(
    1:nrow(marginal_grid_df),
    function(i) cond_marginal_dens(
      vol_dur_vec = marginal_grid_df[i, ],
      peak = qmarginal(1 - hq_prob, gev_peak),
      marginal_vol = gev_vol,
      marginal_dur = gev_dur,
      marginal_peak = gev_peak,
      mdl = mdl,
      mdl_type = mdl_type
    )
  ) |> unlist() 
  marginal_grid_df = marginal_grid_df |> dplyr::mutate(z = (z - mean(z)) / sd(z))
  
  ggplot(marginal_grid_df, aes(x = vol, y = dur, z = z)) +
    geom_contour_filled() +
    geom_point(data = data.frame(vol = vol_opt, dur = dur_opt, z = 0), color = "black") + 
    geom_segment(
      data = eigen_df,
      aes(x = vol, xend = vol_end, y = dur, yend = dur_end),
      linewidth = 1
    ) + 
    theme(legend.position = "none") 
  
  
  
  browser()
  min_vol = init_vol_min
  max_vol = init_vol_max
  min_dur = init_dur_min
  max_dur = init_dur_max
  
  iters = 1
  searching = TRUE
  while (searching) {
    if (iters > 1000) searching = FALSE # To ensure I somewhen finish
    
    auc = cubature::adaptIntegrate(
      f = function(x) cond_marginal_dens(
        vol_dur_vec = c(vol = x[[1]], dur = x[[2]]),
        peak = qmarginal(1 - hq_prob, gev_peak),
        marginal_vol = gev_vol,
        marginal_dur = gev_dur,
        marginal_peak = gev_peak,
        mdl = mdl,
        mdl_type = mdl_type
        ),
      lowerLimit = c(min_vol, min_dur),
      upperLimit = c(max_vol, max_dur)
    )$integral
    # if (auc >= 0.95) searching = FALSE
    if (TRUE) searching = FALSE
    
    
    iters = iters + 1
  }
  
  return(data.frame(min_vol = min_vol, max_vol = max_vol, min_dur = min_dur, max_dur = max_dur, auc = auc, iters = iters, scale_factor = scale_factor))
}

grab_taildeps = function(station, vines, cop_df){
  vine = vines[[station]]
  df = cop_df |> dplyr::filter(unit == station)
  river = df$river[1]
  north = df$north[1]
  east = df$east[1]
  
  data.frame(
    variables = c(
      "D - P",
      "P - V",
      "D - V"
    ),
    cop_fam = c(
      get_copname(vine$family[3, 1]),
      get_copname(vine$family[3, 2]),
      get_copname(vine$family[2, 1])
    ),
    upper = c(
      vine$taildep$upper[3, 1],
      vine$taildep$upper[3, 2],
      vine$taildep$upper[2, 1]
    ),
    lower = c(
      vine$taildep$lower[3, 1],
      vine$taildep$lower[3, 2],
      vine$taildep$lower[2, 1]
    )
  ) |> 
    dplyr::mutate(
      river = river,
      unit = station,
      north = north,
      east = east
    )
}


filter_cop_df = function(cop_df, n_minobs){
  "
  This functions removes the stations that have too little information to fit a copula on.
  "
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
  message(
    paste(length(considered_stations$unit), "stations considered")
  )
  message(
    paste(length(removed_stations$unit), "stations removed")
  )
  message("Removed station table:")
  table_details = data.frame(table(removed_stations$n))
  colnames(table_details) = c("n", "count")
  message(
    paste0(
      capture.output(table_details),
      collapse = "\n"
    )
  )
      
  return(cop_df |> dplyr::filter(unit %in% considered_stations$unit))
}



calc_bivariate_mode = function(x, y) {
  df = cbind(x, y)
  
  # Kernel density estimation
  dens = MASS::kde2d(df[,1], df[,2], n = 1000)
  
  ix_max = which(dens$z == max(dens$z), arr.ind = TRUE)
  mode_x = dens$x[ix_max[1]]
  mode_y = dens$y[ix_max[2]]
  
  mode_point = c(x = mode_x, y = mode_y)
  return(mode_point)
}

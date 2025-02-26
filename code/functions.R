library(ggplot2)
library(patchwork)



get_copula_df = function(
    in_dir = "../data/output/rdata/copula_dfs/"
  ){
  "
  Read all copula dfs and join them to one large copula df
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

render_rivertemplate = function(
    df,
    out_dir = "../riverreports/"
  ){
  report_name = paste(out_dir, "Report_", unique(df$river), "_t", unique(df$p_threshold), ".html", sep = "")
  
  rmarkdown::render(
    input = "02_riverreport_template.Rmd",
    output_file = report_name,
    params = list(
      df = df
    )
  )
}


analyse_dependence = function(df){
  
  vol_dur = ggplot(df, aes(x = volume, y = duration_min)) + 
    geom_point()
  
  vol_peak = ggplot(df, aes(x = volume, y = peak)) + 
    geom_point()
  
  dur_peak = ggplot(df, aes(x = duration_min, y = peak)) + 
    geom_point()
  
  combined_plot = (vol_dur | vol_peak | dur_peak) 
  print(combined_plot)
  
  print(paste("Vol-Dur: ", cor(df$volume, df$duration_min, method = "kendall")))
  print(paste("Vol-Peak:", cor(df$volume, df$peak, method = "kendall")))
  print(paste("Dur-Peak:", cor(df$duration_min, df$peak, method = "kendall")))
}
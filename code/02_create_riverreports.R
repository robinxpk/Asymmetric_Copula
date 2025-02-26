# Load data into this file and then use the riverreport template to create a report for every river
source("functions.R")

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

lapply(
  unique(cop_df$river), 
  function(name) render_rivertemplate(
    df = cop_df |> dplyr::filter(river == name),
    out_dir = "../riverreports/"
  )
)


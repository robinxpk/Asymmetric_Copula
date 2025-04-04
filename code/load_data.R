library(ggplot2)
library(doParallel)
source("functions.R")

# Initialize multiprocessing
n_cores = parallel::detectCores() - 2
cluster = parallel::makeCluster(n_cores, outfile = "load_data.log")
doParallel::registerDoParallel(cluster)
parallel::clusterEvalQ(cluster, source("functions.R"))

create_dfs(
    data_path = "../data/0 input data/",
    extended_dfs_path = "../data/output/rdata/extended_dfs/",
    threshold_dfs_path = "../data/output/rdata/threshold_dfs/",
    hydrograph_path = "../data/output/graphs/hydrographs/",
    copula_dfs_path = "../data/output/rdata/copula_dfs/",
    pattern = "*.Rdata",
    evalCompleteness = F,
    hydros = T,
    debug = F
  )

stopCluster(cluster)




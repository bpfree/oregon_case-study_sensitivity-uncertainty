#############################
### 0. Create Directories ###
#############################

## Create directories
data_dir <- dir.create("data")
data_directories <- c("a_raw_data",
                      "b_intermediate_data",
                      "c_submodel_data",
                      "d_suitability_data",
                      "e_rank_data",
                      "f_sensitivity_data",
                      "g_uncertainty_data",
                      "zz_miscellaneous")

for (i in 1:length(data_directories)){
  directories <- dir.create(paste0("data/",data_directories[i]))
}

# Delete directory
unlink("data/a_raw_data", recursive = T)

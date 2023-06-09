#############################
### 0. Create Directories ###
#############################

# Create data directory
data_dir <- dir.create("data")

# Designate subdirectories
data_subdirectories <- c("a_raw_data",
                         "b_intermediate_data",
                         "c_submodel_data",
                         "d_suitability_data",
                         "e_rank_data",
                         "f_sensitivity_data",
                         "g_uncertainty_data",
                         "zz_miscellaneous")

# Create sub-directories within data directory
for (i in 1:length(data_subdirectories)){
  subdirectories <- dir.create(paste0("data/", data_subdirectories[i]))
}

# Delete directory (if necessary)
#unlink("data/a_raw_data", recursive = T)

##############################
### 21. Fisheries Submodel ###
##############################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               fasterize,
               fs,
               ggplot2,
               janitor,
               pdftools,
               plyr,
               raster,
               rgdal,
               rgeos,
               rmapshaper,
               rnaturalearth, # use devtools::install_github("ropenscilabs/rnaturalearth") if packages does not install properly
               sf,
               sp,
               terra, # is replacing the raster package
               tidyr)


#####################################
#####################################

# Set directories
## Input directories
study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
fisheries_submodel <- "data/c_submodel_data/fisheries_submodel.gpkg"

## Output directories
### Oregon suitability geopackage
oregon_suitability_gpkg <- "data/d_suitability_data/suitability_model.gpkg"

### Fisheries directory
suitability_dir <- "data/d_suitability_data"
dir.create(paste0(suitability_dir, "/",
                  "fisheries_suitability"))

oregon_fisheries_dir <- "data/d_suitability_data/fisheries_suitability"
oregon_fisheries_suitability <- "data/d_suitability_data/fisheries_suitability/fisheries_suitability.gpkg"

#####################################

sf::st_layers(dsn = fisheries_submodel,
              do_count = T)

#####################################
#####################################

clean_function <- function(data){
  
  data <- data %>%
    as.data.frame() %>%
    dplyr::select(-geom)
  
  return(data)
}

#####################################
#####################################

# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Fisheries
oregon_fisheries_nmfs_odfw <- sf::st_read(dsn = fisheries_submodel,
                                          layer = "oregon_hex_fisheries")

#####################################
#####################################

# Prepare the fisheries data
oregon_fisheries_values <- oregon_fisheries_nmfs_odfw %>%
  clean_function()

#####################################
#####################################

# Calculate geometric mean for industry and operations submodel
oregon_fisheries <- oregon_hex %>%
  # join the fisheries values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_fisheries_values,
                   by = "index") %>%
  # calculate across rows
  dplyr::rowwise() %>%
  # calculate the geometric mean
  ## geometric mean = nth root of the product of the variable values
  dplyr::mutate(fish_geom_mean = exp(mean(log(c_across(c("fisheries_index"))),
                                          # remove any values that are NA when calculating the mean
                                          na.rm = T))) %>%
  # select the fields of interest
  dplyr::select(index,
                fisheries_index,
                fish_geom_mean) %>%
  dplyr::rename(fisheries_value = fisheries_index)

### Check to see if there are any duplicates of the indices
### There are none
fish_duplicates <- oregon_fisheries %>%
  # create frequency field based on index
  dplyr::add_count(index) %>%
  # see which ones are duplicates
  dplyr::filter(n>1) %>%
  # show distinct options
  dplyr::distinct()

#####################################
#####################################

# Export data
## Suitability
sf::st_write(obj = oregon_fisheries, dsn = oregon_suitability_gpkg, layer = "oregon_fisheries_suitability", append = F)

## Submodel
### Fisheries
saveRDS(obj = oregon_fisheries_values, file = paste(oregon_fisheries_dir, "oregon_fisheries_values.rds", sep = "/"))

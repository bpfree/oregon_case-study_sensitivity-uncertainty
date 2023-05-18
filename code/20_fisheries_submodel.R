##############################
### 20. Fisheries Submodel ###
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
oregon_fisheries_suitability <- "data/c_submodel_data/fisheries_suitability.gpkg"
oregon_suitability_gpkg <- "data/d_suitability_data/suitability_model.gpkg"

#####################################

sf::st_layers(dsn = fisheries_submodel,
              do_count = T)

#####################################
#####################################

# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Fisheries


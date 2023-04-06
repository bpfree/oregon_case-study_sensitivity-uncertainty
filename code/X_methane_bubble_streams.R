#################################
### X. Methane Bubble Streams ###
#################################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               fasterize,
               fs,
               ggplot2,
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
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
coral_sponge_dir <- "data/a_raw_data/deep_sea_coral_sponge_habitat/0276883/1.1/data/0-data/NCCOS_USWestCoast_DSC_Models_2016_2020"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_geopackage <- "data/c_submodel_data/natural_resources.gpkg"

#### Intermediate directories
coral_sponge_habitat_gpkg <- "data/b_intermediate_data/coral_sponge_habitat.gpkg"

raster_dir <- "data/b_intermediate_data"
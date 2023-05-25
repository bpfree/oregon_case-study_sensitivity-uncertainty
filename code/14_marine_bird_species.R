############################################
### 14. Marine bird species density maps ###
############################################

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
marine_bird_dir <- "data/a_raw_data/marine_bird/0242882/1.1/data/0-data/model_output_predictions"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_submodel <- "data/c_submodel_data/natural_resources_submodel.gpkg"

#### Intermediate directories
##### Marine bird species geopackage (vector data)
marine_bird_gpkg <- "data/b_intermediate_data/marine_bird_species.gpkg"

##### Deep sea coral and sponge habitat directory (raster data)
dir.create(paste0(intermediate_dir, "/",
                  "marine_bird_species"))

marine_bird_species_dir <- "data/b_intermediate_data/marine_bird_species"

#####################################
#####################################

list.files(marine_bird_dir)

#####################################
#####################################

# Load data

## Oregon
### Oregon Call Areas
oregon_call_areas <- sf::st_read(dsn = wind_area_gpkg,
                                 layer = paste(sf::st_layers(dsn = wind_area_gpkg,
                                                             do_count = TRUE)))

### Oregon hex areas
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

#####################################
#####################################




### Kelsey et al. (2018) (source: https://www.sciencedirect.com/science/article/abs/pii/S0301479718309228)
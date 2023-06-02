##############################################
### 24. Local Index of Spatial Association ###
##############################################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               elsa,
               fasterize,
               fs,
               ggplot2,
               janitor,
               pdftools,
               plyr,
               raster,
               rgdal,
               rgeoda,
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
suitability_models <- "data/d_suitability_data/suitability_model.gpkg"

## Output directories
### LISA directory
lisa_rank <- "data/e_rank_data/lisa_rank.gpkg"

#####################################
#####################################

# Inspect geopackage layers
sf::st_layers(dsn = suitability_models,
              do_count = F)

#####################################
#####################################

# Load data
## Oregon call area suitability areas
oregon_model_areas <- sf::st_read(dsn = suitability_models, layer = "oregon_model_areas") %>%
  as_Spatial(.)

#####################################
#####################################

# LISA
test <- elsa::lisa(x = oregon_model_areas, d1 = 0, 
                   # search distance went to 8400m
                   d2 = 8400,
                   # statistic is "local Moran's"
                   statistic = "i",
                   # tell which field to use for calculation ("model_geom_mean")
                   zcol = "model_geom_mean")

test2 <- test %>%
  sf::st_as_sf() %>%
  cbind(., oregon_model_areas) %>%
  dplyr::select(index,
                Ii, Z.Ii)

#### ***WARNING: why are there random spots south of north call area??
g <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = oregon_model_areas, fill = NA, linetype = "dashed") +
  ggplot2::geom_sf(data = test2, fill = NA, color = "blue", linetype = "dashed")
g

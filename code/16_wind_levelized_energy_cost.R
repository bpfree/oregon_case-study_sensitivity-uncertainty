####################################
### 16. Levelized cost of energy ###
####################################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docxtractr,
               dplyr,
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
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
lcoe_dir <- "data/a_raw_data/LCOE_2027"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
wind_submodel <- "data/c_submodel_data/wind_submodel.gpkg"

#### Intermediate directories
wind_gpkg <- "data/b_intermediate_data/wind.gpkg"

#####################################
#####################################

# Load data

## Oregon Call Areas
oregon_call_areas <- sf::st_read(dsn = wind_area_gpkg,
                                 layer = paste(sf::st_layers(dsn = wind_area_gpkg,
                                                             do_count = TRUE)))

## Oregon hex areas
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

#####################################

## Levelized cost of energy (2027)
### ***Note: These data are not presently publicly available, yet a presentation of the data
###          are viewable here: https://www.nrel.gov/docs/fy22osti/80908.pdf (Musial et al. 2021).
###          Additional information can be located here: https://www.osti.gov/biblio/1825061
lcoe_2027 <- sf::st_read(dsn = lcoe_dir, "LCOE_2027") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")

#####################################
#####################################

# Energy cost in Oregon call areas
oregon_lcoe_2027 <- lcoe_2027 %>%
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas)

#####################################

# Normalize the levelized cost data
## The analysis seeks to have values get normalized with a linear function
## and place them between 0.8 and 1.0
### Linear function = (value - min) / (max - min)
### Linear function between values = ((value - min) / (max - min)) * (target_max - target_min) + target_min

## Targets
### target minimum
tmin <- 0.8

### target maximum
tmax <- 1.0

## Values
### value minimum
min <- min(oregon_lcoe_2027$lcoe_mid)

### value maximum
max <- max(oregon_lcoe_2027$lcoe_mid)

## Oregon levelized cost of energy normalized
oregon_lcoe_2027_norm <- oregon_lcoe_2027 %>%
  # normalize data between 0.8 and 1
  dplyr::mutate(value1 = ((lcoe_mid - min) /
                            (max - min)) * (tmax - tmin) + tmin)


############################
### 2. Define Study Area ###
############################

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

# Commentary on R and code formulation:
## ***Note: If not familiar with dplyr notation
## dplyr is within the tidyverse and can use %>%
## to "pipe" a process, allowing for fluidity
## Can learn more here: https://style.tidyverse.org/pipes.html

## Another  common coding notation used is "::"
## For instance, you may encounter it as dplyr::filter()
## This means use the filter function from the dplyr package
## Notation is used given sometimes different packages have
## the same function name, so it helps code to tell which
## package to use for that particular function.
## The notation is continued even when a function name is
## unique to a particular package so it is obvious which
## package is used

#####################################
#####################################

# Set directories
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
wind_area_dir <- "data/a_raw_data/BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb"

### Output directories
#### Analysis directories
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directories
study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

#####################################
#####################################

# Load call areas
wind_areas <- sf::st_read(wind_area_dir, "Wind_Planning_Area_Outlines_2_2023") %>%
  # filter for only Oregon call areas
  dplyr::filter(grepl("Oregon", CATEGORY1)) %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")

## verify that units are in meters and no longer degrees
st_crs(wind_areas, parameters = TRUE)$units_gdal

#####################################
#####################################

# Create 10-acre grid around call areas
wind_area_grid <- sf::st_make_grid(x = wind_areas,
                                   ## see documentation on what cellsize means when relating to hexagons: https://github.com/r-spatial/sf/issues/1505
                                   ## cellsize is the distance between two vertices (short diagonal --> d = square root of 3 * length of side)
                                   ### So in this case, square-root of 3 * 123.80533 = 1.73205080757 * 124.80533 = 216.169172615
                                   cellsize = 216.169172615,
                                   # make hexagon (TRUE will generate squares)
                                   square = FALSE,
                                   # make hexagons orientation with a flat topped (FALSE = pointy top)
                                   flat_topped = TRUE) %>%
  # convert back as sf
  sf::st_as_sf()

#####################################
#####################################

# Subset by location: hexagonal grids that intersect with wind areas
wind_area_hex <- wind_area_grid[wind_areas, ] %>%
  # add field "index" that will be populated with the row_number
  dplyr::mutate(index = row_number())

# Oregon call area hexes as single feature
## ***Note: This dataset will be used to extract any data from datasets
##           within the model that will impact wind siting suitability
oregon_call_area_hex_dissolve <- wind_area_hex %>%
  # create field called "call area"
  dplyr::mutate(call_area = "call_area") %>%
  # group all rows by the different elements with "call area" field -- this will create a row for the grouped data
  dplyr::group_by(call_area) %>%
  # summarise all those grouped elements together -- in effect this will create a single feature
  dplyr::summarise()

#####################################
#####################################

# Export data
## Study Area
sf::st_write(wind_area_grid, dsn = study_area_gpkg, layer = "oregon_call_area_grid", append = F)
sf::st_write(wind_area_hex, dsn = study_area_gpkg, layer = "oregon_call_area_hex", append = F)
sf::st_write(oregon_call_area_hex_dissolve, dsn = study_area_gpkg, layer = "oregon_call_areas_dissolve", append = F)

## Wind Call Areas
sf::st_write(wind_areas, dsn = wind_area_gpkg, layer = "oregon_wind_call_areas", append = F)

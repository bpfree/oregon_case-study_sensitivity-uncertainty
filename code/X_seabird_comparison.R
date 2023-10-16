
# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docxtractr,
               dplyr,
               elsa,
               fasterize,
               fs,
               ggplot2,
               janitor,
               ncf,
               paletteer,
               pdftools,
               plyr,
               purrr,
               raster,
               RColorBrewer,
               reshape2,
               rgdal,
               rgeoda,
               rgeos,
               rmapshaper,
               rnaturalearth, # use devtools::install_github("ropenscilabs/rnaturalearth") if packages does not install properly
               sf,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# Set directories
## Input directories
arc_seabird <- "data/zz_miscellaneous/OregonBirds.gdb"
r_seabird <- "data/c_submodel_data/natural_resources_submodel.gpkg"

#####################################

# Inspect available layers and names within natural resources submodel geopackage
sf::st_layers(dsn = r_seabird,
              do_count = T)

sf::st_layers(dsn = arc_seabird,
              do_count = T)

#####################################
#####################################

# Load data
## ArcGIS marine seabird data
arc_marine_bird <- sf::st_read(dsn = arc_seabird, layer = "Marine_birds_grid") %>%
  # add field "index" that will be populated with the row_number
  dplyr::mutate(index = row_number()) %>%
  as.data.frame() %>%
  dplyr::select(index,
                Marine_birds)

## R marine seabirdbird
r_marine_bird <- sf::st_read(dsn = r_seabird, layer = "oregon_hex_marine_bird")

#####################################
#####################################

# Join datasets together
marine_seabird <- r_marine_bird %>%
  dplyr::inner_join(x = .,
                    y = arc_marine_bird,
                    by = "index") %>%
  dplyr::mutate(diff = Marine_birds - marine_bird_index,
                diff_pct = ((Marine_birds - marine_bird_index) / Marine_birds) * 100) %>%
  dplyr::select(index,
                Marine_birds,
                marine_bird_index,
                diff,
                diff_pct)

marine_seabird_0.5_diff <- marine_seabird %>%
  dplyr::filter(diff_pct >= 0.5 | diff_pct <= -0.5)

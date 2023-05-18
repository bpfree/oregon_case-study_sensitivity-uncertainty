################################
### 17. Constraints Submodel ###
################################

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
constraints_submodel <- "data/c_submodel_data/constraints.gpkg"
study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"

## Output directories
oregon_constraints_gpkg <- "data/c_submodel_data/oregon_constraints.gpkg"
suitability_gpkg <- "data/d_suitability_data/suitability_model.gpkg"

#####################################

sf::st_layers(dsn = constraints_submodel,
              do_count = T)

#####################################
#####################################

# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Constraints
### DoD OPNAV
oregon_hex_dod_opnav <- sf::st_read(dsn = constraints_submodel, layer = "oregon_hex_dod_opnav") %>%
  dplyr::mutate(dod_value = 0) %>%
  as.data.frame()

### PACPARS
oregon_hex_pacpars <- sf::st_read(dsn = constraints_submodel, layer = "oregon_hex_pacpars") %>%
  dplyr::mutate(pacpars_value = 0) %>%
  as.data.frame()

#####################################
#####################################

oregon_constraints <- oregon_hex %>%
  dplyr::left_join(x = .,
                   y = oregon_hex_dod_opnav,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = oregon_hex_pacpars,
                   by = "index") %>%
  dplyr::select(index,
                dod_value,
                pacpars_value)


#####################################
#####################################

# Export data
## Suitability
sf::st_write(obj = oregon_constraints, dsn = suitability_gpkg, layer = "oregon_constraints", append = F)

## Constraints
sf::st_write(obj = oregon_hex_dod_opnav, dsn = oregon_constraints_gpkg, layer = "oregon_hex_dod_opnav", append = F)
sf::st_write(obj = oregon_hex_pacpars, dsn = oregon_constraints_gpkg, layer = "oregon_hex_pacpars", append = F)

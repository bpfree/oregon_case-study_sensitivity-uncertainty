###########################################
### 18. Industry and Operations Submodel ###
###########################################

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
industry_operations_submodel <- "data/c_submodel_data/industry_operations_submodel.gpkg"

## Output directories
oregon_constraints_gpkg <- "data/c_submodel_data/oregon_industry_operations.gpkg"
suitability_gpkg <- "data/d_suitability_data/suitability_model.gpkg"

#####################################

sf::st_layers(dsn = industry_operations_submodel,
              do_count = T)

#####################################
#####################################

# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Industry and Operations
### Submarine cables
oregon_hex_submarine_cable500 <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_submarine_cable_500m") %>%
  dplyr::mutate(submarine_500 = 0.6) %>%
  as.data.frame()

oregon_hex_submarine_cable1000 <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_submarine_cable_1000m") %>%
  dplyr::mutate(submarine_1000 = 0.8) %>%
  as.data.frame()

### Scientific surveys
oregon_hex_eastwest_survey_corridors <- sf::st_read(dsn = industry_operations_submodel,
                                                    layer = "oregon_hex_eastwest_survey_corridors") %>%
  dplyr::mutate(eastwest_survey = 0.01) %>%
  as.data.frame()

oregon_hex_additional_eastwest_survey_corridors <- sf::st_read(dsn = industry_operations_submodel,
                                                               layer = "oregon_hex_additional_eastwest_survey_corridors") %>%
  dplyr::mutate(add_eastwest_survey = 0.5) %>%
  as.data.frame()

oregon_hex_survey_stations <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_survey_stations") %>%
  dplyr::mutate(survey_stations = 0.5) %>%
  as.data.frame()

oregon_hex_survey_transects <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_survey_transects") %>%
  dplyr::mutate(survey_transects = 0.5) %>%
  as.data.frame()


#####################################
#####################################


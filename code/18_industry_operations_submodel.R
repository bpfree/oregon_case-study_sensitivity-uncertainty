############################################
### 18. Industry and Operations Submodel ###
############################################

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
industry_operations_submodel <- "data/c_submodel_data/industry_operations_submodel.gpkg"

## Output directories
oregon_industry_operations_suitability <- "data/c_submodel_data/industry_operations_suitability.gpkg"
oregon_suitability_gpkg <- "data/d_suitability_data/suitability_model.gpkg"

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
oregon_hex_submarine_cable500 <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_submarine_cable500") %>%
  dplyr::mutate(sc500_value = 0.6) %>%
  as.data.frame()

oregon_hex_submarine_cable1000 <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_submarine_cable1000") %>%
  dplyr::mutate(sc1000_value = 0.8) %>%
  as.data.frame()

### Scientific surveys
oregon_hex_eastwest_survey_corridors <- sf::st_read(dsn = industry_operations_submodel,
                                                    layer = "oregon_hex_eastwest_survey_corridors") %>%
  dplyr::mutate(eastwest_value = 0.01) %>%
  as.data.frame()

oregon_hex_additional_eastwest_survey_corridors <- sf::st_read(dsn = industry_operations_submodel,
                                                               layer = "oregon_hex_additional_eastwest_survey_corridors") %>%
  dplyr::mutate(eastwest_add_value = 0.5) %>%
  as.data.frame()

oregon_hex_survey_stations <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_survey_stations_2nm") %>%
  dplyr::mutate(sstat_value = 0.5) %>%
  as.data.frame()

oregon_hex_survey_transects <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_survey_transects_1nm") %>%
  dplyr::mutate(stransect_value = 0.5) %>%
  as.data.frame()


#####################################
#####################################

oregon_industry_operations <- oregon_hex %>%
  # join the submarine cable (0 - 500m) values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_hex_submarine_cable500,
                   by = "index") %>%
  # join the submarine cable (501 - 1000m) values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_hex_submarine_cable1000,
                   by = "index") %>%
  # join the east-west survey corridors values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_hex_eastwest_survey_corridors,
                   by = "index") %>%
  # join the additional east-west survey corridors values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_hex_additional_eastwest_survey_corridors,
                   by = "index") %>%
  # join the survey station values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_hex_survey_stations,
                   by = "index") %>%
  # join the survey transects values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_hex_survey_transects,
                   by = "index") %>%
  # select fields of interest
  dplyr::select(index,
                sc500_value,
                sc1000_value,
                eastwest_value,
                eastwest_add_value,
                sstat_value,
                stransect_value) %>%
  # calculate across rows
  dplyr::rowwise() %>%
  # calculate the geometric mean
  ## geometric mean = nth root of the product of the variable values
  dplyr::mutate(io_geom_mean = exp(mean(log(c_across(c("sc500_value",
                                                       "sc1000_value",
                                                       "eastwest_value",
                                                       "eastwest_add_value",
                                                       "sstat_value",
                                                       "stransect_value"))),
                                # remove any values that are NA when calculating the mean
                                na.rm = T))) %>%
  # select the fields of interest
  dplyr::select(sc500_value,
                sc1000_value,
                eastwest_value,
                eastwest_add_value,
                sstat_value,
                stransect_value,
                io_geom_mean) %>%
  # rename the geometry field
  dplyr::rename(geom = geom.x)

#####################################
#####################################

# Export data
## Suitability
sf::st_write(obj = oregon_industry_operations, dsn = oregon_suitability_gpkg, layer = "oregon_industry_operations_suitability", append = F)
  
## Submodel
sf::st_write(obj = oregon_hex_submarine_cable500, dsn = oregon_industry_operations_suitability, layer = "oregon_hex_submarine_cable_500m", append = F)
sf::st_write(obj = oregon_hex_submarine_cable1000, dsn = oregon_industry_operations_suitability, layer = "oregon_hex_submarine_cable_1000m", append = F)

sf::st_write(obj = oregon_hex_eastwest_survey_corridors, dsn = oregon_industry_operations_suitability, layer = "oregon_hex_eastwest_survey_corridors", append = F)
sf::st_write(obj = oregon_hex_additional_eastwest_survey_corridors, dsn = oregon_industry_operations_suitability, layer = "oregon_hex_additional_eastwest_survey_corridors", append = F)
sf::st_write(obj = oregon_hex_survey_stations, dsn = oregon_industry_operations_suitability, layer = "oregon_hex_survey_stations_2nm", append = F)
sf::st_write(obj = oregon_hex_survey_transects, dsn = oregon_industry_operations_suitability, layer = "oregon_hex_submarine_cable_1nm", append = F)

sf::st_write(obj = oregon_industry_operations, dsn = oregon_industry_operations_suitability, layer = "oregon_industry_operations_suitability", append = F)

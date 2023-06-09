############################################
### 23. Overall Oregon Suitability Model ###
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
suitability_models <- "data/d_suitability_data/suitability_model.gpkg"

## Output directories
### Suitability directory
suitability_dir <- "data/d_suitability_data"
dir.create(paste0(suitability_dir, "/",
                  "overall_suitability"))

oregon_overall_suitability_dir <- "data/d_suitability_data/overall_suitability"
oregon_overall_suitability <- "data/d_suitability_data/overall_suitability/overall_suitability.gpkg"

#####################################

sf::st_layers(dsn = suitability_submodels,
              do_count = T)

#####################################
#####################################

clean_function <- function(data){
  
  data <- data %>%
    as.data.frame() %>%
    dplyr::select(-geom)
  
  return(data)
}

#####################################
#####################################

# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Suitability submodels
### Oregon constraints
oregon_constraints <- sf::st_read(dsn = suitability_models,
                                  layer = "oregon_constraints")

### Oregon industry and operations
oregon_industry_operations_submodel <- sf::st_read(dsn = suitability_models,
                                                      layer = "oregon_industry_operations_suitability")

### Oregon natural resources
oregon_natural_resources_submodel <- sf::st_read(dsn = suitability_models,
                                                 layer = "oregon_natural_resources_suitability")

### Oregon fisheries
oregon_fisheries_submodel <- sf::st_read(dsn = suitability_models,
                                         layer = "oregon_fisheries_suitability")

### Oregon wind
oregon_wind_submodel <- sf::st_read(dsn = suitability_models,
                                    layer = "oregon_wind_suitability")

#####################################
#####################################

# Prepare submodel data
## Oregon constraints
oregon_constraints_values <- oregon_constraints %>%
  clean_function()

## Oregon industry and operations
oregon_industry_operations_values <- oregon_industry_operations_submodel %>%
  clean_function()

## Oregon natural resources
oregon_natural_resources_values <- oregon_natural_resources_submodel %>%
  clean_function()

## Oregon fisheries
oregon_fisheries_values <- oregon_fisheries_submodel %>%
  clean_function()

## Oregon wind
oregon_wind_values <- oregon_wind_submodel %>%
  clean_function()

#####################################
#####################################

oregon_model <- oregon_hex %>%
  # join the constraints areas by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_constraints_values,
                   by = "index") %>%
  # join the industry and operations values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_industry_operations_values,
                   by = "index") %>%
  # join the natural resources values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_natural_resources_values,
                   by = "index") %>%
  # join the fisheries values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_fisheries_values,
                   by = "index") %>%
  # join the wind values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = oregon_wind_values,
                   by = "index")

oregon_model_areas <- oregon_model %>%
  # remove any areas that are constraints -- thus get areas that are NA
  dplyr::filter(is.na(constraints)) %>%
  # calculate across rows
  dplyr::rowwise() %>%
  # calculate the geometric mean
  ## geometric mean = nth root of the product of the variable values
  dplyr::mutate(model_geom_mean = exp(mean(log(c_across(c("io_geom_mean",
                                                          "nr_geom_mean",
                                                          "fish_geom_mean",
                                                          "wind_geom_mean"))),
                                          # remove any values that are NA when calculating the mean
                                          na.rm = T))) %>%
  dplyr::select(index,
                # constraints
                dod_value, pacpars_value,
                # industry and operations
                sc500_value, sc1000_value, eastwest_value, eastwest_add_value, sstat_value, stransect_value,
                # natural resources
                leatherback_value, killerwhale_value, humpback_ca_value, humpback_mx_value, bluewhale_value, species_product,
                efhca_value, rreef_map_value, rreef_prob_value, deep_coralsponge_value, continental_shelf_value,
                methane_bubble_value, habitat_value,
                # fisheries
                fisheries_value,
                # wind
                wind_value,
                # submodel geometric values
                constraints, io_geom_mean, nr_geom_mean, fish_geom_mean, wind_geom_mean,
                # model geometric value
                model_geom_mean)

#####################################
#####################################

# Export data
## Overall suitability
sf::st_write(obj = oregon_model_areas, dsn = suitability_models, layer = "oregon_model_areas", append = F)

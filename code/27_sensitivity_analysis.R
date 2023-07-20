################################
### 27. Sensitivity Analysis ###
################################

# Clear environment
rm(list = ls())

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
               stringr,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# Set directories
## Input directories
study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
industry_operations_submodel <- "data/c_submodel_data/industry_operations_submodel.gpkg"
suitability_models <- "data/d_suitability_data/suitability_model.gpkg"

## Output directories
### Suitability directory
sensitivity_dir <- "data/f_sensitivity_data"
dir.create(paste0(suitability_dir, "/",
                  "test"))

oregon_sensitivity_dir <- "data/f_sensitivity_data/test"
oregon_sensitivity_jackknife <- "data/f_sensitivity_data/test/jackknife_sensitivity.gpkg"

#####################################

sf::st_layers(dsn = suitability_models,
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

# Set parameters
## designate region name
region <- "oregon"

## designate date
date <- format(Sys.time(), "%Y%m%d")

#####################################
#####################################

# load data
# prepare data
# combine data
# remove dataset --> loop across columns
# recalculate geometric mean --> rowwise
# run calculations
# iterate for each dataset

#####################################
#####################################

region <- "oregon"
date <- format(Sys.time(), "%Y%m%d")
layer <-
values <- seq(from = 0, to = 1, by = 0.1)
submarine_cable500 <- 0.6
submarine_cable1000 <- 0.8
eastwest_survey_corridors <- 0.01
eastwest_add <- 0.5
sstat <- 0.5
stransect <- 0.5


###########################################

# TEST AREA
# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Industry and Operations
oregon_hex_submarine_cable500 <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_submarine_cable500")
oregon_hex_submarine_cable1000 <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_submarine_cable1000")
oregon_hex_eastwest_survey_corridors <- sf::st_read(dsn = industry_operations_submodel,
                                                    layer = "oregon_hex_eastwest_survey_corridors")
oregon_hex_additional_eastwest_survey_corridors <- sf::st_read(dsn = industry_operations_submodel,
                                                               layer = "oregon_hex_additional_eastwest_survey_corridors")
oregon_hex_survey_stations <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_survey_stations_2nm")
oregon_hex_survey_transects <- sf::st_read(dsn = industry_operations_submodel, layer = "oregon_hex_survey_transects_1nm")

datasets <- c(oregon_hex_submarine_cable500,
              oregon_hex_submarine_cable1000,
              oregon_hex_eastwest_survey_corridors,
              oregon_hex_additional_eastwest_survey_corridors,
              oregon_hex_survey_stations,
              oregon_hex_survey_transects)

n_files = length(datasets)

#####################################
#####################################

test <- function(datasets, values){
  for (dataset in 1:length(datasets())){
    
  }
    
}


# Calculate geometric mean for industry and operations submodel
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
  dplyr::select(index,
                sc500_value,
                sc1000_value,
                eastwest_value,
                eastwest_add_value,
                sstat_value,
                stransect_value,
                io_geom_mean)















# loop
for (value in values){
  test <- oregon_industry_operations %>%
    dplyr::mutate(sc500_value = replace(sc500_value, !is.na(sc500_value), value))
}




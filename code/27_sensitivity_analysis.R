################################
### 27. Sensitivity Analysis ###
################################

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
suitability_models <- "data/d_suitability_data/suitability_model.gpkg"

## Output directories
### Sensitivity directory

#####################################

# Inspect available layers and names within suitability model geopackage
sf::st_layers(dsn = suitability_models,
              do_count = T)

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
data <- sf::st_read(dsn = suitability_models, layer = "oregon_model_areas")

sensitivity_jackknife <- data %>%
  dplyr::select(index,
                # constraints
                dod_value, pacpars_value,
                # industry and operations
                sc500_value, sc1000_value, eastwest_value, eastwest_add_value, sstat_value, stransect_value,
                # natural resources
                ## species product
                leatherback_value, killerwhale_value, humpback_ca_value, humpback_mx_value, bluewhale_value,
                non_protected_value, species_product,
                ## minimum habitat
                efhca_value, rreef_map_value, rreef_prob_value, deep_coralsponge_value, continental_shelf_value,
                methane_bubble_value, habitat_value,
                ## marine seabird
                marine_bird_value,
                # fisheries
                fisheries_value,
                # wind
                wind_value,
                # submodel geometric values
                constraints, io_geom_mean, nr_geom_mean, fish_geom_mean, wind_geom_mean,
                # model geometric value
                model_geom_mean)

# 4 - 26
for (i in 4:5){
  start2 <- Sys.time()
  
  #i <- 4
  
  name <- names(data)[i]
  
  sensitivity_jacknife <- sensitivity_jackknife %>%
    
    # when field is elected (column i) fill with NA values so as to "remove" it from analysis
    dplyr::mutate(across(.cols = i,
                         ~replace(i, !is.na(i), NA))) %>%
    
    # recalculate the geometric means for each submodel (geometric mean = nth root of the product of the variable values)
    ## calculate across rows
    dplyr::rowwise() %>%
    ## industry and operations
    dplyr::mutate(!!paste0("io_geom_mean_", name) := exp(mean(log(c_across(c("sc500_value",
                                                                             "sc1000_value",
                                                                             "eastwest_value",
                                                                             "eastwest_add_value",
                                                                             "sstat_value",
                                                                             "stransect_value"))),
                                                              # remove any values that are NA when calculating the mean
                                                              na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    ## natural resources
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the product of all protected species values
    dplyr:::mutate(species_product = prod(leatherback_value,
                                          killerwhale_value,
                                          humpback_ca_value,
                                          humpback_mx_value,
                                          bluewhale_value,
                                          non_protected_value,
                                          # remove NA values for product 
                                          na.rm = T)) %>%
    ### calculate minimum value across the habitat subdatasets
    dplyr::rowwise() %>%
    dplyr::mutate(habitat_value = pmin(efhca_value,
                                       rreef_map_value,
                                       rreef_prob_value,
                                       deep_coralsponge_value,
                                       continental_shelf_value,
                                       methane_bubble_value,
                                       na.rm = T)) %>%
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("nr_geom_mean_", name) := exp(mean(log(c_across(c("species_product",
                                                                             "habitat_value",
                                                                             "marine_bird_value"))),
                                                              # remove any values that are NA when calculating the mean
                                                              na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    ## fisheries
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("fish_geom_mean_", name) := exp(mean(log(c_across(c("fisheries_value"))),
                                                                # remove any values that are NA when calculating the mean
                                                                na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    ## wind
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("wind_geom_mean_", name) := exp(mean(log(c_across(c("wind_value"))),
                                                                # remove any values that are NA when calculating the mean
                                                                na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    # recalculate the geometric means for each final model (geometric mean = nth root of the product of the variable values)
    ## final model
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("model_geom_mean_", name) := exp(mean(log(c_across(c("io_geom_mean",
                                                                                "nr_geom_mean",
                                                                                "fish_geom_mean",
                                                                                "wind_geom_mean"))),
                                                                 # remove any values that are NA when calculating the mean
                                                                 na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    dplyr::mutate_all(~ifelse(is.na(.), NA, .))
  
  print(paste(Sys.time() - start2, "minutes to complete creating and adding", name, "data to dataframe", sep = " ")) # print how long it takes to calculate
}

# assign(paste("sensitivity_jackknife_removed", fields[i], sep = "_"), test)

# Export data
## Jackknife

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

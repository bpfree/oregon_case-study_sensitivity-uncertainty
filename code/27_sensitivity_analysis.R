#############################################
### 27. Sensitivity Analysis -- Jackknife ###
#############################################

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
sensitivity_gpkg <- "data/f_sensitivity_data/oregon_sensitivity.gpkg"
sensitivity_dir <- "data/f_sensitivity_data"

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

# Function to calculate product of non-NA values
calculate_geometric_mean <- function(row) {
  non_na_values <- row[!is.na(row)]
  if (length(non_na_values) > 0) {
    prod_values <- prod(non_na_values)
    geometric_mean <- prod_values^(1/length(non_na_values))
    return(geometric_mean)
  } else {
    return(NA)
  }
}

# Function to calculate product of non-NA values
calculate_product <- function(...) {
  values <- c(...)
  non_na_values <- values[!is.na(values)]
  if (length(non_na_values) > 0) {
    return(prod(non_na_values))
  } else {
    return(NA)
  }
}

#####################################
#####################################

# load data
## Oregon suitability data
oregon_suitability <- sf::st_read(dsn = suitability_models, layer = "oregon_model_areas") %>%
  dplyr::select(index,
                # constraints
                dod_value, pacpars_value,
                # industry and operations
                sc500_value, sc1000_value, sub_cable_value, eastwest_value, eastwest_add_value, sstat_value, stransect_value, sci_survey_value,
                # natural resources
                ## species product
                leatherback_value, killerwhale_value, humpback_ca_value, humpback_mx_value, bluewhale_value, species_product_value,
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

#####################################
#####################################

# Create a reference dataframe where data will get added
sensitivity_jackknife <- oregon_suitability

#####################################
#####################################

# Jackknife analysis across all datasets to determine how suitability scores change when dataset is removed
## Datasets cross fields 4 - 27
for (i in 4:27){
  start2 <- Sys.time()
  
  # if wanting to test a particular dataset
  #i <- 6
  
  name <- names(oregon_suitability)[i]
  
  sensitivity_iteration <- oregon_suitability %>%
    as.data.frame() %>%
    
    # when field is elected (column i) fill with NA values so as to "remove" it from analysis
    dplyr::mutate(across(.cols = i,
                         .fns = ~replace(i, !is.na(i), NA))) %>%
    
    # industry and operations
    ### fill sub_cable_values with the sum of 500-m buffer and 500-1000-m buffer values
    ### when a hex has both values give it the lower value (500-m buffer = 0.6)
    #### when iterating, if the column is the sub_cable_value, the field gets NA
    #### otherwise, follow the minimum value rules
    dplyr::mutate(sub_cable_value = rowSums((.[,4:5]),
                                            na.rm = T)) %>%
    dplyr::mutate(sub_cable_value = ifelse(i == 6, NA,
                                           case_when(sub_cable_value == 2 ~ 1,
                                              sub_cable_value == 1.8 ~ 0.8,
                                              sub_cable_value == 1.6 ~ 0.6,
                                              sub_cable_value == 1.4 ~ 0.6,
                                              sub_cable_value == 1.0 ~ 1.0,
                                              sub_cable_value == 0.8 ~ 0.8,
                                              sub_cable_value == 0.6 ~ 0.6))) %>%
    
    
    ## calculate a summary value for scientific surveys
    ### when a hex grid cell has multiple values the minimum
    ### value across the datasets is assigned to the new
    ### summarized field
    dplyr::mutate(sci_survey_value = ifelse(i == 11, NA,
                                            pmin(eastwest_value,
                                                 eastwest_add_value,
                                                 sstat_value,
                                                 stransect_value,
                                                 # remove any values that are NA when new field
                                                 na.rm = T))) %>%
                                          
    ## calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste("io_geom_mean", name, sep ="_") := apply(X = .[c("sub_cable_value",
                                                                           "sci_survey_value")],
                                                                   # 1 = rows, 2 = columns
                                                                   MARGIN = 1,
                                                                   # use the calculate geometric mean function
                                                                   FUN = calculate_geometric_mean)) %>%
    
    #####################################
  
  # natural resources
  ## calculate the product of all protected species values
  dplyr::mutate(species_product_value = ifelse(i == 17, NA,
                                               # function
                                               mapply(FUN = calculate_product,
                                               # fields to apply function to
                                               leatherback_value,
                                               killerwhale_value,
                                               humpback_ca_value,
                                               humpback_mx_value,
                                               bluewhale_value))) %>%
    
    ## calculate minimum value across the habitat subdatasets
    dplyr::mutate(habitat_value = ifelse(i == 24, NA,
                                         pmin(efhca_value,
                                              rreef_map_value,
                                              rreef_prob_value,
                                              deep_coralsponge_value,
                                              continental_shelf_value,
                                              methane_bubble_value,
                                              # remove NA values from the minimum calculation
                                              na.rm = T))) %>%
    
    ## calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste("nr_geom_mean", name, sep= "_") := apply(X = .[c("species_product_value",
                                                                           "habitat_value",
                                                                           "marine_bird_value")],
                                                                   # 1 = rows, 2 = columns
                                                                   MARGIN = 1,
                                                                   # use the calculate geometric mean function
                                                                   FUN = calculate_geometric_mean)) %>%
    
    #####################################
  
  # fisheries
  ## calculate the geometric mean (geometric mean = nth root of the product of the variable values)
  dplyr::mutate(!!paste("fish_geom_mean", name, sep = "_") := apply(X = .[c("fisheries_value")],
                                                                    # 1 = rows, 2 = columns
                                                                    MARGIN = 1,
                                                                    # use the calculate geometric mean function
                                                                    FUN = calculate_geometric_mean)) %>%
    
  #####################################
  
  # wind
  ## calculate the geometric mean (geometric mean = nth root of the product of the variable values)
  dplyr::mutate(!!paste("wind_geom_mean", name, sep = "_") := apply(X = .[c("wind_value")],
                                                                    # 1 = rows, 2 = columns
                                                                    MARGIN = 1,
                                                                    # use the calculate geometric mean function
                                                                    FUN = calculate_geometric_mean)) %>%
    
  #####################################
  
  # final model score
  ## calculate the geometric mean (geometric mean = nth root of the product of the variable values)
  dplyr::mutate(!!paste("model_geom_mean", name, sep = "_") := apply(X = .[c(paste("io_geom_mean", name, sep = "_"),
                                                                                   paste("nr_geom_mean", name, sep = "_"),
                                                                                   paste("fish_geom_mean", name, sep = "_"),
                                                                                   paste("wind_geom_mean", name, sep = "_"))],
                                                                           # 1 = rows, 2 = columns
                                                                           MARGIN = 1,
                                                                           # use the calculate geometric mean function
                                                                           FUN = calculate_geometric_mean)) %>%
    
    # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
    as.data.frame() %>%
    
    # select the fields created by the iteration
    dplyr::select(paste("io_geom_mean", name, sep = "_"),
                  paste("nr_geom_mean", name, sep = "_"),
                  paste("fish_geom_mean", name, sep = "_"),
                  paste("wind_geom_mean", name, sep = "_"),
                  paste("model_geom_mean", name, sep = "_"))
  
  # add the results from the iteration to the sensitivity jackknife dataframe
  sensitivity_jackknife <- cbind(sensitivity_jackknife, sensitivity_iteration)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name, "data to dataframe", sep = " "))
}

#####################################
#####################################

# Export data
## Jackknife
sf::st_write(obj = sensitivity_jackknife, dsn = sensitivity_gpkg, layer = paste(region, "sensitivity", sep = "_"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

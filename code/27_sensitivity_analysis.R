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
  #i <- 14
  
  name <- names(oregon_suitability)[i]
  
  sensitivity_iteration <- oregon_suitability %>%
    
    # when field is elected (column i) fill with NA values so as to "remove" it from analysis
    dplyr::mutate(across(.cols = i,
                         .fns = ~replace(i, !is.na(i), NA))) %>%
    
    # create combined submarine cable field
    ## fill with values of 500-m buffer, 500-1000-m buffer
    ## when a hex has both values give it the lower value (500-m buffer = 0.6)
    dplyr::mutate(sub_cable_value = sc500_value + sc1000_value) %>%
    dplyr::mutate(sub_cable_value = case_when(sub_cable_value == 2 ~ 1,
                                              sub_cable_value == 1.8 ~ 0.8,
                                              sub_cable_value == 1.6 ~ 0.6,
                                              sub_cable_value == 1.4 ~ 0.6,
                                              sub_cable_value == 0.8 ~ 0.8,
                                              sub_cable_value == 0.6 ~ 0.6)) %>%
    
    # calculate a summary value for scientific surveys
    ## when a hex grid cell has multiple values the minimum
    ## value across the datasets is assigned to the new
    ## summarized field
    dplyr::mutate(sci_survey_value = pmin(eastwest_value,
                                          eastwest_add_value,
                                          sstat_value,
                                          stransect_value,
                                          # remove any values that are NA when new field
                                          na.rm = T)) %>%
    
    # recalculate the geometric means for each submodel (geometric mean = nth root of the product of the variable values)
    ## calculate across rows
    dplyr::rowwise() %>%
    # calculate the geometric mean
    ## industry and operations
    ### ***Note: !! will have will create the new field name before evaluating the statement
    dplyr::mutate(!!paste0("io_geom_mean_", name) := exp(mean(log(c_across(c("sub_cable_value",
                                                                             "sci_survey_value"))),
                                                              # remove any values that are NA when calculating the mean
                                                              na.rm = T))) %>%
    
    ### natural resources
    #### calculate the product of all protected species values
    dplyr::mutate(species_product_value = prod(leatherback_value,
                                               killerwhale_value,
                                               humpback_ca_value,
                                               humpback_mx_value,
                                               bluewhale_value,
                                               # remove NA values from the minimum calculation
                                               na.rm = T)) %>%
    #### calculate minimum value across the habitat subdatasets
    dplyr::mutate(habitat_value = pmin(efhca_value,
                                       rreef_map_value,
                                       rreef_prob_value,
                                       deep_coralsponge_value,
                                       continental_shelf_value,
                                       methane_bubble_value,
                                       # remove NA values from the minimum calculation
                                       na.rm = T)) %>%
    
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("nr_geom_mean_", name) := exp(mean(log(c_across(c("species_product_value",
                                                                             "habitat_value",
                                                                             "marine_bird_value"))),
                                                              # remove any values that are NA when calculating the mean
                                                              na.rm = T))) %>%
    
    ## fisheries
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("fish_geom_mean_", name) := exp(mean(log(c_across(c("fisheries_value"))),
                                                                # remove any values that are NA when calculating the mean
                                                                na.rm = T))) %>%
    
    ## wind
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("wind_geom_mean_", name) := exp(mean(log(c_across(c("wind_value"))),
                                                                # remove any values that are NA when calculating the mean
                                                                na.rm = T))) %>%
    
    # recalculate the geometric means for each final model (geometric mean = nth root of the product of the variable values)
    ## final model
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("model_geom_mean_", name) := exp(mean(log(c_across(c(paste0("io_geom_mean_", name),
                                                                                paste0("nr_geom_mean_", name),
                                                                                paste0("fish_geom_mean_", name),
                                                                                paste0("wind_geom_mean_", name)))),
                                                                 # remove any values that are NA when calculating the mean
                                                                 na.rm = T))) %>%
    
    # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
    as.data.frame() %>%
  
    # select the fields created by the iteration
    dplyr::select(paste0("io_geom_mean_", name),
                  paste0("nr_geom_mean_", name),
                  paste0("fish_geom_mean_", name),
                  paste0("wind_geom_mean_", name),
                  paste0("model_geom_mean_", name))
  
  # add the results from the iteration to the sensitivity jackknife dataframe
  sensitivity_jackknife <- cbind(sensitivity_jackknife, sensitivity_iteration)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name, "data to dataframe", sep = " "))
}

#####################################
#####################################

sensitivity_jackknife <- sf::st_read(dsn = sensitivity_gpkg, layer = paste(region, "sensitivity", sep = "_"))

sensitivity_nominal_percent_change <- sensitivity_jackknife %>%
  dplyr::select(index,
                model_geom_mean)

# Calculate nominal and percent changes between model iterations
for (i in 1:24){
  start2 <- Sys.time()
  
  # if wanting to test a particular dataset
  #i <- 8
  
  original_model_score <- 33
  
  p <- original_model_score + i*5
  
  name_change <- names(sensitivity_jackknife)[p] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = "(?<=mean_).*?(?=_value)")
  
  change_iteration <- sensitivity_jackknife %>%
    dplyr::mutate(!!paste("model_geom_mean", name_change, "nominal_change", sep = "_") := .[[p]] - .[[original_model_score]],
                  .after = paste0("model_geom_mean_", name_change, "_value")) %>%
    dplyr::mutate(!!paste("model_geom_mean", name_change, "percent_change", sep = "_") := round(((.[[p]] - .[[original_model_score]]) / .[[p]]) * 100, 2) ,
                  .after = paste("model_geom_mean", name_change, "nominal_change", sep = "_")) %>%
    
    
    # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
    as.data.frame() %>%
    
    # select the fields created by the iteration
    dplyr::select(paste("model_geom_mean", name_change, "nominal_change", sep = "_"),
                  paste("model_geom_mean", name_change, "percent_change", sep = "_"))
  
  # add the results from the iteration to the sensitivity nominal and percent change dataframe
  sensitivity_nominal_percent_change <- cbind(sensitivity_nominal_percent_change, change_iteration)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name_change, "data to dataframe", sep = " "))
}

#####################################

table <- data.frame(dataset = character(),
                    n_min = numeric(),
                    n_max = numeric(),
                    p_min = numeric(),
                    p_max = numeric())

# Calculate minimum and maximum changes for each dataset on model geometric mean
for (i in 0:23){
  start2 <- Sys.time()
  
  # if wanting to test a particular dataset
  #i <- 4
  
  nominal_data <- 3 + i*2
  percent_data <- 4 + i*2
  
  name_change <- names(sensitivity_nominal_percent_change)[nominal_data] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = "(?<=mean_).*?(?=_nominal_change)")
  
  dataset_list <- c(name_change)
  
  n_min <- min(sensitivity_nominal_percent_change[[nominal_data]])
  n_max <- max(sensitivity_nominal_percent_change[[nominal_data]])

  p_min <- min(sensitivity_nominal_percent_change[[percent_data]])
  p_max <- max(sensitivity_nominal_percent_change[[percent_data]])
  
  result_table <- data.frame(
    dataset_list,
    n_min,
    n_max,
    p_min,
    p_max
  )
  
  table <- rbind(table, result_table)
  
  hist(sensitivity_nominal_percent_change[[nominal_data]])
  hist(sensitivity_nominal_percent_change[[percent_data]])
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name_change, "data to dataframe", sep = " "))
}

table <- table %>%
  dplyr::mutate(n_range = n_max - n_min,
                p_range = p_max - p_min) %>%
  dplyr::mutate(dataset_list = recode(dataset_list,
                                      "sc500" = "Submarine cable (500m)",
                                      "sc1000" = "Submarine cable (1000m)",
                                      "sub_cable" = "Submarine cable",
                                      "eastwest" = "East-west surveys",
                                      "eastwest_add" = "East-west surveys (additional)",
                                      "sstat" = "Survey station",
                                      "stransect" = "Survey transect",
                                      "sci_survey" = "Scientific survey (combined)",
                                      "leatherback" = "Leatherback",
                                      "killerwhale" = "Killer whale",
                                      "humpback_ca" = "Humpback (CA)",
                                      "humpback_mx" = "Humpback (MX)",
                                      "bluewhale" = "Blue whale",
                                      "species_product" = "Species product (combined)",
                                      "efhca" = "EFHCA",
                                      "rreef_map" = "Rocky reef (mapped)",
                                      "rreef_prob" = "Rocky reef (probable)",
                                      "deep_coralsponge" = "Deep sea coral-sponge",
                                      "continental_shelf" = "Contiental shelf (10km)",
                                      "methane_bubble" = "Methan bubble streams",
                                      "habitat" = "Habitat (combined)",
                                      "marine_bird" = "Marine seabird",
                                      "fisheries" = "Fisheries",
                                      "wind" = "Wind"))

n <- ggplot(table, aes(x = reorder(dataset_list, n_range), y = n_range)) +
  #geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = n_min,
                    ymax = n_max), width = 0.5, color = "darkblue") +
  coord_flip() +
  labs(x = "Dataset", y = "Nominal Range") +
  theme_minimal()

print(n)


p <- ggplot(table, aes(x = reorder(dataset_list, p_range), y = p_range)) +
  #geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = p_min,
                    ymax = p_max), width = 0.5, color = "darkblue") +
  coord_flip() +
  labs(x = "Dataset", y = "Percent Range") +
  theme_minimal()

print(p)

#####################################
#####################################

# Export data
## Jackknife
sf::st_write(obj = sensitivity_jackknife, dsn = sensitivity_gpkg, layer = paste(region, "sensitivity", sep = "_"), append = F)

## Nominal / percent change
sf::st_write(obj = sensitivity_nominal_percent_change, dsn = sensitivity_gpkg, layer = paste(region, "sensitivity_nominal_percent_change", sep = "_"), append = F)

## Table
base::saveRDS(object = table, file = paste(sensitivity_dir, paste(region, "sensitivity_rable.rds", sep = "_"), sep = "/"))

# Export figures


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

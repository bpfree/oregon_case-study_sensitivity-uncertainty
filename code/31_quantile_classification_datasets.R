#######################################################################
### 31. Sensitivity Analysis -- Quantile Classifications (datasets) ###
#######################################################################

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
                # industry and operations
                sub_cable_value, sci_survey_value,
                # natural resources
                ## species product
                species_product_value,
                ## minimum habitat
                habitat_value,
                ## marine seabird
                marine_bird_value,
                # fisheries
                fisheries_value,
                # wind
                wind_value)

#####################################
#####################################

# Prepare clean quantitative classification dataframe for the datasets
oregon_dataset_quant_class <- oregon_suitability %>%
  dplyr::select(index)

# Calculate quantile classifications for each model iteration
for (i in 2:(length(oregon_suitability) - 1)){
  start2 <- Sys.time()
  
  # if need to test iterations
  #i <- 2
  
  # get the name of the dataset
  name_change <- names(oregon_suitability)[i] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = ".*?(?=_value)")
  
  # calculate quantile
  quantile_calculation <- quantile(x = oregon_suitability[[i]],
                                   # probabilities between (0 - 1)
                                   ## these will be 0%, 12.5%, 25%, 37.5%, 50%, 67.5%, 75%, 87.5%, and 100%
                                   probs = c(0.0, 0.125, 0.25,
                                             0.375, 0.5,
                                             0.675, 0.75,
                                             0.875, 1.0))
  
  # reclassify quantiles
  quant_class_iteration <- oregon_suitability %>%
    # create new score classification and dictate which quantile a hex grid gets classified
    dplyr::mutate(!!paste(name_change, "quantile_classification", sep = "_") := dplyr::case_when(.[[i]] <= quantile_calculation[[1]] ~ 0,
                                                                                                 # 12.5% percentile
                                                                                                 .[[i]] > quantile_calculation[[1]] &
                                                                                                   .[[i]] <= quantile_calculation[[2]] ~ 1,
                                                                                                 # 25% percentile
                                                                                                 .[[i]] > quantile_calculation[[2]] & 
                                                                                                   .[[i]] <= quantile_calculation[[3]] ~ 2,
                                                                                                 # 37.5% percentile
                                                                                                 .[[i]] > quantile_calculation[[3]] & 
                                                                                                   .[[i]] <= quantile_calculation[[4]] ~ 3,
                                                                                                 # 50% percentile
                                                                                                 .[[i]] > quantile_calculation[[4]] & 
                                                                                                   .[[i]] <= quantile_calculation[[5]] ~ 4,
                                                                                                 # 67.5% percentile
                                                                                                 .[[i]] > quantile_calculation[[5]] & 
                                                                                                   .[[i]] <= quantile_calculation[[6]] ~ 5,
                                                                                                 # 75% percentile
                                                                                                 .[[i]] > quantile_calculation[[6]] & 
                                                                                                   .[[i]] <= quantile_calculation[[7]] ~ 6,
                                                                                                 # 87.5% percentile
                                                                                                 .[[i]] > quantile_calculation[[7]] & 
                                                                                                   .[[i]] <= quantile_calculation[[8]] ~ 7,
                                                                                                 # 100% percentile
                                                                                                 .[[i]] > quantile_calculation[[8]] & 
                                                                                                   .[[i]] <= quantile_calculation[[9]] ~ 8),
                  # move field after the dataset model geometric mean field
                  .before = geom) %>%
    
    # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
    as.data.frame() %>%
    
    # select the fields created by the iteration
    dplyr::select(!!paste(name_change, "quantile_classification", sep = "_"))
  
  # add the results from the iteration to the template quantile classification dataframe
  oregon_dataset_quant_class <- cbind(oregon_dataset_quant_class, quant_class_iteration)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name_change, "data to dataframe", sep = " "))
}

#####################################
#####################################

xx<-c(1:(length(oregon_dataset_quant_class)-2))
yy<-combn(xx,2)
yy<-transform(yy)
yy<-t(yy) %>%
  as.data.frame.matrix()

# Calculate quantile classifications change for each model iteration
oregon_dataset_quant_class_change <- oregon_dataset_quant_class %>%
  dplyr::select(index)

for (i in 2:(length(oregon_dataset_quant_class)-1)){
  start2 <- Sys.time()
  
  #i <- 2
  
  dataset1_name <- names(oregon_dataset_quant_class)[i] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = ".*?(?=_quantile)")
  
  for (p in (i+1):(length(oregon_dataset_quant_class)-1)){
    
    #p <- i + 1
    
    dataset2_name <- names(oregon_dataset_quant_class)[p] %>%
      # extract name of leftout dataset
      ## ?<= will look back and exclude the match (mean_)
      ## .*? will look for match until next pattern
      ## ?= will look ahead and exclude the match (_value)
      stringr::str_extract(string = ., pattern = ".*?(?=_quantile)")
    
    quant_change_iteration <- oregon_dataset_quant_class %>%
      dplyr::mutate(!!paste(dataset1_name, dataset2_name, "quant_score_change", sep = "_") := (oregon_dataset_quant_class[[p]] - oregon_dataset_quant_class[[i]])) %>%
      
      # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
      as.data.frame() %>%
      
      # select the fields created by the iteration
      dplyr::select(!!paste(dataset1_name, dataset2_name, "quant_score_change", sep = "_"))
    
    # add the results from the iteration to the template quantile classification change dataframe
    oregon_dataset_quant_class_change <- cbind(oregon_dataset_quant_class_change, quant_change_iteration)
    
    # stop when it gets to the final two datasets (no need to compare the final dataset against itself)
    if (i == (length(oregon_dataset_quant_class)-2) & p == (length(oregon_dataset_quant_class)-1)) break
    
    # print how long it takes to calculate
    print(paste("Iteration", p, "takes", Sys.time() - start2, "minutes to complete creating and adding changes between", dataset1_name, "and", dataset2_name, "data to dataframe", sep = " "))
  }
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding changes between for all of", dataset1_name, "data to dataframe", sep = " "))
}



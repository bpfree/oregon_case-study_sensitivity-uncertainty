################################################
### 28. Sensitivity Analysis -- Change Table ###
################################################

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

### Figures
figure_dir <- "figure"

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
sensitivity_jackknife <- sf::st_read(dsn = sensitivity_gpkg, layer = paste(region, "sensitivity", sep = "_"))

sensitivity_nominal_percent_change <- sensitivity_jackknife %>%
  dplyr::select(index,
                model_geom_mean)

#####################################
#####################################

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
  
  hist_nominal <- hist(sensitivity_nominal_percent_change[[nominal_data]])
  hist_percent <- hist(sensitivity_nominal_percent_change[[percent_data]])
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name_change, "data to dataframe", sep = " "))
}

#####################################
#####################################

table <- table %>%
  # calculate the ranges for nominal changes and percent changes
  dplyr::mutate(n_range = n_max - n_min,
                p_range = p_max - p_min) %>%
  # recode the layer names
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

#####################################
#####################################

# Tornado figures
## Nominal change figure
n <- ggplot(table, aes(x = reorder(dataset_list, n_range), y = n_range)) +
  #geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = n_min,
                    ymax = n_max), width = 0.5, color = "darkblue") +
  coord_flip() +
  labs(x = "Dataset", y = "Change (Nominal)") +
  theme_minimal()

print(n)

## Percent change figure
p <- ggplot(table, aes(x = reorder(dataset_list, p_range), y = p_range)) +
  #geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = p_min,
                    ymax = p_max), width = 0.5, color = "darkblue") +
  coord_flip() +
  labs(x = "Dataset", y = "Change (Percent)") +
  theme_minimal()

print(p)

#####################################
#####################################

# Export data
## Nominal / percent change
sf::st_write(obj = sensitivity_nominal_percent_change, dsn = sensitivity_gpkg, layer = paste(region, "sensitivity_nominal_percent_change", sep = "_"), append = F)

## Table
base::saveRDS(object = table, file = paste(sensitivity_dir, paste(region, "sensitivity_table.rds", sep = "_"), sep = "/"))

## Figures
ggsave(n, filename=file.path(figure_dir, paste(region, "nominal_change_tornado_plot.tiff", sep = "_")), width=6.5,
       height=4.5, units="in", dpi=600, compression = "lzw")
ggsave(p, filename=file.path(figure_dir, paste(region, "percent_change_tornado_plot.tiff", sep = "_")), width=6.5,
       height=4.5, units="in", dpi=600, compression = "lzw")

#####################################
#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

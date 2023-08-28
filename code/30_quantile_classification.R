###############################################################################
### 30. Sensitivity Analysis -- Quantile Classifications (model iterations) ###
###############################################################################

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
               purrr,
               raster,
               RColorBrewer,
               reshape2,
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

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.title.x = element_text(size = 10),
                    axis.text.x = element_text(size = 8),
                    axis.title.y = element_text(size = 10),
                    axis.text.y = element_text(size = 8),
                    plot.subtitle = element_text(size = 8),
                    plot.caption = element_text(size = 6,
                                                lineheight = 0.5),
                    legend.title = element_text(size = 8),
                    legend.text = element_text(size = 5))

color_palette <- c(colorRampPalette(rev(brewer.pal(9, "BuPu")))(14),
                   # 0 values
                   "grey",
                   # positive values
                   colorRampPalette(brewer.pal(9, "YlGn"))(10))

color_palette <- c(rev(paletteer_c("ggthemes::Purple", 14)),
                   # 0 values
                   "grey",
                   # positive values
                   colorRampPalette(brewer.pal(9, "YlGn"))(10))

us_boundary <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  dplyr::filter(name %in% c("California",
                            "Oregon"))

#####################################
#####################################

# load data
sensitivity_jackknife <- sf::st_read(dsn = sensitivity_gpkg, layer = paste(region, "sensitivity", sep = "_"))

#####################################
#####################################

# Calculate sample quantiles

## Template sensitivity quantile classification
sensitivity_quant_class <- sensitivity_jackknife %>%
  # select fields of interest
  dplyr::select(index,
                model_geom_mean)

### Calculate the quantile probabilities for 8 classifications
model_quant_class <- quantile(x = sensitivity_quant_class[["model_geom_mean"]],
                              # probabilities between (0 - 1)
                              ## these will be 0%, 12.5%, 25%, 37.5%, 50%, 67.5%, 75%, 87.5%, and 100%
                              probs = c(0.0, 0.125, 0.25,
                                        0.375, 0.5,
                                        0.675, 0.75,
                                        0.875, 1.0))

### Reclassification of probabilities too classifications (1 - 8)
sensitivity_quant_class <- sensitivity_quant_class %>%
  dplyr::mutate(model_score_classification = dplyr::case_when(.[[2]] <= model_quant_class[[1]] ~ 0,
                                                              .[[2]] > model_quant_class[[1]] & .[[2]] <= model_quant_class[[2]] ~ 1,
                                                              .[[2]] > model_quant_class[[2]] & .[[2]] <= model_quant_class[[3]] ~ 2,
                                                              .[[2]] > model_quant_class[[3]] & .[[2]] <= model_quant_class[[4]] ~ 3,
                                                              .[[2]] > model_quant_class[[4]] & .[[2]] <= model_quant_class[[5]] ~ 4,
                                                              .[[2]] > model_quant_class[[5]] & .[[2]] <= model_quant_class[[6]] ~ 5,
                                                              .[[2]] > model_quant_class[[6]] & .[[2]] <= model_quant_class[[7]] ~ 6,
                                                              .[[2]] > model_quant_class[[7]] & .[[2]] <= model_quant_class[[8]] ~ 7,
                                                              .[[2]] > model_quant_class[[8]] & .[[2]] <= model_quant_class[[9]] ~ 8),
                # move classification field after the model geometric mean field
                .after = model_geom_mean) %>%
  # remove the model geometric mean field
  dplyr::select(-model_geom_mean)

#####################################
#####################################

# Calculate quantile classifications for each model iteration
for (i in 1:24){
  start2 <- Sys.time()
  
  # if wanting to test a particular dataset
  #i <- 1
  
  # iterate over fields (p = parameter) of final geometric mean scores
  p <- 33 + i*5
  
  # get the name of the dataset
  name_change <- names(sensitivity_jackknife)[p] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = "(?<=mean_).*?(?=_value)")
  
  # calculate quantile
  quantile_calculation <- quantile(x = sensitivity_jackknife[[p]],
                                   # probabilities between (0 - 1)
                                   ## these will be 0%, 12.5%, 25%, 37.5%, 50%, 67.5%, 75%, 87.5%, and 100%
                                   probs = c(0.0, 0.125, 0.25,
                                             0.375, 0.5,
                                             0.675, 0.75,
                                             0.875, 1.0))
  
  # reclassify quantiles
  quant_class_iteration <- sensitivity_jackknife %>%
    # create new score classification and dictate which quantile a hex grid gets classified
    dplyr::mutate(!!paste("model_geom_mean", name_change, "score_classification", sep = "_") := dplyr::case_when(.[[p]] <= quantile_calculation[[1]] ~ 0,
                                                                                                                 # 12.5% percentile
                                                                                                                 .[[p]] > quantile_calculation[[1]] &
                                                                                                                   .[[p]] <= quantile_calculation[[2]] ~ 1,
                                                                                                                 # 25% percentile
                                                                                                                 .[[p]] > quantile_calculation[[2]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[3]] ~ 2,
                                                                                                                 # 37.5% percentile
                                                                                                                 .[[p]] > quantile_calculation[[3]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[4]] ~ 3,
                                                                                                                 # 50% percentile
                                                                                                                 .[[p]] > quantile_calculation[[4]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[5]] ~ 4,
                                                                                                                 # 67.5% percentile
                                                                                                                 .[[p]] > quantile_calculation[[5]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[6]] ~ 5,
                                                                                                                 # 75% percentile
                                                                                                                 .[[p]] > quantile_calculation[[6]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[7]] ~ 6,
                                                                                                                 # 87.5% percentile
                                                                                                                 .[[p]] > quantile_calculation[[7]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[8]] ~ 7,
                                                                                                                 # 100% percentile
                                                                                                                 .[[p]] > quantile_calculation[[8]] & 
                                                                                                                   .[[p]] <= quantile_calculation[[9]] ~ 8),
                  # move field after the dataset model geometric mean field
                  .after = paste0("model_geom_mean_", name_change, "_value")) %>%
    
    # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
    as.data.frame() %>%
    
    # select the fields created by the iteration
    dplyr::select(!!paste("model_geom_mean", name_change, "score_classification", sep = "_"))
  
  # add the results from the iteration to the template quantile classification dataframe
  sensitivity_quant_class <- cbind(sensitivity_quant_class, quant_class_iteration)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name_change, "data to dataframe", sep = " "))
}

#####################################
#####################################

# Calculate quantile classifications change for each model iteration
sensitivity_quant_change <- sensitivity_quant_class %>%
  dplyr::select(index)

for (i in 3:26){
  start2 <- Sys.time()
  
  #i <- 26
  
  name_change <- names(sensitivity_quant_class)[i] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = "(?<=mean_).*?(?=_score)")
  
  quant_change_iteration <- sensitivity_quant_class %>%
    dplyr::mutate(!!paste("model_geom_mean", name_change, "quant_score_change", sep = "_") := (sensitivity_quant_class[[i]] - sensitivity_quant_class[[2]])) %>%
    
    # convert to dataframe so geometry is dropped and not duplicated when binded to the reference dataframe
    as.data.frame() %>%
    
    # select the fields created by the iteration
    dplyr::select(!!paste("model_geom_mean", name_change, "quant_score_change", sep = "_"))
    
  # add the results from the iteration to the template quantile classification change dataframe
  sensitivity_quant_change <- cbind(sensitivity_quant_change, quant_change_iteration)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "takes", Sys.time() - start2, "minutes to complete creating and adding", name_change, "data to dataframe", sep = " "))
}

#####################################

# Create total change across all datasets
sensitivity_quant_total_change <- as.data.frame(sensitivity_quant_change) %>%
  dplyr::mutate(total_change = base::rowSums(x = .[2:25])) %>%
  dplyr::select(index,
                total_change,
                geom) %>%
  sf::st_as_sf(x = .)

## Create histogram of total change
tc_hist <- ggplot() +
  geom_histogram(data = sensitivity_quant_total_change, aes(x = total_change),
                 fill = "#869BC4", color = "#869BC4", alpha = 0.8, bins = 25) +
  labs(x = "Quantile change",
       y = "Occurances",
       title = paste("Total quantile change in", str_to_title(region), sep = " "), 
       subtitle = "Aggregated quantile classification change by hex grid") +
  theme_bw() +
  plot_theme

tc_hist

## Create map of total change
tc_plot <- ggplot() +
  geom_sf(data = sensitivity_quant_total_change, mapping = aes(fill = factor(total_change)), lwd = 0) +
  geom_sf(data = us_boundary, fill = "grey90") +
  scale_fill_manual(values = color_palette,
                    name = "Total change") +
  labs(x = "",
       y = "",
       title = paste("Total quantile change in", str_to_title(region), sep = " "),
       subtitle = "Aggregated quantile classification change by hex grid") +
  coord_sf(xlim = c(xmin = st_bbox(sensitivity_quant_total_change)$xmin,
                    xmax = st_bbox(sensitivity_quant_total_change)$xmax+50000),
           ylim = c(ymin = st_bbox(sensitivity_quant_total_change)$ymin,
                    ymax = st_bbox(sensitivity_quant_total_change)$ymax)) +
  geom_sf_text(data = us_boundary[2,], mapping = aes(label = name)) +
  theme_bw() +
  guides(fill = guide_legend(ncol = 2)) +
  plot_theme

print(tc_plot)

#####################################
#####################################

# minimum quantitative classification for each index
sensitivity_quant_minmax <- sensitivity_quant_class %>%
  dplyr::mutate(quant_min = purrr::pmap(.l = across(starts_with("model")),
                                        .f = min),
                quant_max = purrr::pmap(.l = across(starts_with("model")),
                                        .f = max)) %>%
  dplyr::select(index,
                quant_min,
                quant_max)

sensitivity_quant_minmax <- as.data.frame(lapply(sensitivity_quant_minmax, unlist)) %>%
  dplyr::group_by(index) %>%
  dplyr::reframe(quant_min = min(quant_min),
                 quant_max = max(quant_max)) %>%
  dplyr::left_join(x = .,
                   y = sensitivity_quant_total_change,
                   by = "index") %>%
  st_as_sf( x = .)


min_plot <- ggplot() +
  geom_sf(data = sensitivity_quant_minmax, mapping = aes(fill = factor(quant_min)), lwd = 0) +
  geom_sf(data = us_boundary, fill = "grey90") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "PuRd"))(10),
                    name = "Minimum quantile classification") +
  labs(x = "",
       y = "",
       title = paste("Minimum quantile classification in", str_to_title(region), sep = " "),
       subtitle = "Minimum quantile classification change by hex grid") +
  coord_sf(xlim = c(xmin = st_bbox(sensitivity_quant_minmax)$xmin,
                    xmax = st_bbox(sensitivity_quant_minmax)$xmax+50000),
           ylim = c(ymin = st_bbox(sensitivity_quant_minmax)$ymin,
                    ymax = st_bbox(sensitivity_quant_minmax)$ymax)) +
  geom_sf_text(data = us_boundary[2,], mapping = aes(label = name)) +
  theme_bw() +
  guides(fill = guide_legend(ncol = 2)) +
  plot_theme

print(min_plot)

#####################################
#####################################

list <- list()

# Contingency matrix
for (i in 3:length(sensitivity_quant_class) - 1){
  #i <- 5
  
  original_data <- as.data.frame(sensitivity_quant_class[, c(2,i)]) %>%
    dplyr::select(-geom)
  
  layer_name <- names(original_data)[2] %>%
    # extract name of leftout dataset
    ## ?<= will look back and exclude the match (mean_)
    ## .*? will look for match until next pattern
    ## ?= will look ahead and exclude the match (_value)
    stringr::str_extract(string = ., pattern = "(?<=mean_).*?(?=_score)")
  
  matrix_name <- paste("contingency_matrix", layer_name, sep = "_")
  
  contigency_matrix <- assign(matrix_name, as.data.frame(unclass(table(original_data))))
  
  list[[length(list) + 1]] <- contigency_matrix
}

contingency_matrix_bluewhale2 <- contingency_matrix_bluewhale %>%
  dplyr::mutate(class = row_number() - 1) %>%
  dplyr::relocate(class, .before = 0)

melt <- contingency_matrix_bluewhale2 %>%
  reshape2::melt(.,
                 id.var = "class")

matrix_plot <- ggplot(melt,
                      mapping = aes(x=variable,
                                    y=class,
                                    fill=factor(value))) +
  geom_tile() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "BuGn"))(19)) +
  geom_text(aes(label = value)) +
  scale_y_reverse(breaks = c(0:8)) +
  labs(x = paste("Reclassified quantiles"),
       y = "Original quantiles",
       title = paste("Classification reclassifications in", str_to_title(region), sep = " "),
       subtitle = "Aggregated quantile reclassifications") +
  theme_bw() +
  plot_theme +
  theme(legend.position = "none")

print(matrix_plot)

#####################################

clean_list <- list[2:25]

add_diag <- function(x,t){
  sum(x[row(x)+col(x)==t])
}

quant_sum <- as.data.frame(matrix(nrow = length(clean_list),
                                  ncol = 5)) %>%
  dplyr::rename(layer_name = V1,
                no_change = V2,
                increase = V3,
                decrease = V4,
                total = V5)

data_names_list <- list("Submarine cable (500m)",
                        "Submarine cable (1000m)",
                        "Submarine cable (combined)",
                        "East-west surveys",
                        "East-west surveys (additional)",
                        "Survey station",
                        "Survey transect",
                        "Scientific survey (combined)",
                        "Leatherback sea turtle",
                        "Killer whale",
                        "Humpback whale (CA)",
                        "Humpback whale (MX)",
                        "Blue whale",
                        "Species product (combined)",
                        "EFHCA",
                        "Rocky reef (mapped)",
                        "Rocky reef (probable)",
                        "Deep sea coral-sponge",
                        "Contiental shelf (10km)",
                        "Methane bubble streams",
                        "Habitat (combined)",
                        "Marine seabird",
                        "Fisheries",
                        "Wind")

# Quantile change table
for (i in 1:length(clean_list)){
  #i <- 2
  
  data <- clean_list[[i]]
  
  quant_sum[i,1] <- data_names_list[[i]]
  
  quant_sum[i,2] <- add_diag(x = data, t = seq(2,18,2))
  
  quant_sum[i,3] <- sum(sapply(2:9, function(j) sum(data[c(1:j-1),j])))
  
  quant_sum[i,4] <- sum(sapply(2:9, function(j) sum(data[j,c(1:j-1)])))
  
  quant_sum[i,5] <- sum(quant_sum[i, 2:4])
}

#####################################



results <- lapply(1:length(clean_list), function(i) {
  add_diag <- function(x,t){
    sum(x[row(x)+col(x)==t])
  }
  
  data <- clean_list[[i]]
  
  quant_sum <- data.frame(
    layer_name = data_names_list[[i]],
    no_change = add_diag(x = data, t = seq(2, 18, 2)),
    increase = sum(sapply(2:9, function(j) sum(data[c(1:j - 1), j]))),
    decrease = sum(sapply(2:9, function(j) sum(data[j, c(1:j - 1)]))),
    total = 0)
  
  quant_sum$total <- sum(quant_sum$no_change,
                         quant_sum$increase,
                         quant_sum$decrease)
  quant_sum
})

final_result <- bind_rows(results)
final_result









for (i in 1:length(clean_list)){
  quant_sum2 <- as.data.frame(matrix(nrow = length(clean_list),
                                     ncol = 5)) %>%
    dplyr::rename(layer_name = V1,
                  no_change = V2,
                  increase = V3,
                  decrease = V4,
                  total = V5) %>%
    dplyr::mutate(layer_name = data_names_list) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(no_change = add_diag(x = clean_list[[i]], t = seq(2, 18, 2)))
}



















for (i in 1:length(clean_list)){
  #i <- 2
  
  data <- clean_list[[i]]
  
  quant_sum <- quant_sum %>%
    dplyr::rowwise() %>%
    dplyr::mutate(no_change = add_diag(x = data, t = seq(2,18,2)))
}

quant_sum <- clean_list %>%
  purrr::map_dfr(~ tibble(layer_name = data_names_list[[.y]],
                   no_change = add_diag(.x, seq(2, 18, 2)),
                   increase = sum(sapply(2:9, function(j) sum(.x[c(1:(j - 1)), j]))),
                   decrease = sum(sapply(2:9, function(j) sum(.x[j, c(1:(j - 1))]))),
                   total = no_change + increase + decrease),
          .id = "row_id")

# Rename the columns
quant_sum <- quant_sum %>%
  rename(row_id = row_id,
         layer_name = layer_name,
         no_change = no_change,
         increase = increase,
         decrease = decrease,
         total = total)

#####################################
#####################################

# Export data
## Tables

## Figures


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
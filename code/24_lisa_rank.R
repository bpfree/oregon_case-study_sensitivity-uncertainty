##############################################
### 24. Local Index of Spatial Association ###
##############################################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
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
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# Set directories
## Input directories
suitability_models <- "data/d_suitability_data/suitability_model.gpkg"

## Output directories
### LISA directory
lisa_rank <- "data/e_rank_data/lisa_rank.gpkg"

#####################################
#####################################

# Inspect geopackage layers
sf::st_layers(dsn = suitability_models,
              do_count = F)

#####################################
#####################################

# Load data
## Oregon call area suitability areas
oregon_model_areas_sf <- sf::st_read(dsn = suitability_models, layer = "oregon_model_areas")

oregon_model_areas <- sf::st_read(dsn = suitability_models, layer = "oregon_model_areas") %>%
  as_Spatial(.)

#####################################
#####################################

# ELSA
## LISA ()
oregon_lisa <- elsa::lisa(x = oregon_model_areas,
                          # minimum distance
                          d1 = 0,
                          # search distance went to 8400m
                          d2 = 8400,
                          # statistic is "local Moran's"
                          statistic = "i",
                          # tell which field to use for calculation ("model_geom_mean")
                          zcol = "model_geom_mean")

oregon_hex_lisa <- oregon_lisa %>%
  sf::st_as_sf() %>%
  cbind(., oregon_model_areas) %>%
  dplyr::select(index,
                # Ii = Moran's I statistic, Z.Ii = z-score of Moran's I
                Ii, Z.Ii)


#####################################

# rgeoda
## Create distance weights (8400 meters)
dist_thres <- rgeoda::min_distthreshold(oregon_model_areas_sf)
dist_thres # every hex will have at least a single neighbor at a distance of 216.1691 meters

weights_8400 <- rgeoda::distance_weights(sf_obj = oregon_model_areas_sf,
                                         # set distance weight to be 8400 meters
                                         dist_thres = 8400,
                                         # set false to calculate distance in a Euclidean space
                                         is_arc = F,
                                         # set false to keep in metric system (units are in miles)
                                         is_mile = F)

## Create the LISA product
start = Sys.time()
lisa <- rgeoda::local_moran(w = weights_8400, # weight is equal to distance weight of 8400 meters
                            # analyze using the final model geometric mean values
                            df = oregon_model_areas_sf["model_geom_mean"],
                            # run 9999 times to generate pseudo-p-values
                            permutations = 9999,
                            # set cutoff to be 0.05 significance
                            significance_cutoff = 0.05,
                            # set seed so future runs can compare
                            seed = 561974)
print(Sys.time() - start)

## Gather important fields
oregon_lisa_pvalues <- rgeoda_lisa1$p_vals %>%
  as.data.frame() %>%
  dplyr::rename("p_vals" = ".")

oregon_lisa_cvalues <- rgeoda_lisa1$c_vals %>%
  as.data.frame() %>%
  dplyr::rename("c_vals" = ".")

oregon_lisa_lisa_values <- rgeoda_lisa1$lisa_vals %>%
  as.data.frame() %>%
  dplyr::rename("lisa_vals" = ".")

oregon_lisa_neighbors <- rgeoda_lisa1$nn_vals %>%
  as.data.frame() %>%
  dplyr::rename("num_neigh" = ".")

oregon_lisa_labels <- rgeoda_lisa1$labels %>%
  as.data.frame() %>%
  dplyr::rename("labels" = ".") %>%
  # Predefined values
  ## 0 Not significant
  ## 1 High-High
  ## 2 Low-Low
  ## 3 High-Low
  ## 4 Low-High
  ## 5 Undefined
  ## 6 Isolated
  dplyr::mutate(c_vals = c(0, 1, 2, 3, 4, 5, 6))

oregon_lisa_colors <- rgeoda_lisa1$colors %>%
  as.data.frame() %>%
  dplyr::rename("colors" = ".") %>%
  dplyr::mutate(c_vals = c(0, 1, 2, 3, 4, 5, 6))

oregon_lisa_cvalue_labels <- oregon_lisa_cvalues %>%
  dplyr::left_join(x = .,
                   y = oregon_lisa_labels,
                   by = "c_vals")

oregon_lisa <- oregon_lisa_pvalues %>%
  cbind(oregon_lisa_cvalue_labels,
        oregon_lisa_lisa_values,
        oregon_lisa_neighbors,)

## calculate important fields

rgeoda_lisa_pvalues <- rgeoda::lisa_pvalues(gda_lisa = rgeoda_lisa1) %>%
  as.data.frame() %>%
  dplyr::rename("pvals" = ".")

rgeoda_lisa_cluster <- rgeoda::lisa_clusters(gda_lisa = rgeoda_lisa1)

rgeoda_lisa_colors <- rgeoda::lisa_colors(gda_lisa = rgeoda_lisa1)

rgeoda_lisa_neighbors <- rgeoda::lisa_num_nbrs(gda_lisa = rgeoda_lisa1) %>%
  as.data.frame() %>%
  dplyr::rename("n_neigh" = ".")

#####################################
#####################################
#####################################
#####################################
#####################################

oregon_model_areas <- oregon_model_areas_sf %>%
  cbind(rgeoda::lisa_pvalues(gda_lisa = rgeoda_lisa1),
        rgeoda::lisa_clusters(gda_lisa = rgeoda_lisa1),
        rgeoda::lisa_colors(gda_lisa = rgeoda_lisa1),
        rgeoda::lisa_num_nbrs(gda_lisa = rgeoda_lisa1))



#### ***WARNING: why are there random spots south of north call area??
g <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = oregon_model_areas, fill = NA, color = "green", linetype = "dashed") +
  ggplot2::geom_sf(data = oregon_hex_lisa, aes(fill = NA, color = Ii), linetype = "dashed")
g

#####################################
#####################################

# Export data
## LISA
sf::st_write(obj = oregon_hex_lisa, dsn = suitability_models, layer = "oregon_hex_lisa")

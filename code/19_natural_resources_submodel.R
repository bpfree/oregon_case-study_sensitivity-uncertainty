######################################
### 19. Natural Resources Submodel ###
######################################

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
natural_resources_submodel <- "data/c_submodel_data/natural_resources_submodel.gpkg"

## Output directories
oregon_natural_resources_suitability <- "data/c_submodel_data/natural_resources_suitability.gpkg"
oregon_suitability_gpkg <- "data/d_suitability_data/suitability_model.gpkg"

#####################################

sf::st_layers(dsn = natural_resources_submodel,
              do_count = T)

#####################################
#####################################

# Load data
## Oregon hex
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Natural Resources
### Protected species
leatherback <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_leatherback")
killer_whale <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_leatherback")
humpback_ca_dps <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_leatherback")
humpback_mexico_dps <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_leatherback")
blue_whale <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_leatherback")

## Habitat layers
efhca <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_efhca_500m")
rocky_reef_mapped <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_rocky_reef_mapped_500m")
rocky_reef_probable <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_rocky_reef_probable_500m")
deep_sea_coral_sponge <- sf::st_read(dsn = natural_resources_submodel, layer = )
continental_shelf <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_continental_shelf_10km")
methane_bubble <- sf::st_read(dsn = natural_resources_submodel, layer = "oregon_hex_methane_bubble_streams_1km")

## Seabird

#####################################
#####################################

# Calculate score
## Protected species
### Individual species
#### Leatherback sea turtle
leatherback_value <- leatherback %>%
  dplyr::mutate(leatherback_value = 0.1) %>%
  as.data.frame()

#### Killer whale
killer_whale_value <- killer_whale %>%
  dplyr::mutate(killerwhale_value = 0.1) %>%
  as.data.frame()

#### Humpback whale (Central America DPS)
humpback_ca_value <- humpback_ca_dps %>%
  dplyr::mutate(humpback_ca_value = 0.5) %>%
  as.data.frame()

#### Humpback whale (Mexico DPS)
humpback_mx_value <- humpback_mexico_dps %>%
  dplyr::mutate(humpback_mx_value = 0.5) %>%
  as.data.frame()

#### Blue whale
bluewhale_value <- blue_whale %>%
  dplyr::mutate(bluewhale_value = 0.2) %>%
  as.data.frame()

### Summary value
protected_species <- oregon_hex %>%
  # join the leatherback sea turtle values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = leatherback_value,
                   by = "index") %>%
  # join the killer whale values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = killer_whale_value,
                   by = "index") %>%
  # join the humpback whale (Central America DPS) values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = humpback_ca_value,
                   by = "index") %>%
  # join the humpback whale (Mexico DPS) values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = humpback_mx_value,
                   by = "index") %>%
  # join the blue whale values by index field to full Oregon call area hex grid
  dplyr::left_join(x = .,
                   y = bluewhale_value,
                   by = "index") %>%
  # select only fields of interest
  dplyr::select(index,
                layer,
                leatherback_value,
                killerwhale_value,
                humpback_ca_value,
                humpback_mx_value,
                bluewhale_value,
                geom.x) %>%
  # rename geometry field
  dplyr::rename(geom = geom.x) %>%
  # calculate product of protected species
  dplyr::mutate(species_product = leatherback_value * killerwhale_value * humpback_ca_value * humpback_mx_value * bluewhale_value) %>%
  dplyr::select(index,
                layer,
                leatherback_value,
                killerwhale_value,
                humpback_ca_value,
                humpback_mx_value,
                bluewhale_value,
                species_product)

#####################################

## Habitat layers
### EFHCA
efhca_value <- efhca %>%
  dplyr::mutate(efhca_value = 0.01) %>%
  as.data.frame()

### Rocky reef (mapped)
rreef_map_value <- rocky_reef_mapped %>%
  dplyr::mutate(rreef_map_value = 0.01) %>%
  as.data.frame()

### Rocky reef (probable)
rreef_prob_value <- rocky_reef_probable %>%
  dplyr::mutate(rreef_prob_value = 0.2) %>%
  as.data.frame()

### Deep sea coral and sponge
coral_sponge_value <- deep_sea_coral_sponge %>%
  dplyr::mutate(deep_coralsponge_value = 0.01) %>%
  as.data.frame()

### Continental shelf break
continental_shelf_value <- continental_shelf %>%
  dplyr::mutate(continental_shelf_value = 0.6) %>%
  as.data.frame()

### Methane bubble streans
methane_bubble_value <- methane_bubble %>%
  dplyr::mutate(methane_bubble_value = 0.8) %>%
  as.data.frame()

### Summary value
habitat_values <- oregon_hex %>%
  dplyr::left_join(x = .,
                   y = efhca_value,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = rreef_map_value,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = rreef_prob_value,
                   by = "index") %>%
# dplyr::left_join(x = .,
#                  y = deep_coralsponge_value,
#                  by = "index") %>%
  dplyr::left_join(x = .,
                   y = continental_shelf_value,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = methane_bubble_value,
                   by = "index") %>%
  # select only fields of interest
  dplyr::select(index,
#                layer,
                efhca_value,
                rreef_map_value,
                rreef_prob_value,
                # deep_coralsponge_value,
                continental_shelf_value,
                methane_bubble_value,
                geom.x) %>%
  # rename geometry field
  dplyr::rename(geom = geom.x) %>%
  # calculate minimum value across the habitat subdatasets
  dplyr::rowwise() %>%
  dplyr::mutate(habitat_value = pmin(efhca_value,
                                     rreef_map_value,
                                     rreef_prob_value,
                                     # deep_coralsponge_value,
                                     continental_shelf_value,
                                     methane_bubble_value,
                                     na.rm = T)) %>%
  dplyr::select(index,
#                layer,
                efhca_value,
                rreef_map_value,
                rreef_prob_value,
                # deep_coralsponge_value,
                continental_shelf_value,
                methane_bubble_value,
                habitat_value)

#####################################
#####################################

# Calculate geometric mean

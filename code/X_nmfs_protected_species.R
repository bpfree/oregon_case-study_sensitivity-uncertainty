#################################
### X. NMFS Protected Species ###
#################################

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
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
nmfs_esa_habitat_dir <- "data/a_raw_data/NMFS_ESA_Critical_Habitat_20221017.gdb"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Analysis directories
fisheries_submodel <- "data/c_submodel_data/fisheries_submodel.gpkg"

#### Intermediate directories
nmfs_esa_gpkg <- "data/b_intermediate_data/nmfs_esa_protected_species.gpkg"

#####################################
#####################################

sf::st_layers(dsn = nmfs_esa_habitat_dir,
              do_count = TRUE)

# Layers of interest:
## 28 -- SeaTurtleLeatherback_20120126
## 34 -- WhaleHumpback_CentralAmericaDPS_20210421
## 35 -- WhaleHumpback_MexicoDPS_20210421
## 37 -- WhaleKiller_SouthernResidentDPS_20210802

#####################################
#####################################

# Load data
## Oregon Call Areas
oregon_call_areas <- sf::st_read(dsn = wind_area_gpkg,
                                 layer = paste(sf::st_layers(dsn = wind_area_gpkg,
                                                             do_count = TRUE)))

## Oregon hex areas
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

#####################################

## Species data (source: https://noaa.maps.arcgis.com/home/item.html?id=f66c1e33f91d480db7d1b1c1336223c3)
### NMFS ESA Critical Habitat Mapper: https://noaa.maps.arcgis.com/apps/webappviewer/index.html?id=68d8df16b39c48fe9f60640692d0e318
### NOAA InPort: https://www.fisheries.noaa.gov/inport/item/65207

### For West Coast Specific the download is here: https://www.webapps.nwfsc.noaa.gov/portal7/home/item.html?id=40d9b14ae87e4023ae07361cf3067007
### West Coast Region Protected Resources App: https://www.webapps.nwfsc.noaa.gov/portal/apps/webappviewer/index.html?id=7514c715b8594944a6e468dd25aaacc9

### Leatherback sea turtle (species specific data: https://www.fisheries.noaa.gov/resource/map/leatherback-turtle-critical-habitat-map-and-gis-data)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65327)
#### Code of regulations:  https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.207
leatherback_exclusion_area <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "SeaTurtleLeatherback_20120126") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")  %>% # EPSG 26910 (https://epsg.io/26910) %>%
  # obtain only submarine cables in the study area
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas) %>%
  # create field called "layer" and fill with "submarine cables" for summary
  dplyr::mutate(layer = "protected species")

### Humpback whale (Central America DPS) (species specific data: https://noaa.maps.arcgis.com/home/item.html?id=9426731f9651463bac4eb9cfba6574bd)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65375
#### Code of regulations: https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.227
humpback_central_america_dps <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "WhaleHumpback_MexicoDPS_20210421") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")  %>% # EPSG 26910 (https://epsg.io/26910) %>%
  # obtain only submarine cables in the study area
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas) %>%
  # create field called "layer" and fill with "submarine cables" for summary
  dplyr::mutate(layer = "protected species")

### Humpback whale (Mexico DPS) (species specific data: https://noaa.maps.arcgis.com/home/item.html?id=9426731f9651463bac4eb9cfba6574bd)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65377
#### Code of regulations: https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.227
humpback_mexico_dps <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "WhaleHumpback_CentralAmericaDPS_20210421") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")  %>% # EPSG 26910 (https://epsg.io/26910) %>%
  # obtain only submarine cables in the study area
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas) %>%
  # create field called "layer" and fill with "submarine cables" for summary
  dplyr::mutate(layer = "protected species")

### Killer whale (Southern Resident) (species specific data: https://noaa.maps.arcgis.com/home/item.html?id=fe1e0f42587f437b93e6168929f03593)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65409
#### Code of regulations: https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.206
killer_whale_critical_habitat <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "WhaleKiller_SouthernResidentDPS_20210802") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")  %>% # EPSG 26910 (https://epsg.io/26910) %>%
  # obtain only submarine cables in the study area
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas) %>%
  # create field called "layer" and fill with "submarine cables" for summary
  dplyr::mutate(layer = "protected species")

#####################################

# Oregon hex by species
oregon_leatherback <- oregon_hex[leatherback_exclusion_area, ]
oregon_humpback_ca_dps <- oregon_hex[humpback_central_america_dps, ]
oregon_humpback_mexico_dps <- oregon_hex[humpback_mexico_dps, ]
oregon_killer_whale <- oregon_hex[killer_whale_critical_habitat, ]

#####################################
#####################################

# Export data
sf::st_write(obj = oregon_leatherback, dsn = fisheries_submodel, layer = "oregon_hex_leatherback", append = F)
sf::st_write(obj = oregon_humpback_ca_dps, dsn = fisheries_submodel, layer = "oregon_hex_humpback_ca_dps", append = F)
sf::st_write(obj = oregon_humpback_mexico_dps, dsn = fisheries_submodel, layer = "oregon_hex_humpback_mexico_dps", append = F)
sf::st_write(obj = oregon_killer_whale, dsn = fisheries_submodel, layer = "oregon_hex_killer_whale", append = F)


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

### Blue Whale
### ***Note: Comments from NMFS seeks to close any areas between 42-degrees and 10 minutes and south to the Oregon-California border
### These are closed as foraging grounds to the blue whale. The comments reference 9 areas for blue whales for the biologically
### important areas. None of these areas (https://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip) intersect the Oregon
### call areas. Those data came from the 2015 dataset. In 2023, a new update was conducted (https://www.frontiersin.org/articles/10.3389/fmars.2023.1081893/full)
### Presently (31 March 2023) the data are limited to Alaska, yet it is expected data for the west coast will be produced.

### Oregon and California border is detailed as the 42nd parallel in both of their constitutions.
#### Oregon (Article XVI - Boundaries): https://www.oregonlegislature.gov/bills_laws/Pages/OrConst.aspx
#### Oregon's admittance to the US (see Admission of State--Boundaries) : https://sos.oregon.gov/blue-book/Pages/facts/history/congress-act.aspx
#### California (see Article 3 Section 170): https://leginfo.legislature.ca.gov/faces/codes_displayText.xhtml?division=1.&chapter=1.&lawCode=GOV&title=1.&article=3.

#### California-Oregon boundary line
cal_ore_boundary <- rbind(c("point",-124.1,42),
                          c("point",-124.3,42),
                          c("point",-124.6,42),
                          c("point",-124.3,42),
                          c("point",-124.9,42),
                          c("point",-125.2,42),
                          c("point",-125.5,42),
                          c("point",-125.8,42)) %>%
  # convert to data frame
  as.data.frame() %>%
  # rename column names
  dplyr::rename("point" = "V1",
                "lon" = "V2",
                "lat" = "V3") %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>% # EPSG 26910 (https://epsg.io/26910)
  # group all the points
  dplyr::group_by(point) %>%
  # combine geometries without resolving borders to create multipoint feature
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  # convert back to sf
  sf::st_as_sf() %>%
  # convert to linestring simple feature
  sf::st_cast("LINESTRING") %>%
  # convert back to sf
  sf::st_as_sf()

#### Oregon Call area line
oregon_4210_line <- rbind(c("point",-124.1,42.10),
                          c("point",-124.3,42.10),
                          c("point",-124.6,42.10),
                          c("point",-124.3,42.10),
                          c("point",-124.9,42.10),
                          c("point",-125.2,42.10),
                          c("point",-125.5,42.10),
                          c("point",-125.8,42.10)) %>%
  # convert to data frame
  as.data.frame() %>%
  # rename column names
  dplyr::rename("point" = "V1",
                "lon" = "V2",
                "lat" = "V3") %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>% # EPSG 26910 (https://epsg.io/26910)
  # group all the points
  dplyr::group_by(point) %>%
  # combine geometries without resolving borders to create multipoint feature
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  # convert back to sf
  sf::st_as_sf() %>%
  # convert to linestring simple feature
  sf::st_cast("LINESTRING") %>%
  # convert back to sf
  sf::st_as_sf()

test <- rbind(c("point",-124.1,42),
              c("point",-124.3,42),
              c("point",-124.6,42),
              c("point",-124.3,42),
              c("point",-124.9,42),
              c("point",-125.2,42),
              c("point",-125.5,42),
              c("point",-125.8,42),
              c("point",-124.1,42.10),
              c("point",-124.3,42.10),
              c("point",-124.6,42.10),
              c("point",-124.3,42.10),
              c("point",-124.9,42.10),
              c("point",-125.2,42.10),
              c("point",-125.5,42.10),
              c("point",-125.8,42.10)) %>%
  # convert to data frame
  as.data.frame() %>%
  # rename column names
  dplyr::rename("point" = "V1",
                "lon" = "V2",
                "lat" = "V3") %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>% # EPSG 26910 (https://epsg.io/26910)
  # group all the points
  dplyr::group_by(point) %>%
  # combine geometries without resolving borders to create multipoint feature
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  # convert back to sf
  sf::st_as_sf() %>%
  # convert to polygon simple feature
  sf::st_cast("POLYGON") %>%
  # convert back to sf
  sf::st_as_sf()

blue_whale <- oregon_call_areas %>%
  sf::st_make_valid() %>%
  rmapshaper::ms_clip(clip = test)

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


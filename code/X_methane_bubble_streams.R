#################################
### X. Methane Bubble Streams ###
#################################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docxtractr,
               dplyr,
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
data_dir <- "data/a_raw_data"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_geopackage <- "data/c_submodel_data/natural_resources.gpkg"

#### Intermediate directories
methane_bubble_gpkg <- "data/b_intermediate_data/methane_bubble_streams.gpkg"

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

## Methane bubble streams (Merle et al. 2021) (source: https://www.pmel.noaa.gov/eoi/Cascadia/Supplemental-Tables-US-only-revised-dec30-2020.xlsx)
### ***Note: data come from the supplemental table
### Paper: https://www.frontiersin.org/articles/10.3389/feart.2021.531714/full
methane_merle <- readxl::read_xlsx(path = paste(data_dir, "methane_bubble_streams_merle.xlsx", sep = "/"),
                                    # designate the sheet with the data, in this case 2 = 2015 (COD)
                                    sheet = 2) %>%
  # delete the 1st row as it does not relevant information
  dplyr::filter(!row_number() %in% c(1)) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")

## Methane bubble streams (Reidel et al. 2018) (source: https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-018-05736-x/MediaObjects/41467_2018_5736_MOESM4_ESM.xlsx)
### ***Note: data come from the supplementary data 2
### Paper: https://www.nature.com/articles/s41467-018-05736-x
methane_reidel <- readxl::read_xlsx(path = paste(data_dir, "methane_bubble_streams_reidel.xlsx", sep = "/"),
                                   # designate the sheet with the data, in this case 2 = 2015 (COD)
                                   sheet = 1)

# set column names to be values of first row
colnames(methane_reidel) <- methane_reidel[1,]

methane_reidel <- methane_reidel %>%
  # delete the first two rows as they do not contain data
  dplyr::filter(!row_number() %in% c(1:2)) %>%
  # delete last 12 rows as they do not contain data
  dplyr::filter(!row_number() %in% c(1114:1126)) %>%
  # remove any sites that do not contain longitude data
  dplyr::filter(!is.na(.$Longitude)) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")

## Open the methane bubble stream data from Johnson et al. (2015) (source: https://agupubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2F2015GC005955&file=ggge20859-sup-0001-2015GC005955-SupInfo.docx)
### ***Note: data come from the supporting information document (see S2 and S3)
### ***Note: S3 does not contain any sites that fall within Oregon call areas
### Paper: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015GC005955
methane_johnson_doc <- docxtractr::read_docx(file.path(data_dir, "methane_bubble_streams_johnson.docx"))

### extract data from supporting information table 2 (S2)
methane_johnson_s2 <- docxtractr::docx_extract_tbl(methane_johnson_doc,
                                     tbl_number = 1)

# set column names to be values of first row
colnames(methane_johnson_s2) <- methane_johnson_s2[1, ]

methane_johnson_s2 <- methane_johnson_s2 %>%
  # delete the first two rows as they do not contain data
  dplyr::filter(!row_number() %in% c(1:2)) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Lon", "Lat"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")

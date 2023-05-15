#############################################
### 11. Deep-sea Coral and Sponge Habitat ###
#############################################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               fasterize,
               fs,
               ggplot2,
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
coral_sponge_dir <- "data/a_raw_data/deep_sea_coral_sponge_habitat/0276883/1.1/data/0-data/NCCOS_USWestCoast_DSC_Models_2016_2020"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_geopackage <- "data/c_submodel_data/natural_resources.gpkg"

#### Intermediate directories
coral_sponge_habitat_gpkg <- "data/b_intermediate_data/coral_sponge_habitat.gpkg"

#### PACPARS directory
dir.create(paste0(intermediate_dir, "/",
                  "deep_sea_coral_sponge_habitat"))

coral_sponge_dir <- "data/b_intermediate_data/deep_sea_coral_sponge_habitat"

#####################################
#####################################

# Unzip the data for the coral richness (source: https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/276883.1.1.tar.gz)
### NCEI: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0276883
### Metadata: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0276883;view=iso
#### Link for habitat suitability data only: https://www.nodc.noaa.gov/archive/arc0211/0276883/1.1/data/0-data/NCCOS_USWestCoast_DSC_Models_2016_2020/Coral_Richness.zip

## Examine if the filename contains the pattern "Coral_Richness"
### grepl returns a logic statement when pattern "Coral_Richness" is met in the file
file <- basename(list.files(coral_sponge_dir)[3])

unzip_function <- function(data_dir, file){
  
  if (grepl("Coral_Richness", file)){
  
  # grab text before ".zip" and keep only text before that
  new_dir_name <- sub(".zip", "", file)
  
  # create new directory for data
  new_dir <- file.path(data_dir, new_dir_name)
  
  # unzip the file
  unzip(zipfile = file.path(paste0(coral_sponge_dir, "/", file)),
        # export file to the new data directory
        exdir = new_dir)
  
  # remove original zipped file
  file.remove(file.path(data_dir, file))
  }
}

unzip_function(coral_sponge_dir, file)

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

## set directory
richness_dir <- "data/a_raw_data/deep_sea_coral_sponge_habitat/0276883/1.1/data/0-data/NCCOS_USWestCoast_DSC_Models_2016_2020/Coral_Richness/Coral_Richness"

## Open high and robust high datasets
high_habitat <- terra::rast(paste(richness_dir, "Num_Taxa_Corals_HardSubstrate_with_High_Habitat_Suitability.tif", sep = "/"))
robust_habitat <- terra::rast(paste(richness_dir, "Num_Taxa_Corals_HardSubstrate_with_Robust_High_Habitat_Suitability.tif", sep = "/"))

# Convert raster to polygon
## High habitat suitability
high_habitat_polygon <- terra::as.polygons(x = high_habitat) %>%
  # change to simple feature (sf)
  sf::st_as_sf() %>%
  # cast to polygon
  sf::st_cast("POLYGON") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>%
  # simplify column name to "richness" (this is the first column of the object, thus the colnames(.)[1] means take the first column name from the high_habitat object)
  dplyr::rename(richness = colnames(.)[1]) %>%
  # obtain areas with species (richness >= 1)
  dplyr::filter(richness >= 1) %>%
  # add 500-meter setback
  sf::st_buffer(dist = 500) %>%
  # obtain data within Oregon call areas
  rmapshaper::ms_clip(clip = oregon_call_areas)

## Robust high habitat suitability
robust_habitat_polygon <- terra::as.polygons(x = robust_habitat) %>%
  sf::st_as_sf() %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>%
  # simplify column name to "richness"
  dplyr::rename(richness = colnames(.)[1]) %>%
  # obtain areas with species (richness >= 1)
  dplyr::filter(richness >= 1) %>%
  # add 500-meter setback
  sf::st_buffer(dist = 500) %>%
  # obtain data within Oregon call areas
  rmapshaper::ms_clip(clip = oregon_call_areas)

#####################################

# Deep-sea coral and sponge habitat in call area
## High habitat suitability
oregon_hex_coral_sponge_high <- oregon_hex[high_habitat_polygon, ] %>%
  # spatially join NMFS EFHCA values to Oregon hex cells
  sf::st_join(x = .,
              y = high_habitat_polygon,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(index, layer)

## Robust high habitat suitability
oregon_hex_coral_sponge_robust <- oregon_hex[robust_habitat_polygon, ] %>%
  # spatially join NMFS EFHCA values to Oregon hex cells
  sf::st_join(x = .,
              y = robust_habitat_polygon,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(index, layer)

#####################################
#####################################

# Export data
## Natural resources submodel
sf::st_write(obj = oregon_hex_coral_sponge_high, dsn = natural_resources_geopackage, layer = "oregon_hex_high_habitat_coral_sponge", append = F)
sf::st_write(obj = oregon_hex_coral_sponge_robust, dsn = natural_resources_geopackage, layer = "oregon_hex_robust_habitat_coral_sponge", append = F)

## Deep-sea coral and sponge habitat
sf::st_write(obj = high_habitat_polygon, dsn = coral_sponge_habitat_gpkg, layer = "coral_sponge_high_habitat_polygon", append = F)
sf::st_write(obj = robust_habitat_polygon, dsn = coral_sponge_habitat_gpkg, layer = "coral_sponge_robust_habitat_polygon", append = F)

sf::st_write(obj = oregon_hex_coral_sponge_high, dsn = coral_sponge_habitat_gpkg, layer = "oregon_hex_high_habitat_coral_sponge", append = F)
sf::st_write(obj = oregon_hex_coral_sponge_robust, dsn = coral_sponge_habitat_gpkg, layer = "oregon_hex_robust_habitat_coral_sponge", append = F)

terra::writeRaster(high_habitat, filename = file.path(coral_sponge_dir, "coral_sponge_high_habitat.grd"), overwrite = T)
terra::writeRaster(robust_habitat, filename = file.path(coral_sponge_dir, "coral_sponge_robust_habitat.grd"), overwrite = T)

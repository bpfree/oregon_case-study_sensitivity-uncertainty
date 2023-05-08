##################################
### X. NMFS Scientific Surveys ###
##################################

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

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Analysis directories
industry_operations_submodel <- "data/c_submodel_data/industry_operations_submodel.gpkg"

#### Intermediate directories
nmfs_scientific_survey_gpkg <- "data/b_intermediate_data/oregon_nmfs_scientific_survey.gpkg"

#####################################
#####################################

# Make scientific survey 
## This function will take the imported data and reduce it down to a single feature.
scientific_survey_function <- function(scientific_survey_points){
  scientific_survey_corridor <- scientific_survey_points %>%
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
    sf::st_as_sf() %>%
    # add 2 nautical mile buffer (4 nautical mile wide) [1 nautical mile = 3704 meters]
    sf::st_buffer(dist = 3704)
  return(scientific_survey_corridor)
}

#####################################
#####################################

# Load data
## Oregon hex areas
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

## Oregon call areas
oregon_call_areas <- sf::st_read(dsn = wind_area_gpkg,
                                 layer = paste(sf::st_layers(dsn = wind_area_gpkg,
                                                             do_count = TRUE)))

#####################################
#####################################

# East-West Sampling Survey
## Coos Bay Call Area
### ***Note: Comments from NOAA (NMFS, NCCOS, & IOOS) seek to close any areas around sampling corridors at 43°20’ N, 43°30’ N,
### 43°40’ N, and 43°50’ N. These corridors mark the areas where NOAA conducts the Pelagic Fish Survey transects and the
### Pre-Recruit Survey and Northern California Ecosystem Survey samples sampling station lines.

## Brookings Call Area
### ***Note: Comments from NOAA (NMFS, NCCOS, & IOOS) seek to close any areas around sampling corridors 42°00’ N and 42°10’ N
### as these are areas where NOAA conducts other surveys for the Pre-Recruit Survey and Northern California Ecosystem Survey.

### NOAA (NMFS, NCCOS, & IOOS) Comments: https://www.regulations.gov/comment/BOEM-2022-0009-0178

## Coos Bay Call Area
### 43°20' N
east_west_4320 <- rbind(c("point",-125.8,43.3333333),
                        c("point",-125.6,43.3333333),
                        c("point",-125.4,43.3333333),
                        c("point",-125.2,43.3333333),
                        c("point",-125.0,43.3333333),
                        c("point",-124.8,43.3333333),
                        c("point",-124.6,43.3333333),
                        c("point",-124.4,43.3333333),
                        c("point",-124.2,43.3333333),
                        c("point",-124.0,43.3333333)) %>%
  scientific_survey_function()

### 43°30' N
east_west_4330 <- rbind(c("point",-125.8,43.5),
                        c("point",-125.6,43.5),
                        c("point",-125.4,43.5),
                        c("point",-125.2,43.5),
                        c("point",-125.0,43.5),
                        c("point",-124.8,43.5),
                        c("point",-124.6,43.5),
                        c("point",-124.4,43.5),
                        c("point",-124.2,43.5),
                        c("point",-124.0,43.5)) %>%
  scientific_survey_function()

### 43°40' N
east_west_4340 <- rbind(c("point",-125.8,43.6666667),
                        c("point",-125.6,43.6666667),
                        c("point",-125.4,43.6666667),
                        c("point",-125.2,43.6666667),
                        c("point",-125.0,43.6666667),
                        c("point",-124.8,43.6666667),
                        c("point",-124.6,43.6666667),
                        c("point",-124.4,43.6666667),
                        c("point",-124.2,43.6666667),
                        c("point",-124.0,43.6666667)) %>%
  scientific_survey_function()

### 43°50' N
east_west_4350 <- rbind(c("point",-125.8,43.8333333),
                        c("point",-125.6,43.8333333),
                        c("point",-125.4,43.8333333),
                        c("point",-125.2,43.8333333),
                        c("point",-125.0,43.8333333),
                        c("point",-124.8,43.8333333),
                        c("point",-124.6,43.8333333),
                        c("point",-124.4,43.8333333),
                        c("point",-124.2,43.8333333),
                        c("point",-124.0,43.8333333)) %>%
  scientific_survey_function()

#####################################

## Brookings Call Area
### 42°00' N
east_west_4200 <- rbind(c("point",-125.8,42.0),
                        c("point",-125.6,42.0),
                        c("point",-125.4,42.0),
                        c("point",-125.2,42.0),
                        c("point",-125.0,42.0),
                        c("point",-124.8,42.0),
                        c("point",-124.6,42.0),
                        c("point",-124.4,42.0),
                        c("point",-124.2,42.0),
                        c("point",-124.0,42.0)) %>%
  scientific_survey_function()

### 42°10' N
east_west_4210 <- rbind(c("point",-125.8,42.1666667),
                        c("point",-125.6,42.1666667),
                        c("point",-125.4,42.1666667),
                        c("point",-125.2,42.1666667),
                        c("point",-125.0,42.1666667),
                        c("point",-124.8,42.1666667),
                        c("point",-124.6,42.1666667),
                        c("point",-124.4,42.1666667),
                        c("point",-124.2,42.1666667),
                        c("point",-124.0,42.1666667)) %>%
  scientific_survey_function()

#####################################

# All scientific surveys
scientific_survey_corridors <- east_west_4320 %>%
  # additional Coos Bay call areas
  rbind(east_west_4330,
        east_west_4340,
        east_west_4350,
        # Brookings call area
        east_west_4200,
        east_west_4210)

oregon_call_area_scientific_survey_corridors <- scientific_survey_corridors %>%
  # limit survey corridors to the call areas
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas)

#####################################
#####################################

# Additional scientific survey corridors
## ***Note: NOAA further requested that areas avoid two additional scientific survey corridors (42°20'N and 42°30'N)
## Both of these areas fall within the Brookings call area. Like the other scientific surveys, they receive a
## 4-nautical mile buffer.

### 42°20' N
additional_east_west_4220 <- rbind(c("point",-125.8,42.3333333),
                                   c("point",-125.6,42.3333333),
                                   c("point",-125.4,42.3333333),
                                   c("point",-125.2,42.3333333),
                                   c("point",-125.0,42.3333333),
                                   c("point",-124.8,42.3333333),
                                   c("point",-124.6,42.3333333),
                                   c("point",-124.4,42.3333333),
                                   c("point",-124.2,42.3333333),
                                   c("point",-124.0,42.3333333)) %>%
  scientific_survey_function()

### 42°30' N
additional_east_west_4230 <- rbind(c("point",-125.8,42.5),
                                   c("point",-125.6,42.5),
                                   c("point",-125.4,42.5),
                                   c("point",-125.2,42.5),
                                   c("point",-125.0,42.5),
                                   c("point",-124.8,42.5),
                                   c("point",-124.6,42.5),
                                   c("point",-124.4,42.5),
                                   c("point",-124.2,42.5),
                                   c("point",-124.0,42.5)) %>%
  scientific_survey_function()

#####################################

additional_scientific_survey_corridors <- additional_east_west_4220 %>%
  rbind(additional_east_west_4230)

#####################################
#####################################


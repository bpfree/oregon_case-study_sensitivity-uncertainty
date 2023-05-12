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
survey_gpkg <- "data/a_raw_data/NCCOS_Share.gdb"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Analysis directories
industry_operations_submodel <- "data/c_submodel_data/industry_operations_submodel.gpkg"

#### Intermediate directories
nmfs_scientific_survey_gpkg <- "data/b_intermediate_data/oregon_nmfs_scientific_survey.gpkg"

#####################################

sf::st_layers(dsn = survey_gpkg,
              do_count = T)

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
nmfs_scientific_survey_corridors <- east_west_4320 %>%
  # additional Coos Bay call areas
  rbind(east_west_4330,
        east_west_4340,
        east_west_4350,
        # Brookings call area
        east_west_4200,
        east_west_4210)

oregon_nmfs_scientific_survey_corridors <- nmfs_scientific_survey_corridors %>%
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

nmfs_additional_scientific_survey_corridors <- additional_east_west_4220 %>%
  rbind(additional_east_west_4230)

oregon_nmfs_additional_scientific_survey_corridors <- nmfs_additional_scientific_survey_corridors %>%
  # limit survey corridors to the call areas
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas)

#####################################
#####################################

## *** Note: NOAA requested additional exclusion areas around the survey stations for the
## Pre-Recruit and Northern California Current Ecosystem surveys. Table 4 within NOAA's
## comments to BOEM provide hyperlinks for the surveys. Within the table, the survey design
## states that there are fixed stations and transects (in the case of Northern California
## Current Ecosystem survey).

### NOAA (NMFS, NCCOS, & IOOS) Comments: https://www.regulations.gov/comment/BOEM-2022-0009-0178

## The link for the Pre-Recruit data navigates to here (https://www.fisheries.noaa.gov/inport/item/20562).
## Contained within those metadata is a link to the Coastwide Cooperative Pre-Recruit Survey.
## It contains two different data tables:
##  1.) Prerecruit Survey Trawl Data Catch: https://www.webapps.nwfsc.noaa.gov/apex/parrdata/inventory/tables/table/prerecruit_survey_trawl_data_catch
##  2.) Prerecruit Survey Trawl Data Haul: https://www.webapps.nwfsc.noaa.gov/apex/parrdata/inventory/tables/table/prerecruit_survey_trawl_data_haul

## These datasets have the same spatial locations even as the number of observations are different: data catch (3227) vs. data haul (220)

## The link for the Northern California Current Ecosystem navigates to a page writing about the survey.
## The page does not contain any data related to the survey.

## NOAA's Integrated Ecosystem Assessment studies the California Current: https://www.integratedecosystemassessment.noaa.gov/index.php/regions/california-current
## The most recent report states that the assessment surveys along the Newport Line in Oregon: https://www.pcouncil.org/documents/2023/02/h-1-a-cciea-team-report-1-electronic-only-2022-2023-california-current-ecosystem-status-report-and-appendices.pdf/
## For more information about the Newport Hydrographic Line: https://www.integratedecosystemassessment.noaa.gov/regions/california-current/newport-hydrographic-line
## This would be too far north for the Oregon call areas

### Data used for the analysis were created and provided by Curt Whitmire (curt.whitmire@noaa.gov).
### Survey location data were passed to Curt from various partners at the NWFSC and SWFSC.
### As mentioned above, surveys came as points (fixed stations) and lines (fixed transects).

# Survey stations
nmfs_survey_stations <- sf::st_read(dsn = survey_gpkg, layer = "NMFS_SurveyStations")%>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>% # EPSG 26910 (https://epsg.io/26910) %>%
  # select only the two surveys of interest: Pre-Recruit Survey and Northern California Current Ecosystem Survey
  dplyr::filter(SurveyName %in% c("Pre-Recruit Survey", "Northern California Current Ecosystem Survey"))

oregon_nmfs_survey_stations <- nmfs_survey_stations %>%
  # add 2 nautical mile buffer around transect line (1 nautical mile = 1852 meters)
  sf::st_buffer(dist = 3704) %>%
  # limit survey transects to the call areas
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas)


# Survey transects
nmfs_survey_transects <- sf::st_read(dsn = survey_gpkg, layer = "NMFS_SurveyTransects") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>% # EPSG 26910 (https://epsg.io/26910) %>%
  # select only the two surveys of interest: Pre-Recruit Survey and Northern California Current Ecosystem Survey
  dplyr::filter(SurveyName %in% c("Pre-Recruit Survey", "Northern California Current Ecosystem Survey"))

oregon_nmfs_survey_transects <- nmfs_survey_transects %>%
  # add 1 nautical mile buffer around transect line (1 nautical mile = 1852 meters)
  sf::st_buffer(dist = 1852) %>%
  # limit survey transects to the call areas
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas)

#####################################
#####################################

# Oregon hex
## East-West scientific survey corridors
oregon_hex_eastwest_survey_corridors <- oregon_hex[oregon_nfms_scientific_survey_corridors, ] %>%
  sf::st_join(x = .,
              y = oregon_nmfs_scientific_survey_corridors,
              join = st_intersects)

## Additional east-west scientific survey corridors
oregon_hex_additional_eastwest_survey_corridors <- oregon_hex[nmfs_additional_scientific_survey_corridors, ] %>%
  sf::st_join(x = .,
              y = nmfs_additional_scientific_survey_corridors,
              join = st_intersects)

## Survey stations
oregon_hex_survey_stations <- oregon_hex[oregon_nmfs_survey_stations, ] %>%
  sf::st_join(x = .,
              y = oregon_nmfs_survey_stations,
              join = st_intersects)

## Survey transects
oregon_hex_survey_transects <- oregon_hex[oregon_nmfs_survey_transects, ] %>%
  sf::st_join(x = .,
              y = oregon_nmfs_survey_transects,
              join = st_intersects)

#####################################
#####################################

# Export data
## Submodel geopackage
sf::st_write(obj = oregon_hex_eastwest_survey_corridors, dsn = industry_operations_submodel, layer = "oregon_hex_eastwest_survey_corridors", append = F)
sf::st_write(obj = oregon_hex_additional_eastwest_survey_corridors, dsn = industry_operations_submodel, layer = "oregon_hex_additional_eastwest_survey_corridors", append = F)
sf::st_write(obj = oregon_hex_survey_stations, dsn = industry_operations_submodel, layer = "oregon_hex_survey_stations", append = F)
sf::st_write(obj = oregon_hex_survey_transects, dsn = industry_operations_submodel, layer = "oregon_hex_survey_transects", append = F)

## Scientific survey geopackage
### Coos Bay
sf::st_write(obj = east_west_4320, dsn = nmfs_scientific_survey_gpkg, layer = "east_west_4320_corridor", append = F)
sf::st_write(obj = east_west_4330, dsn = nmfs_scientific_survey_gpkg, layer = "east_west_4330_corridor", append = F)
sf::st_write(obj = east_west_4340, dsn = nmfs_scientific_survey_gpkg, layer = "east_west_4340_corridor", append = F)
sf::st_write(obj = east_west_4350, dsn = nmfs_scientific_survey_gpkg, layer = "east_west_4350_corridor", append = F)

### Brookings
sf::st_write(obj = east_west_4200, dsn = nmfs_scientific_survey_gpkg, layer = "east_west_4200_corridor", append = F)
sf::st_write(obj = east_west_4210, dsn = nmfs_scientific_survey_gpkg, layer = "east_west_4210_corridor", append = F)

### Additional Scientific Surveys
sf::st_write(obj = additional_east_west_4220, dsn = nmfs_scientific_survey_gpkg, layer = "additional_east_west_4220_corridor", append = F)
sf::st_write(obj = additional_east_west_4230, dsn = nmfs_scientific_survey_gpkg, layer = "additional_east_west_4230_corridor", append = F)

### East-West corridors
sf::st_write(obj = nmfs_scientific_survey_corridors, dsn = nmfs_scientific_survey_gpkg, layer = "nmfs_scientific_survey_corridorsr", append = F)
sf::st_write(obj = oregon_nmfs_scientific_survey_corridors, dsn = nmfs_scientific_survey_gpkg, layer = "oregon_nmfs_scientific_survey_corridors", append = F)
sf::st_write(obj = nmfs_additional_scientific_survey_corridors, dsn = nmfs_scientific_survey_gpkg, layer = "nmfs_additional_scientific_survey_corridors", append = F)
sf::st_write(obj = oregon_nmfs_additional_scientific_survey_corridors, dsn = nmfs_scientific_survey_gpkg, layer = "oregon_nmfs_additional_scientific_survey_corridors", append = F)

### Survey Stations
sf::st_write(obj = nmfs_survey_stations, dsn = nmfs_scientific_survey_gpkg, layer = "nmfs_survey_stations", append = F)
sf::st_write(obj = oregon_nmfs_survey_stations, dsn = nmfs_scientific_survey_gpkg, layer = "oregon_nmfs_survey_stations", append = F)

### Survey Transects
sf::st_write(obj = nmfs_survey_transects, dsn = nmfs_scientific_survey_gpkg, layer = "nmfs_survey_transects", append = F)
sf::st_write(obj = oregon_nmfs_survey_transects, dsn = nmfs_scientific_survey_gpkg, layer = "oregon_nmfs_survey_transecs", append = F)

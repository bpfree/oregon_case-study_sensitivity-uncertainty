###############################
### X. NREL Mean Wind Speed ###
###############################

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
               rhdf5,
               rmapshaper,
               rnaturalearth, # use devtools::install_github("ropenscilabs/rnaturalearth") if packages does not install properly
               sf,
               sp,
               terra, # is replacing the raster package
               tidyr)

### ***Note: run only if not installed
# install.packages("BiocManager")
# BiocManager::install("rhdf5")

#####################################
#####################################

# Set directories
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
all_data <- "data/a_raw_data/all.h5"
month_data <- "data/a_raw_data/month.h5"
hour_data <- "data/a_raw_data/hour.h5"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_geopackage <- "data/c_submodel_data/natural_resources.gpkg"

#### Intermediate directories
nrel_wind_gpkg <- "data/b_intermediate_data/nrel_wind.gpkg"

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

all_wind <- rhdf5::H5Fopen(name = all_data)
rhdf5::h5ls(all_wind)
View(all_wind$"datetime")
View(as.data.frame(all_wind$"latitude"))
View(as.data.frame(all_wind$"longitude"))
View(all_wind$"mean")

all_date <- all_wind$"datetime"
all_lat <- as.data.frame(all_wind$"latitude")
all_lon <- as.data.frame(all_wind$"longitude")
all_mean <- t(all_wind$"mean")
all_n <- t(all_wind$"n")
all_wk <- t(all_wind$"wk")

all_wind <- cbind(all_lat,
                all_lon,
                all_mean)


all_df <- cbind(all_lat,
                all_lon,
                all_mean) %>%
  dplyr::rename(lon = "all_wind$longitude",
                lat = "all_wind$latitude",
                mean = "all_mean") %>%
  dplyr::select(lon,
                lat,
                mean) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>%
  # get wind averages within study area
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas)

plot(all_df)

sf::st_write(obj = all_df, dsn = nrel_wind_gpkg, layer = "nrel_offshore_mean_annual_wind", append = F)

#####################################

month_wind <- rhdf5::H5Fopen(name = month_data)
rhdf5::h5ls(month_wind)
month_wind$"datetime"
month_wind$"latitude"
month_wind$"longitude"
month_wind$"mean"

month_date <- month_wind$"datetime"
month_lat <- month_wind$"latitude"
month_lon <- month_wind$"longitude"
month_mean <- month_wind$"mean"
month_n <- month_wind$"n"
month_wk <- month_wind$"wk"


month_df <- cbind(month_lat,
                month_lon,
                month_mean,
                month_n,
                month_wk)

#####################################

hour_wind <- rhdf5::H5Fopen(name = hour_data)
rhdf5::h5ls(hour_wind)
View(hour_wind$"datetime")
hour_wind$"latitude"
hour_wind$"longitude"
hour_wind$"mean"

hour_date <- hour_wind$"datetime"
hour_lat <- hour_wind$"latitude"
hour_lon <- hour_wind$"longitude"
hour_mean <- t(hour_wind$"mean")
hour_n <- t(hour_wind$"n")
hour_wk <- t(hour_wind$"wk")


hour_df <- cbind(hour_lat,
                  hour_lon,
                  hour_mean,
                  hour_n,
                  hour_wk)
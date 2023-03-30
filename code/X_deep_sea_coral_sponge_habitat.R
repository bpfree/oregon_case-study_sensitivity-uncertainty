############################################
### X. Deep-sea Coral and Sponge Habitat ###
############################################

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
coral_sponge_habitat_gpkg <- "data/b_intermediate_data/pacpars.gpkg"

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

############################################
### 15. Marine Bird Species Density Maps ###
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
marine_bird_dir <- "data/a_raw_data/marine_bird/0242882/1.1/data/0-data/model_output_predictions/"
marine_bird_species_dir <- "data/b_intermediate_data/marine_bird_species"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_submodel <- "data/c_submodel_data/natural_resources_submodel.gpkg"

#### Intermediate directories
##### Marine bird species geopackage (vector data)
marine_bird_gpkg <- "data/b_intermediate_data/marine_bird_species.gpkg"

#####################################
#####################################

list.files(marine_bird_dir)

#####################################
#####################################

# Create shorthands for seasonal data
spring <- "_spring_predicted_density.tif"
summer <- "_summer_predicted_density.tif"
fall <- "_fall_predicted_density.tif"
winter <- "_winter_predicted_density.tif"

#####################################
#####################################

# Load data

## Oregon
### Oregon Call Areas
oregon_call_areas <- sf::st_read(dsn = wind_area_gpkg,
                                 layer = paste(sf::st_layers(dsn = wind_area_gpkg,
                                                             do_count = TRUE)))

### Oregon hex areas
oregon_hex <- sf::st_read(dsn = study_area_gpkg,
                          layer = paste(sf::st_layers(dsn = study_area_gpkg,
                                                      do_count = TRUE)[[1]][2]))

#####################################

marine_bird_vulnerability <- base::readRDS(file = paste(marine_bird_species_dir, "marine_bird_species_vulnerability.rds", sep = "/"))

#####################################

## Marine bird species (source: https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/242882.1.1.tar.gz)
### ***NOTE: NOAA conducted an analysis on marine bird species that combined data from Leirness et al. 2021)
###          https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0242882. For more specific downloads
###          on just the model output predictors: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/

### ***NOTE: NOAA analyzed and quantified the data for 30 species and 12 taxonomic groups
###          Densities were summarized by season  (fall, spring, summer, winter) -- though not all seasons exist

#### Species
####   1.) South polar skua (Stercorarius maccormicki) -- SPSK:
####      Fall (https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SPSK_fall_predicted_density.tif)
spsk_fall <- terra::rast(x = paste0(marine_bird_dir, "SPSK", fall))

####   2.) Common murre (Uria aalge) -- COMU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_winter_predicted_density.tif
comu_spring <- terra::rast(x = paste0(marine_bird_dir, "comu", spring))
comu_summer <- terra::rast(x = paste0(marine_bird_dir, "comu", summer))
comu_fall <- terra::rast(x = paste0(marine_bird_dir, "comu", fall))
comu_winter <- terra::rast(x = paste0(marine_bird_dir, "comu", winter))

####   3.) Pigeon guillemot (Cepphus columba) -- PIGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PIGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PIGU_summer_predicted_density.tif
pigu_spring <- terra::rast(x = paste0(marine_bird_dir, "pigu", spring))
pigu_summer <- terra::rast(x = paste0(marine_bird_dir, "pigu", summer))

####   4.) Marbled murrelet (Brachyramphus marmoratus) -- MAMU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MAMU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MAMU_summer_predicted_density.tif
mamu_spring <- terra::rast(x = paste0(marine_bird_dir, "mamu", spring))
mamu_summer <- terra::rast(x = paste0(marine_bird_dir, "mamu", summer))

####   5.) Ancient murrelet (Synthliboramphus antiquus) -- ANMU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ANMU_spring_predicted_density.tif
anmu_spring <- terra::rast(x = paste0(marine_bird_dir, "anmu", spring))

####   6.) Cassin's auklet (Ptychoramphus aleuticus) -- CAAU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_winter_predicted_density.tif
caau_spring <- terra::rast(x = paste0(marine_bird_dir, "caau", spring))
caau_summer <- terra::rast(x = paste0(marine_bird_dir, "caau", summer))
caau_fall <- terra::rast(x = paste0(marine_bird_dir, "caau", fall))
caau_winter <- terra::rast(x = paste0(marine_bird_dir, "caau", winter))

####   7.) Rhinoceros auklet (Cerorhinca monocerata) -- RHAU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_winter_predicted_density.tif
rhau_spring <- terra::rast(x = paste0(marine_bird_dir, "rhau", spring))
rhau_summer <- terra::rast(x = paste0(marine_bird_dir, "rhau", summer))
rhau_fall <- terra::rast(x = paste0(marine_bird_dir, "rhau", fall))
rhau_winter <- terra::rast(x = paste0(marine_bird_dir, "rhau", winter))

####   8.) Tufted puffin (Fratercula cirrhata) -- TUPU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/TUPU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/TUPU_summer_predicted_density.tif
tupu_spring <- terra::rast(x = paste0(marine_bird_dir, "tupu", spring))
tupu_summer <- terra::rast(x = paste0(marine_bird_dir, "tupu", summer))

####   9.) Black-legged kittiwake (Rissa tridactyla) -- BLKI:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_winter_predicted_density.tif
blki_spring <- terra::rast(x = paste0(marine_bird_dir, "blki", spring))
blki_fall <- terra::rast(x = paste0(marine_bird_dir, "blki", fall))
blki_winter <- terra::rast(x = paste0(marine_bird_dir, "blki", winter))

####   10.) Sabine's gull (Xema sabini) -- SAGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_fall_predicted_density.tif
sagu_spring <- terra::rast(x = paste0(marine_bird_dir, "sagu", spring))
sagu_summer <- terra::rast(x = paste0(marine_bird_dir, "sagu", summer))
sagu_fall <- terra::rast(x = paste0(marine_bird_dir, "sagu", fall))

####   11.) Bonaparte's gull (Chroicocephalus philadelphia) -- BOGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_winter_predicted_density.tif
bogu_spring <- terra::rast(x = paste0(marine_bird_dir, "bogu", spring))
bogu_fall <- terra::rast(x = paste0(marine_bird_dir, "bogu", fall))
bogu_winter <- terra::rast(x = paste0(marine_bird_dir, "bogu", winter))

####   12.) Heermann's gull (Larus heermanni) -- HEEG:
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_fall_predicted_density.tif  
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_winter_predicted_density.tif
heeg_summer <- terra::rast(x = paste0(marine_bird_dir, "heeg", summer))
heeg_fall <- terra::rast(x = paste0(marine_bird_dir, "heeg", fall))
heeg_winter <- terra::rast(x = paste0(marine_bird_dir, "heeg", winter))

####   13.) California gull (Larus californicus) -- CAGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_winter_predicted_density.tif
cagu_spring <- terra::rast(x = paste0(marine_bird_dir, "cagu", spring))
cagu_summer <- terra::rast(x = paste0(marine_bird_dir, "cagu", summer))
cagu_fall <- terra::rast(x = paste0(marine_bird_dir, "cagu", fall))
cagu_winter <- terra::rast(x = paste0(marine_bird_dir, "cagu", winter))

####   14.) Caspian tern (Hydroprogne caspia) -- CATE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CATE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CATE_summer_predicted_density.tif
cate_spring <- terra::rast(x = paste0(marine_bird_dir, "cate", spring))
cate_summer <- terra::rast(x = paste0(marine_bird_dir, "cate", summer))

####   15.) Laysan albratross (Phoebastria immutabilis) -- LAAL:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LAAL_spring_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LAAL_winter_predicted_density.tif
laal_spring <- terra::rast(x = paste0(marine_bird_dir, "laal", spring))
laal_winter <- terra::rast(x = paste0(marine_bird_dir, "laal", winter))

####   16.) Black-footed alaatross (Phoebastria nigripes) -- BFAL:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_winter_predicted_density.tif
bfal_spring <- terra::rast(x = paste0(marine_bird_dir, "bfal", spring))
bfal_summer <- terra::rast(x = paste0(marine_bird_dir, "bfal", summer))
bfal_fall <- terra::rast(x = paste0(marine_bird_dir, "bfal", fall))
bfal_winter <- terra::rast(x = paste0(marine_bird_dir, "bfal", winter))

####   17.) Fork-tailed storm-petrel (Hydrobates furcatus) -- FTSP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_winter_predicted_density.tif
ftsp_spring <- terra::rast(x = paste0(marine_bird_dir, "ftsp", spring))
ftsp_summer <- terra::rast(x = paste0(marine_bird_dir, "ftsp", summer))
ftsp_fall <- terra::rast(x = paste0(marine_bird_dir, "ftsp", fall))
ftsp_winter <- terra::rast(x = paste0(marine_bird_dir, "ftsp", winter))

####   18.) Leach's storm-petrel (Hydrobates leucorhous) -- LESP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_winter_predicted_density.tif
lesp_spring <- terra::rast(x = paste0(marine_bird_dir, "lesp", spring))
lesp_summer <- terra::rast(x = paste0(marine_bird_dir, "lesp", summer))
lesp_fall <- terra::rast(x = paste0(marine_bird_dir, "lesp", fall))
lesp_winter <- terra::rast(x = paste0(marine_bird_dir, "lesp", winter))

####   19.) Ashy storm-petrel (Hydrobates homochroa) -- ASSP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_fall_predicted_density.tif
assp_spring <- terra::rast(x = paste0(marine_bird_dir, "assp", spring))
assp_summer <- terra::rast(x = paste0(marine_bird_dir, "assp", summer))
assp_fall <- terra::rast(x = paste0(marine_bird_dir, "assp", fall))

####   20.) Black storm-petrel (Hydrobates melania) -- BLSP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_fall_predicted_density.tif
blsp_spring <- terra::rast(x = paste0(marine_bird_dir, "blsp", spring))
blsp_summer <- terra::rast(x = paste0(marine_bird_dir, "blsp", summer))
blsp_fall <- terra::rast(x = paste0(marine_bird_dir, "blsp", fall))

####   21.) Northern fulmar (Fulmarus glacialis) -- NOFU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_winter_predicted_density.tif
nofu_spring <- terra::rast(x = paste0(marine_bird_dir, "nofu", spring))
nofu_summer <- terra::rast(x = paste0(marine_bird_dir, "nofu", summer))
nofu_fall <- terra::rast(x = paste0(marine_bird_dir, "nofu", fall))
nofu_winter <- terra::rast(x = paste0(marine_bird_dir, "nofu", winter))

####   22.) Murphy's petrel (Pterodroma ultima) -- MUPE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MUPE_spring_predicted_density.tif
mupe_spring <- terra::rast(x = paste0(marine_bird_dir, "mupe", spring))

####   23.) Cook's petrel (Pterodroma cookii) -- COPE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_fall_predicted_density.tif
cope_spring <- terra::rast(x = paste0(marine_bird_dir, "cope", spring))
cope_summer <- terra::rast(x = paste0(marine_bird_dir, "cope", summer))
cope_fall <- terra::rast(x = paste0(marine_bird_dir, "cope", fall))

####   24.) Buller's shearwater (Ardenna bulleri) -- BULS:
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BULS_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BULS_fall_predicted_density.tif
buls_summer <- terra::rast(x = paste0(marine_bird_dir, "buls", summer))
buls_fall <- terra::rast(x = paste0(marine_bird_dir, "buls", fall))

####   25.) Pink-footed shearwater (Ardenna creatopus) -- PFSH:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_fall_predicted_density.tif
pfsh_spring <- terra::rast(x = paste0(marine_bird_dir, "pfsh", spring))
pfsh_summer <- terra::rast(x = paste0(marine_bird_dir, "pfsh", summer))
pfsh_fall <- terra::rast(x = paste0(marine_bird_dir, "pfsh", fall))

####   26.) Black-vented shearwater (Puffinus opisthomelas) -- BVSH:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_winter_predicted_density.tif
bvsh_spring <- terra::rast(x = paste0(marine_bird_dir, "bvsh", spring))
bvsh_fall <- terra::rast(x = paste0(marine_bird_dir, "bvsh", fall))
bvsh_winter <- terra::rast(x = paste0(marine_bird_dir, "bvsh", winter))

####   27.) Brandt's cormorant (Phalacrocorax penicillatus) -- BRAC:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRAC_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRAC_summer_predicted_density.tif
brac_spring <- terra::rast(x = paste0(marine_bird_dir, "brac", spring))
brac_summer <- terra::rast(x = paste0(marine_bird_dir, "brac", summer))

####   28.) Pelagic cormorant (Phalacrocorax pelagicus) -- PECO:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PECO_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PECO_summer_predicted_density.tif
peco_spring <- terra::rast(x = paste0(marine_bird_dir, "peco", spring))
peco_summer <- terra::rast(x = paste0(marine_bird_dir, "peco", summer))

####   29.) Double-crested cormorant (Phalacrocorax auritus) -- DCCO:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/DCCO_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/DCCO_summer_predicted_density.tif
dcco_spring <- terra::rast(x = paste0(marine_bird_dir, "dcco", spring))
dcco_summer <- terra::rast(x = paste0(marine_bird_dir, "dcco", summer))

####   30.) Brown pelican (Pelecanus occidentalis) -- BRPE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_winter_predicted_density.tif
brpe_spring <- terra::rast(x = paste0(marine_bird_dir, "brpe", spring))
brpe_summer <- terra::rast(x = paste0(marine_bird_dir, "brpe", summer))
brpe_fall <- terra::rast(x = paste0(marine_bird_dir, "brpe", fall))
brpe_winter <- terra::rast(x = paste0(marine_bird_dir, "brpe", winter))



#####################################

#### Taxonomic Groups:
####   1.) Scoter species (surf scoter, white-winged scoter, black scoter) -- SUSC, WWSC, BLSC:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_winter_predicted_density.tif
scot_spring <- terra::rast(x = paste0(marine_bird_dir, "scot", spring))
scot_summer <- terra::rast(x = paste0(marine_bird_dir, "scot", summer))
scot_fall <- terra::rast(x = paste0(marine_bird_dir, "scot", fall))
scot_winter <- terra::rast(x = paste0(marine_bird_dir, "scot", winter))

####   2.) Western (Aechmophorus occidentalis) / Clark's Grebe (Aechmophorus clarkii) -- WEGR, CLGR: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_winter_predicted_density.tif
wegr_clgr_spring <- terra::rast(x = paste0(marine_bird_dir, "wegr-clgr", spring))
wegr_clgr_fall <- terra::rast(x = paste0(marine_bird_dir, "wegr-clgr", fall))
wegr_clgr_winter <- terra::rast(x = paste0(marine_bird_dir, "wegr-clgr", winter))

####   3.) Phalarope species (red-necked phalarope, red phalarope) -- RNPH, REPH: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_winter_predicted_density.tif
phal_spring <- terra::rast(x = paste0(marine_bird_dir, "phal", spring))
phal_summer <- terra::rast(x = paste0(marine_bird_dir, "phal", summer))
phal_fall <- terra::rast(x = paste0(marine_bird_dir, "phal", fall))
phal_winter <- terra::rast(x = paste0(marine_bird_dir, "phal", winter))

####   4.) Jaeger species (pomarine jaeger, parasitic jaeger, long-tailed jaeger) -- POJA, PAJA, LTJA:
#####     Pomarine jaeger (Stercorarius pomarinus) -- POJA:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_winter_predicted_density.tif
poja_spring <- terra::rast(x = paste0(marine_bird_dir, "poja", spring))
poja_summer <- terra::rast(x = paste0(marine_bird_dir, "poja", summer))
poja_fall <- terra::rast(x = paste0(marine_bird_dir, "poja", fall))
poja_winter <- terra::rast(x = paste0(marine_bird_dir, "poja", winter))

#####     Parasitic jaeger (Stercorarius parasiticus) & long-tailed jaeger (Stercorarius longicaudus) -- PAJA, LTJA:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_fall_predicted_density.tif
paja_ltja_spring <- terra::rast(x = paste0(marine_bird_dir, "paja-ltja", spring))
paja_ltja_summer <- terra::rast(x = paste0(marine_bird_dir, "paja-ltja", summer))
paja_ltja_fall <- terra::rast(x = paste0(marine_bird_dir, "paja-ltja", fall))

####   5.) Scripps's (Synthliboramphus scrippsi) / Guadeloupe (Synthliboramphus hypoleucus) / Craveri's murrelet (Synthliboramphus craveri) -- SCMU, GUMU, CRMU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCMU-GUMU-CRMU_spring_predicted_density.tif
scmu_gumu_crmu_spring <- terra::rast(x = paste0(marine_bird_dir, "scmu-gumu-crmu", spring))

####   6.) Herring (Larus smithsonianus) / Iceland gull (Larus glaucoides) -- HERG, ICGU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_winter_predicted_density.tif
herg_icgu_spring <- terra::rast(x = paste0(marine_bird_dir, "herg-icgu", spring))
herg_icgu_summer <- terra::rast(x = paste0(marine_bird_dir, "herg-icgu", summer))
herg_icgu_fall <- terra::rast(x = paste0(marine_bird_dir, "herg-icgu", fall))
herg_icgu_winter <- terra::rast(x = paste0(marine_bird_dir, "herg-icgu", winter))

####   7.) Western (Larus occidentalis) / glaucous-winged gull (Larus glaucescens) -- WEGU, GWGU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_winter_predicted_density.tif
wegu_wgwh_gwgu_spring <- terra::rast(x = paste0(marine_bird_dir, "wegu-wgwh-gwgu", spring))
wegu_wgwh_gwgu_summer <- terra::rast(x = paste0(marine_bird_dir, "wegu-wgwh-gwgu", summer))
wegu_wgwh_gwgu_fall <- terra::rast(x = paste0(marine_bird_dir, "wegu-wgwh-gwgu", fall))
wegu_wgwh_gwgu_winter <- terra::rast(x = paste0(marine_bird_dir, "wegu-wgwh-gwgu", winter))

####   8.) Common (Sterna hirundo) / Arctic tern (Sterna paradisaea) -- COTE, ARTE: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_fall_predicted_density.tif
cote_arte_spring <- terra::rast(x = paste0(marine_bird_dir, "cote-arte", spring))
cote_arte_summer <- terra::rast(x = paste0(marine_bird_dir, "cote-arte", summer))
cote_arte_fall <- terra::rast(x = paste0(marine_bird_dir, "cote-arte", fall))

####   9.) Royal (Thalasseus maximus) / elegant tern (Thalasseus elegans) -- ROYT, ELTE: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_fall_predicted_density.tif
royt_elte_spring <- terra::rast(x = paste0(marine_bird_dir, "royt-elte", spring))
royt_elte_summer <- terra::rast(x = paste0(marine_bird_dir, "royt-elte", summer))
royt_elte_fall <- terra::rast(x = paste0(marine_bird_dir, "royt-elte", fall))

####   10.) Loon species (Gavia spp.) -- RTLO, PALO, COLO, YBLO:
####      ***NOTE: paper references 4 species: red-throated (Gavia stellata), Pacific (Gavia pacifica), common (Gavia immer), and yellow-billed (Gavia adamsii)
####      ***WARNING: Cornell does not record the yellow-billed loon as part of the Gaviidae family: https://www.allaboutbirds.org/guide/browse/taxonomy/Gaviidae
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_winter_predicted_density.tif
loon_spring <- terra::rast(x = paste0(marine_bird_dir, "loon", spring))
loon_summer <- terra::rast(x = paste0(marine_bird_dir, "loon", summer))
loon_fall <- terra::rast(x = paste0(marine_bird_dir, "loon", fall))
loon_winter <- terra::rast(x = paste0(marine_bird_dir, "loon", winter))

####   11.) Short-tailed (Ardenna tenuirostris) / sooty (Ardenna grisea) / flesh-footed shearwater (Ardenna carneipes) -- STTS, SOSH, FFSH:
####      ***WARNING: the short-tailed shearwater has different codes between the data download (STTS) and the paper (SRTS)
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_winter_predicted_density.tif
stts_sosh_ffsh_spring <- terra::rast(x = paste0(marine_bird_dir, "stts-sosh-ffsh", spring))
stts_sosh_ffsh_summer <- terra::rast(x = paste0(marine_bird_dir, "stts-sosh-ffsh", summer))
stts_sosh_ffsh_fall <- terra::rast(x = paste0(marine_bird_dir, "stts-sosh-ffsh", fall))
stts_sosh_ffsh_winter <- terra::rast(x = paste0(marine_bird_dir, "stts-sosh-ffsh", winter))

####   12.) Cormorant (Phalacrocorax spp.):
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CORM_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CORM_winter_predicted_density.tif
corm_fall <- terra::rast(x = paste0(marine_bird_dir, "corm", fall))
corm_winter <- terra::rast(x = paste0(marine_bird_dir, "corm", winter))

#####################################
#####################################

# Sum seasonal data to create annual density dataset
## Species
### Common murre (Uria aalge) -- COMU
#### Combine annual density
comu_annual <- c(comu_spring,
                 comu_summer,
                 comu_fall,
                 comu_winter) %>%
  terra::app(sum, na.rm = T)
  
#### Calculate annual total density
comu_annual_total <- terra::global(x = comu_annual, fun = "sum", na.rm = T)
comu_annual_sum <- comu_annual_total$sum

#### Normalize densities
##### Divide the annual density data by the total summed data
comu_annual_norm <- comu_annual[] / comu_annual_sum
##### Set the normalized data back to the original annual density dataset
comu_normalize <- terra::setValues(comu_annual, comu_annual_norm)

##### Convert normalized data to polygon data with 2km cell size
comu_polygon <- terra::as.polygons(x = comu_normalize,
                                   dissolve = F,
                                   # NA values are not removed
                                   na.rm = F) %>%
  # change to simple feature (sf)
  sf::st_as_sf() %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>%
  # simplify column name to "density" (this is the first column of the object, thus the colnames(.)[1] means take the first column name from the comu_normalize object)
  dplyr::rename(density = colnames(.)[1]) %>%
  # create field for species
  dplyr::mutate(species_code = "COMU") %>%
  dplyr::relocate(species_code,
                  .before = density) %>%
  # join the vulnerability species data
  dplyr::left_join(x = .,
                   y = marine_bird_vulnerability,
                   by = "species_code") %>%
  dplyr::relocate(density,
                  .after = dis_be_value)



############################################
### 14. Marine bird species density maps ###
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
marine_bird_dir <- "data/a_raw_data/marine_bird/0242882/1.1/data/0-data/model_output_predictions"
intermediate_dir <- "data/b_intermediate_data"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Submodel directory
natural_resources_submodel <- "data/c_submodel_data/natural_resources_submodel.gpkg"

#### Intermediate directories
##### Marine bird species geopackage (vector data)
marine_bird_gpkg <- "data/b_intermediate_data/marine_bird_species.gpkg"

##### Deep sea coral and sponge habitat directory (raster data)
dir.create(paste0(intermediate_dir, "/",
                  "marine_bird_species"))

marine_bird_species_dir <- "data/b_intermediate_data/marine_bird_species"

#####################################
#####################################

list.files(marine_bird_dir)

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

## Marine bird species (source: https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/242882.1.1.tar.gz)
### ***Note: NOAA conducted an analysis on marine bird species that combined data from Leirness et al. 2021)
###          https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0242882. For more specific downloads
###          on just the model output predictors: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/

### ***Note: NOAA analyzed and quantified the data for 30 species and 12 taxonomic groups
###          Densities were summarized by season  (fall, spring, summer, winter) -- though not all seasons exist

#### Species
####   1.) South polar skua (Stercorarius maccormicki):
####      Fall (https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SPSK_fall_predicted_density.tif)
####   2.) Common murre (Uria aalge): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_winter_predicted_density.tif
####   3.) Pigeon guillemot (Cepphus columba):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PIGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PIGU_summer_predicted_density.tif
####   4.) Marbled murrelet (Brachyramphus marmoratus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MAMU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MAMU_summer_predicted_density.tif
####   5.) Ancient murrelet (Synthliboramphus antiquus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ANMU_spring_predicted_density.tif
####   6.) Cassin's auklet (Ptychoramphus aleuticus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_winter_predicted_density.tif
####   7.) Rhinoceros auklet (Cerorhinca monocerata):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_winter_predicted_density.tif
####   8.) Tufted puffin (Fratercula cirrhata):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/TUPU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/TUPU_summer_predicted_density.tif
####   9.) Black-legged kittiwake (Rissa tridactyla):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_winter_predicted_density.tif
####   10.) Sabine's gull (Xema sabini):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_fall_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_summer_predicted_density.tif
####   11.) Bonaparte's gull (Chroicocephalus philadelphia):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_winter_predicted_density.tif
####   12.) Heermann's gull (Larus heermanni):
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_fall_predicted_density.tif  
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_winter_predicted_density.tif
####   13.) California gull (Larus californicus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_winter_predicted_density.tif
####   14.) Caspian tern (Hydroprogne caspia):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CATE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CATE_summer_predicted_density.tif
####   15.) Laysan albratross (Phoebastria immutabilis):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LAAL_spring_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LAAL_winter_predicted_density.tif
####   16.) Black-footed ablatross (Phoebastria nigripes):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_winter_predicted_density.tif
####   17.) Fork-tailed storm-petrel (Hydrobates furcatus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_winter_predicted_density.tif
####   18.) Leach's storm-petrel (Hydrobates leucorhous):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_winter_predicted_density.tif
####   19.) Ashy storm-petrel (Hydrobates homochroa):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_fall_predicted_density.tif
####   20.) Black storm-petrel (Hydrobates melania):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_fall_predicted_density.tif
####   21.) Northern fulmar (Fulmarus glacialis):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_winter_predicted_density.tif
####   22.) Murphy's petrel (Pterodroma ultima):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MUPE_spring_predicted_density.tif
####   23.) Cook's petrel (Pterodroma cookii):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_fall_predicted_density.tif
####   24.) Buller's shearwater (Ardenna bulleri):
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BULS_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BULS_fall_predicted_density.tif
####   25.) Pink-footed shearwater (Ardenna creatopus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_fall_predicted_density.tif
####   26.) Black-vented shearwater (Puffinus opisthomelas):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_winter_predicted_density.tif
####   27.) Brandt's cormorant (Phalacrocorax penicillatus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRAC_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRAC_summer_predicted_density.tif
####   28.) Pelagic cormorant (Phalacrocorax pelagicus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PECO_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PECO_summer_predicted_density.tif
####   29.) Double-crested cormorant (Phalacrocorax auritus):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/DCCO_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/DCCO_summer_predicted_density.tif
####   30.) Brown pelican (Pelecanus occidentalis):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_winter_predicted_density.tif


#### Taxonomic Groups:
####   1.) Scoter species (surf scoter, white-winged scoter, black scoter):
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_winter_predicted_density.tif
####   2.) Western (Aechmophorus occidentalis) / Clark's Grebe (Aechmophorus clarkii): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_winter_predicted_density.tif
####   3.) Phalarope species (red-necked phalarope, red phalarope): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_winter_predicted_density.tif
####   4.) Jaeger species (pomarine jaeger, parasitic jaeger, long-tailed jaeger):
#####     Pomarine jaeger (Stercorarius pomarinus)
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_winter_predicted_density.tif

#####     Parasitic jaeger (Stercorarius parasiticus) & long-tailed jaeger (Stercorarius longicaudus)
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_fall_predicted_density.tif
####   5.) Scripps's (Synthliboramphus scrippsi) / Guadeloupe (Synthliboramphus hypoleucus) / Craveri's murrelet (Synthliboramphus craveri): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCMU-GUMU-CRMU_spring_predicted_density.tif
####   6.) Herring (Larus smithsonianus) / Iceland gull (): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_winter_predicted_density.tif
####   7.) Western () / glauguous-winged gull (): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_winter_predicted_density.tif
####   8.) Common (Sterna hirundo) / Arctic tern (Sterna paradisaea): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_fall_predicted_density.tif
####   9.) Royal (Thalasseus maximus) / elegant tern (Thalasseus elegans): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_fall_predicted_density.tif
####   10.) Loon species (Gavia spp.): 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_winter_predicted_density.tif
#####     Pomarine jaeger
####      Spring: 
####      Summer: 
####      Fall: 
####      Winter: 

#####     Parasitic jaeger & long-tailed jaeger
####      Spring: 
####      Summer: 
####      Fall: 
####      Winter: 
####   5.) Scripps's () / Guadeloupe () / Craveri's murrelet (): 
####      Spring: 
####   6.) Herring () / Iceland gull (): 
####      Spring: 
####      Summer: 
####      Fall: 
####      Winter: 
####   7.) Westerm () / glauguous-winged gull (): 
####      Spring: 
####      Summer: 
####      Fall: 
####      Winter: 
####   8.) Common () / Arctic tern (): 
####      Spring: 
####      Summer: 
####      Fall: 
####   9.) Royal () / elegant tern (): 
####      Spring: 
####      Summer: 
####      Fall: 
####   10.) Loon species: 
####      Spring: 
####      Summer: 
####      Fall: 
####      Winter: 
####   11.) Short-tailed () / sooty () / flesh-footed shearwater (): 
####      Spring: 
####      Summer: 
####      Fall: 
####      Winter: 
####   12.) Cormorant (): 
####      Fall: 
####      Winter: 

#####################################
#####################################




### Kelsey et al. (2018) (source: https://www.sciencedirect.com/science/article/abs/pii/S0301479718309228)
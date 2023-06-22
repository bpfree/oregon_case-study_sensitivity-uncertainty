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

# Functions
## Prepping annual datasets
annual_species_density_function <- function(directory, spp_code){
  species_season_files <- list.files(directory,
                                     pattern = "_density.tif")
  
  species_list <- unique(sapply(strsplit(species_season_files, "_"), function(x) x[1]))
  
  species_code <- spp_code
  
  target_species_seasons <- list.files(directory, pattern = paste0(spp_code, "_"))
  
  annual_species_list <- target_species_seasons[sapply(strsplit(target_species_seasons, "_"), function(x) x[4] == "density.tif")]
  
  #####################################
  
  # create a summed raster across the seasons
  species_annual_raster <- sum(terra::rast(file.path(directory, annual_species_list)),
                               na.rm = T)
  
  #####################################
  
  # calculate annual total density
  species_annual_total <- terra::global(x = species_annual_raster, fun = "sum", na.rm = T)
  species_annual_sum <- species_annual_total$sum
  
  # Normalize densities
  ## Divide the annual density data by the total summed data
  species_annual_norm <- species_annual_raster[] / species_annual_sum
  ## Set the normalized data back to the original annual density dataset
  species_normalize <- terra::setValues(species_annual_raster, species_annual_norm)
}

## Prepping annual datasets
cormorant_species_function <- function(directory, spp_code){
  species_season_files <- list.files(directory,
                                     pattern = "_density.tif")
  
  species_list <- unique(sapply(strsplit(species_season_files, "_"), function(x) x[1]))
  
  species_code <- spp_code
  
  target_species_seasons <- list.files(directory, pattern = paste0(spp_code, "_"))
  
  annual_species_list <- target_species_seasons[sapply(strsplit(target_species_seasons, "_"), function(x) x[4] == "density.tif")]

  #####################################
  
  # create a summed raster across the seasons
  species_annual_raster <- sum(terra::rast(file.path(directory, annual_species_list)),
                               na.rm = T)
}

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

## Marine bird vulnerability indices
marine_bird_vulnerability <- base::readRDS(file = paste(marine_bird_species_dir, "species_vulnerabilities_normalized.rds", sep = "/"))

files_list <- list.files(marine_bird_dir, pattern = "_density.tif")
species_list <- unique(sapply(strsplit(files_list, "_"), function(x) x[1]))

modeled_species <- species_list[!(species_list %in% c("POJA", "PAJA-LTJA", "RTLO", "COLO", "BRAC", "PECO", "DCCO"))]

weights <- marine_bird_vulnerability %>%
  dplyr::filter(species_code %in% modeled_species) %>%
  dplyr::select(species_code,
                overall_vul) %>%
  # arrange alphabetically by species code
  dplyr::arrange(species_code)

# convert weights to list
species_weights <- weights[[2]]

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
spsk <- annual_species_density_function(directory = marine_bird_dir, spp_code = "SPSK")

####   2.) Common murre (Uria aalge) -- COMU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COMU_winter_predicted_density.tif
comu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "COMU")

####   3.) Pigeon guillemot (Cepphus columba) -- PIGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PIGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PIGU_summer_predicted_density.tif
pigu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "PIGU")

####   4.) Marbled murrelet (Brachyramphus marmoratus) -- MAMU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MAMU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MAMU_summer_predicted_density.tif
mamu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "MAMU")

####   5.) Ancient murrelet (Synthliboramphus antiquus) -- ANMU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ANMU_spring_predicted_density.tif
anmu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "ANMU")

####   6.) Cassin's auklet (Ptychoramphus aleuticus) -- CAAU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAAU_winter_predicted_density.tif
caau <- annual_species_density_function(directory = marine_bird_dir, spp_code = "CAAU")

####   7.) Rhinoceros auklet (Cerorhinca monocerata) -- RHAU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/RHAU_winter_predicted_density.tif
rhau <- annual_species_density_function(directory = marine_bird_dir, spp_code = "RHAU")

####   8.) Tufted puffin (Fratercula cirrhata) -- TUPU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/TUPU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/TUPU_summer_predicted_density.tif
tupu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "TUPU")

####   9.) Black-legged kittiwake (Rissa tridactyla) -- BLKI:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLKI_winter_predicted_density.tif
blki <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BLKI")

####   10.) Sabine's gull (Xema sabini) -- SAGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SAGU_fall_predicted_density.tif
sagu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "SAGU")

####   11.) Bonaparte's gull (Chroicocephalus philadelphia) -- BOGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BOGU_winter_predicted_density.tif
bogu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BOGU")

####   12.) Heermann's gull (Larus heermanni) -- HEEG:
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_fall_predicted_density.tif  
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HEEG_winter_predicted_density.tif
heeg <- annual_species_density_function(directory = marine_bird_dir, spp_code = "HEEG")

####   13.) California gull (Larus californicus) -- CAGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CAGU_winter_predicted_density.tif
cagu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "CAGU")

####   14.) Caspian tern (Hydroprogne caspia) -- CATE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CATE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CATE_summer_predicted_density.tif
cate <- annual_species_density_function(directory = marine_bird_dir, spp_code = "CATE")

####   15.) Laysan albratross (Phoebastria immutabilis) -- LAAL:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LAAL_spring_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LAAL_winter_predicted_density.tif
laal <- annual_species_density_function(directory = marine_bird_dir, spp_code = "LAAL")

####   16.) Black-footed alaatross (Phoebastria nigripes) -- BFAL:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BFAL_winter_predicted_density.tif
bfal <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BFAL")

####   17.) Fork-tailed storm-petrel (Hydrobates furcatus) -- FTSP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/FTSP_winter_predicted_density.tif
ftsp <- annual_species_density_function(directory = marine_bird_dir, spp_code = "FTSP")

####   18.) Leach's storm-petrel (Hydrobates leucorhous) -- LESP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LESP_winter_predicted_density.tif
lesp <- annual_species_density_function(directory = marine_bird_dir, spp_code = "LESP")

####   19.) Ashy storm-petrel (Hydrobates homochroa) -- ASSP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ASSP_fall_predicted_density.tif
assp <- annual_species_density_function(directory = marine_bird_dir, spp_code = "ASSP")

####   20.) Black storm-petrel (Hydrobates melania) -- BLSP:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BLSP_fall_predicted_density.tif
blsp <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BLSP")

####   21.) Northern fulmar (Fulmarus glacialis) -- NOFU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/NOFU_winter_predicted_density.tif
nofu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "NOFU")

####   22.) Murphy's petrel (Pterodroma ultima) -- MUPE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/MUPE_spring_predicted_density.tif
mupe <- annual_species_density_function(directory = marine_bird_dir, spp_code = "MUPE")

####   23.) Cook's petrel (Pterodroma cookii) -- COPE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COPE_fall_predicted_density.tif
cope <- annual_species_density_function(directory = marine_bird_dir, spp_code = "COPE")

####   24.) Buller's shearwater (Ardenna bulleri) -- BULS:
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BULS_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BULS_fall_predicted_density.tif
buls <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BULS")

####   25.) Pink-footed shearwater (Ardenna creatopus) -- PFSH:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PFSH_fall_predicted_density.tif
pfsh <- annual_species_density_function(directory = marine_bird_dir, spp_code = "PFSH")

####   26.) Black-vented shearwater (Puffinus opisthomelas) -- BVSH:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BVSH_winter_predicted_density.tif
bvsh <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BVSH")

####   30.) Brown pelican (Pelecanus occidentalis) -- BRPE:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRPE_winter_predicted_density.tif
brpe <- annual_species_density_function(directory = marine_bird_dir, spp_code = "BRPE")

#####################################

#### Taxonomic Groups:
####   1.) Scoter species (surf scoter, white-winged scoter, black scoter) -- SUSC, WWSC, BLSC:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCOT_winter_predicted_density.tif
scot <- annual_species_density_function(directory = marine_bird_dir, spp_code = "SCOT")

####   2.) Western (Aechmophorus occidentalis) / Clark's Grebe (Aechmophorus clarkii) -- WEGR, CLGR: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_spring_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGR-CLGR_winter_predicted_density.tif
wegr_clgr <- annual_species_density_function(directory = marine_bird_dir, spp_code = "WEGR-CLGR")

####   3.) Phalarope species (red-necked phalarope, red phalarope) -- RNPH, REPH: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PHAL_winter_predicted_density.tif
phal <- annual_species_density_function(directory = marine_bird_dir, spp_code = "PHAL")

####   4.) Jaeger species:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/JAEG_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/JAEG_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/JAEG_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/JAEG_winter_predicted_density.tif
jaeg <- annual_species_density_function(directory = marine_bird_dir, spp_code = "JAEG")

###### ***NOTE: There are other jaeger data based on species
#####     Pomarine jaeger (Stercorarius pomarinus) -- POJA:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/POJA_winter_predicted_density.tif

#####     Parasitic jaeger (Stercorarius parasiticus) & long-tailed jaeger (Stercorarius longicaudus) -- PAJA, LTJA:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PAJA-LTJA_fall_predicted_density.tif

####   5.) Scripps's (Synthliboramphus scrippsi) / Guadeloupe (Synthliboramphus hypoleucus) / Craveri's murrelet (Synthliboramphus craveri) -- SCMU, GUMU, CRMU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/SCMU-GUMU-CRMU_spring_predicted_density.tif
scmu_gumu_crmu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "SCMU-GUMU-CRMU")

####   6.) Herring (Larus smithsonianus) / Iceland gull (Larus glaucoides) -- HERG, ICGU: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/HERG-ICGU_winter_predicted_density.tif
herg_icgu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "HERG-ICGU")

####   7.) Western (Larus occidentalis) / glaucous-winged gull (Larus glaucescens) -- WEGU, GWGU:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/WEGU-WGWH-GWGU_winter_predicted_density.tif
wegu_wgwh_gwgu <- annual_species_density_function(directory = marine_bird_dir, spp_code = "WEGU-WGWH-GWGU")

####   8.) Common (Sterna hirundo) / Arctic tern (Sterna paradisaea) -- COTE, ARTE: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/COTE-ARTE_fall_predicted_density.tif
cote_arte <- annual_species_density_function(directory = marine_bird_dir, spp_code = "COTE-ARTE")

####   9.) Royal (Thalasseus maximus) / elegant tern (Thalasseus elegans) -- ROYT, ELTE: 
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/ROYT-ELTE_fall_predicted_density.tif
royt_elte <- annual_species_density_function(directory = marine_bird_dir, spp_code = "ROYT-ELTE")

####   10.) Loon species (Gavia spp.) -- RTLO, PALO, COLO, YBLO:
####      ***NOTE: paper references 4 species: red-throated (Gavia stellata), Pacific (Gavia pacifica), common (Gavia immer), and yellow-billed (Gavia adamsii)
####      ***WARNING: Cornell does not record the yellow-billed loon as part of the Gaviidae family: https://www.allaboutbirds.org/guide/browse/taxonomy/Gaviidae
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/LOON_winter_predicted_density.tif
loon <- annual_species_density_function(directory = marine_bird_dir, spp_code = "LOON")

####   11.) Short-tailed (Ardenna tenuirostris) / sooty (Ardenna grisea) / flesh-footed shearwater (Ardenna carneipes) -- STTS, SOSH, FFSH:
####      ***WARNING: the short-tailed shearwater has different codes between the data download (STTS) and the paper (SRTS)
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_summer_predicted_density.tif
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/STTS-SOSH-FFSH_winter_predicted_density.tif
stts_sosh_ffsh <- annual_species_density_function(directory = marine_bird_dir, spp_code = "STTS-SOSH-FFSH")

#####################################

## Cormorants
####   27.) Brandt's cormorant (Phalacrocorax penicillatus) -- BRAC:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRAC_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/BRAC_summer_predicted_density.tif
brac <- cormorant_species_function(directory = marine_bird_dir, spp_code = "BRAC")

####   28.) Pelagic cormorant (Phalacrocorax pelagicus) -- PECO:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PECO_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/PECO_summer_predicted_density.tif
peco <- cormorant_species_function(directory = marine_bird_dir, spp_code = "PECO")

####   29.) Double-crested cormorant (Phalacrocorax auritus) -- DCCO:
####      Spring: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/DCCO_spring_predicted_density.tif
####      Summer: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/DCCO_summer_predicted_density.tif
dcco <- cormorant_species_function(directory = marine_bird_dir, spp_code = "DCCO")

####   12.) Cormorant (Phalacrocorax spp.):
####      Fall: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CORM_fall_predicted_density.tif
####      Winter: https://www.nodc.noaa.gov/archive/arc0193/0242882/1.1/data/0-data/model_output_predictions/CORM_winter_predicted_density.tif
corm <- cormorant_species_function(directory = marine_bird_dir, spp_code = "CORM")


corm_annual_raster <- c(brac,
                        peco,
                        dcco,
                        corm) %>%
  terra::app(sum, na.rm = T)

# calculate annual total density
corm_annual_total <- terra::global(x = corm_annual_raster, fun = "sum", na.rm = T)
corm_annual_sum <- corm_annual_total$sum

# Normalize densities
## Divide the annual density data by the total summed data
corm_annual_norm <- corm_annual_raster[] / corm_annual_sum
## Set the normalized data back to the original annual density dataset
corm_normalize <- terra::setValues(corm_annual_raster, corm_annual_norm)

#####################################
#####################################

pacific_species <- terra::rast(list(anmu,
                                    assp,
                                    bfal,
                                    blki,
                                    blsp,
                                    bogu,
                                    brpe,
                                    buls,
                                    bvsh,
                                    caau,
                                    cagu,
                                    cate,
                                    comu,
                                    cope,
                                    corm_normalize,
                                    cote_arte,
                                    ftsp,
                                    heeg,
                                    herg_icgu,
                                    jaeg,
                                    laal,
                                    lesp,
                                    loon,
                                    mamu,
                                    mupe,
                                    nofu,
                                    pfsh,
                                    phal,
                                    pigu,
                                    rhau,
                                    royt_elte,
                                    sagu,
                                    scmu_gumu_crmu,
                                    scot,
                                    spsk,
                                    stts_sosh_ffsh,
                                    tupu,
                                    wegr_clgr,
                                    wegu_wgwh_gwgu))

#####################################
#####################################

marine_bird_wt_mean <- terra::weighted.mean(pacific_species, w = species_weights)

terra::minmax(marine_bird_wt_mean)






# buffer call areas by 5 km
wpa <- sf::st_transform(oregon_call_areas, sf::st_crs(marine_bird_wt_mean))
wpa <- sf::st_buffer(wpa, 5000)

# crop and mask to call areas
wpa <- terra::project(terra::vect(wpa), marine_bird_wt_mean)
rsv_o <- terra::crop(marine_bird_wt_mean, wpa)
rsv_o <- terra::mask(rsv_o, wpa)

# check plot
terra::plot(rsv_o, col = colorRampPalette(c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"))(256), colNA = "gray50", main = "All species")

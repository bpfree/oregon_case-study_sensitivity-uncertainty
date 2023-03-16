########################
### 1. Download Data ###
########################

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

# Commentary on R and code formulation:
## ***Note: If not familiar with dplyr notation
## dplyr is within the tidyverse and can use %>%
## to "pipe" a process, allowing for fluidity
## Can learn more here: https://style.tidyverse.org/pipes.html

## Another common coding notation used is "::"
## For instance, you may encounter it as dplyr::filter()
## This means "use the filter function from the dplyr package"
## Notation is used given sometimes different packages have
## the same function name, so it helps code to tell which
## package to use for that particular function.
## The notation is continued even when a function name is
## unique to a particular package so it is obvious which
## package is used

#####################################
#####################################

# Create function that will pull data from publicly available websites
## This allows for the analyis to have the most current data; for some
## of the datasets are updated with periodical frequency (e.g., every 
## month) or when needed. Additionally, this keeps consistency with
## naming of files and datasets.
### The function downloads the desired data from the URL provided and
### then unzips the data for use

data_download_function <- function(download_list, data_dir){
  
  # loop function across all datasets
  for(i in 1:length(download_list)){
    
    # designate the URL that the data are hosted on
    url <- download_list[i]
    
    # file will become last part of the URL, so will be the data for download
    file <- basename(url)
    
    # Download the data
    if (!file.exists(file)) {
      options(timeout=1000)
      # download the file from the URL
      download.file(url = url,
                    # place the downloaded file in the data directory
                    destfile = file.path(data_dir, file),
                    mode="wb")
    }
    
    # Unzip the file if the data are compressed as .zip
    ## Examine if the filename contains the pattern ".zip"
    ### grepl returns a logic statement when pattern ".zip" is met in the file
    if (grepl(".zip", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- sub(".zip", "", file)
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    dir <- file.path(data_dir, new_dir_name)
    
  }
  
}

#####################################
#####################################

# Set directories
## Define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data"

#####################################
#####################################

# Download data

## Download the BOEM Wind Call Areas
### BOEM Source (geodatabase): https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data
### An online download link: https://www.boem.gov/renewable-energy/boem-renewable-energy-geodatabase
### Metadata: https://metadata.boem.gov/geospatial/boem_renewable_lease_areas.xml
#### ***Note: Data are also accessible for download on MarineCadastre (under "Active Renewable Energy Leases")
#### This provides a useable URL for R: https://www.boem.gov/BOEM-Renewable-Energy-Geodatabase.zip
boem_wind_area_data <- "https://www.boem.gov/BOEM-Renewable-Energy-Geodatabase.zip"

#####################################

## Land boundary data
### Shoreline data (USGS Global Islands Vector dataset)
### Global Island Explorer has detailed information about the dataset and can link to the paper detailing the methods (https://rmgsc.cr.usgs.gov/gie/)
### For visual inspection, navigate here: https://rmgsc.cr.usgs.gov/gie/gie.shtml
### For manual downloading the data (https://rmgsc.cr.usgs.gov/outgoing/ecosystems/Global/USGSEsriWCMC_GlobalIslands_v3.mpk):
#### 1.) Navigate to this page: https://rmgsc.cr.usgs.gov/outgoing/ecosystems/Global/
#### 2.) Click the USGSEsriWCMC_GlobalIslands_v3.mpk (***Note: there may be a newer version so name could be different -- Version 3 as of `October 11 October 2022)
#### 3.) Where file is located change .mpk (Esri mappackage) to .zip
#### 4.) Unzip file
#### 5.) Navigate to and copy geodatabase (as of 11 October 2021, v10 and v108 had no differences in their data)
#### 6.) Paste to data dictionary
usgs_data <- "https://rmgsc.cr.usgs.gov/outgoing/ecosystems/Global/USGSEsriWCMC_GlobalIslands_v3.mpk"

#####################################

## Seagrass data
### TPWD Seagrass (map viewer: https://tpwd.maps.arcgis.com/apps/webappviewer/index.html?id=af7ff35381144b97b38fe553f2e7b562)
#### Source: https://tpwd.texas.gov/gis/resources/tpwd-seagrass.zip)
seagrass_tpwd_data <-"https://tpwd.texas.gov/gis/resources/tpwd-seagrass.zip"

### NOAA US + Territories (source: ftp://ftp.coast.noaa.gov/pub/MSP/Seagrasses.zip)
#### Alternative data accessed here: https://marinecadastre.gov/downloads/data/mc/Seagrass.zip (has 3 fewer features -- none in study area)
seagrass_noaa_data <- "ftp://ftp.coast.noaa.gov/pub/MSP/Seagrasses.zip"

### Gulfwide Seagrass (source: https://www.ncei.noaa.gov/waf/data-atlas-waf/biotic/documents/GulfwideSAV.zip)
seagrass_ncei_data <- "https://www.ncei.noaa.gov/waf/data-atlas-waf/biotic/documents/GulfwideSAV.zip"

#####################################

## Oyster data
### Texas Parks & Wildlife (https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/)
#### Copano Bay survey (2015) (source: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/copano-bay-habitat-classification-shapefiles.zip)
#### Metadata: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/copano-bay-metadata.docx
copano_bay_data <- "https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/copano-bay-habitat-classification-shapefiles.zip"

### Espiritu Santo survey (source: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/espiritu-santo-oyster-habitat-shapefiles.zip)
#### Metadata: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/espiritu-santo-metadata.docx
#### ***Note: None of these data fall within the present study area
espiritu_santo_data <- "https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/espiritu-santo-oyster-habitat-shapefiles.zip"

### Galveston Bay survey (2004 - 2015) (source: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/galveston-bay-habitat-classification-shapefiles.zip)
#### Metadata: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/galveston-bay-metadata.zip
galveston_bay_data <- "https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/galveston-bay-habitat-classification-shapefiles.zip"

### Lavaca and Tres Palacios survey (source: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/lavaca-tres-palacios-habitat-shapefile.zip)
#### Metadata: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/lavaca-tres-palacios-metadata.zip
#### ***Note: None of these data fall within the present study area
lavaca_tres_palacios_data <- "https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/lavaca-tres-palacios-habitat-shapefile.zip"

### West Galveston Bay survey (source: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/west-galveston-bay-habitat-classification-shapefiles.zip)
#### Metadata: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/west-galveston-bay-metadata.zip
west_galveston_bay_data <- "https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/west-galveston-bay-habitat-classification-shapefiles.zip"

### Oyster restoration sites (source: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/tpwd-oyster-restoration-sites.zip)
oyster_restoration_data <- "https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/resources/tpwd-oyster-restoration-sites.zip"

### Gulf of Mexico Atlas (2011) -- American oyster (source: https://www.sciencebase.gov/catalog/item/594830afe4b062508e344418)
#### Alternative Gulf of Mexico source: https://www.ncei.noaa.gov/waf/data-atlas-waf/living-marine/documents/Oysters_GOM_2011.zip
#### Gulf of Mexico Metadata: https://www.ncei.noaa.gov/maps/gulf-data-atlas//Metadata/ISO/Oysters_GOM_2011.html
##### Texas only: https://www.ncei.noaa.gov/waf/data-atlas-waf/living-marine/documents/Oysters_TX_2011.zip
##### Texas metadata: https://www.ncei.noaa.gov/maps/gulf-data-atlas//Metadata/ISO/Oysters_TX_2011.html
##### ***Note: To download Texas only data:
#####    1.) Visit: https://www.ncei.noaa.gov/maps/gulf-data-atlas/atlas.htm
#####    2.) Click "Living Marine Resources" tab to display dropdown
#####    3.) Navigate to "Invertebrates"
#####    4.) Click "1. Eastern Oyster"
#####    5.) On right panel, click "More Information"
#####    6.) Click on folder icon next to "Data Download / Access Links"
#####    7.) Click blue "Download" hyperlinked text for Texas
gom_atlas_data <- "https://www.ncei.noaa.gov/waf/data-atlas-waf/living-marine/documents/Oysters_GOM_2011.zip"

#####################################

## Bathymetry data (source: https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol5.nc)
### For more United States coverage and spatial resolution information, visit: https://www.ngdc.noaa.gov/mgg/coastal/crm.html
bathymetry_data <- "https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol5.nc"

#####################################

## Load AIS data (2019)
### Transit counts: https://marinecadastre.gov/downloads/data/ais/ais2019/AISVesselTransitCounts2019.zip
### Metadata: https://www.fisheries.noaa.gov/inport/item/61037
ais_transit2019_data <- "https://marinecadastre.gov/downloads/data/ais/ais2019/AISVesselTransitCounts2019.zip"

#####################################

## Vessel tracks (other): https://marinecadastre.gov/downloads/data/ais/ais2019/AISVesselTracks2019.zip
### Metadata: https://www.fisheries.noaa.gov/inport/item/59927
ais_tracks2019_data <- "https://marinecadastre.gov/downloads/data/ais/ais2019/AISVesselTracks2019.zip"

#####################################

## Shipping lanes data (source: http://encdirect.noaa.gov/theme_layers/data/shipping_lanes/shippinglanes.zip)
### These are federal water shipping lanes
federal_shipping_lanes_data <- "http://encdirect.noaa.gov/theme_layers/data/shipping_lanes/shippinglanes.zip"

#####################################

## Conservation areas data
### Texas Wildlife Management Areas (source: https://tpwd.texas.gov/gis/resources/wildlife-management-areas.zip)
wma_texas_data <- "https://tpwd.texas.gov/gis/resources/wildlife-management-areas.zip"

### Texas State Parks (source: https://tpwd.texas.gov/gis/resources/tpwd-statepark-boundaries.zip)
state_parks_texas_data <- "https://tpwd.texas.gov/gis/resources/tpwd-statepark-boundaries.zip"

### Flower Garden Banks National Marine Sanctuary (source: https://sanctuaries.noaa.gov/media/gis/fgbnms_py.zip)
#### Metadata: https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/gis/fgbnms_py.pdf
flower_garden_nms_data <- "https://sanctuaries.noaa.gov/media/gis/fgbnms_py.zip"

#####################################

## NOAA lightering zones data (source: https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/66149
### For more detailed information on lightering zones and coordinates for polygons: https://www.govinfo.gov/content/pkg/CFR-2018-title33-vol2/xml/CFR-2018-title33-vol2-part156.xml#seqnum156.300
lightering_zone_data <- "https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip"

#####################################

## Artificial reefs (source: https://tpwd.texas.gov/gis/resources/tpwd-artificial-reef-data.zip)
### Data are from Texas Parks and Wildlife
artificial_reefs_data <- "https://tpwd.texas.gov/gis/resources/tpwd-artificial-reef-data.zip"

#####################################

## Unexploded ordnance data
### Unexploded ordnance (source: https://marinecadastre.gov/downloads/data/mc/UnexplodedOrdnance.zip)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/66208
unexploded_ordnance_data <- "https://marinecadastre.gov/downloads/data/mc/UnexplodedOrdnance.zip"

#####################################

## BOEM active oil and gas lease data (source: https://www.data.boem.gov/Main/Mapping.aspx#ascii)
### Geodatabase download link: https://www.data.boem.gov/Mapping/Files/ActiveLeasePolygons.gdb.zip
### Shapefile download link: https://www.data.boem.gov/Mapping/Files/actlease.zip
#### Metadata: https://www.data.boem.gov/Mapping/Files/actlease_meta.html
##### ***Note: data are updated each month near the first of the month
boem_active_oil_gas_data <- "https://www.data.boem.gov/Mapping/Files/ActiveLeasePolygons.gdb.zip"

#####################################

## Significant sediment data
### ***Note: There are a few ways to obtain the data
#### 1.) BOEM Marine Mineral Mapping and Data page: https://www.boem.gov/marine-minerals/marine-minerals-mapping-and-data
##### Here you can download the geodatabase or shapefile for the Gulf of Mexico or the Atlantic
######   - Geodatabase download link: https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_fgdb.zip
######   - Shapefile download link: https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_shp.zip
######   - Metadata: https://mmis.doi.gov/boemmmis/metadata/PlanningAndAdministration/GOMSigSedBlocks.xml
significant_sediment_data <- "https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_fgdb.zip"

#####################################

## Anchorage area data (source: https://marinecadastre.gov/downloads/data/mc/Anchorage.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/48849
anchorage_area_data <- "https://marinecadastre.gov/downloads/data/mc/Anchorage.zip"

#####################################

## BOEM drilling platforms data (source: https://www.data.boem.gov/Mapping/Files/Platforms.gdb.zip)
### Metadata: https://www.data.boem.gov/Mapping/Files/platform_meta.html
#### Note: These data came from the mapping page: https://www.data.boem.gov/Main/Mapping.aspx#ascii
#### Note: These data are different from the platform query page that BOEM has: https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx
#### That query page seems to mirror the data that BSEE also has
drilling_platform_data <- "https://www.data.boem.gov/Mapping/Files/Platforms.gdb.zip"

#####################################

## Submarine cables data
### Submarine cable area (source: https://marinecadastre.gov/downloads/data/mc/SubmarineCableArea.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/66190
submarine_cable_areas_data <- "https://marinecadastre.gov/downloads/data/mc/SubmarineCableArea.zip"

### NOAA Charted submarine cable (source: https://marinecadastre.gov/downloads/data/mc/SubmarineCable.zip)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/57238
submarine_cable_noaa_data <- "https://marinecadastre.gov/downloads/data/mc/SubmarineCable.zip"

#####################################

## Aids to navigation data (source: https://marinecadastre.gov/downloads/data/mc/AtoN.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/56120
aids_navigation_data <- "https://marinecadastre.gov/downloads/data/mc/AtoN.zip"

#####################################

## Pipelines data
### Source: https://www.data.boem.gov/Mapping/Files/Pipelines.gdb.zip
### Options page: https://www.data.boem.gov/Main/Mapping.aspx#ascii
### Metadata: https://www.data.boem.gov/Mapping/Files/ppl_arcs_meta.html
pipeline_data <- "https://www.data.boem.gov/Mapping/Files/Pipelines.gdb.zip"

#####################################

## Coral habitat area of particular concern (source: http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip)
## Habitat Areas of Particular Concern are a subset of Essential Fish Habitat
## Older areas can have regulations or no regulations; newer ones under Amendment 9 might have proposed regulations or none proposed
## Amendment 9 went into effect on November 16, 2020 (read more about amendment here: https://www.govinfo.gov/content/pkg/FR-2020-10-16/pdf/2020-21298.pdf)
coral_hapc_data <- "http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip"

#####################################

## BOEM Lease Blocks data (source: https://www.data.boem.gov/Mapping/Files/Blocks.gdb.zip) -- shapefile format is also available
### Metadata: https://www.data.boem.gov/Mapping/Files/blocks_meta.html
boem_lease_blocks_data <- "https://www.data.boem.gov/Mapping/Files/Blocks.gdb.zip"

#####################################

## NREL Net Value -- 2015 data (source: https://data.nrel.gov/system/files/67/170514_OSW%20cost%20analysis_output%20file%20%281%29.xlsx)
### Data page: https://data.nrel.gov/submissions/67, report: https://www.nrel.gov/docs/fy17osti/67675.pdf
### ***Note: Data come as an Excel spreadsheet. To use data, delete tabs expect 2015 (COD) and save as CSV
### ***Note: The data were renamed as nrel_offshore_wind-net_value2015.csv, but any name can be used
nrel_net_value_data <- "https://data.nrel.gov/system/files/67/170514_OSW%20cost%20analysis_output%20file%20%281%29.xlsx"

#####################################
#####################################

# Download list
download_list <- c(
  # BOEM wind energy areas
  boem_wind_area_data,
  
  # USGS global islands
  usgs_data,
  
  # seagrass
  seagrass_tpwd_data,
  seagrass_noaa_data,
  seagrass_ncei_data,
  
  # oyster
  copano_bay_data,
  espiritu_santo_data,
  galveston_bay_data,
  lavaca_tres_palacios_data,
  west_galveston_bay_data,
  oyster_restoration_data,
  gom_atlas_data,
  
  # bathymetry
  bathymetry_data,
  
  # vessel traffic
  ais_transit2019_data,
  ais_tracks2019_data,
  
  # shipping lanes
  federal_shipping_lanes_data,
  
  # conservation areas
  wma_texas_data,
  state_parks_texas_data,
  flower_garden_nms_data,
  
  # lightering zones
  lightering_zone_data,
  
  # artificial reefs
  artificial_reefs_data,
  
  # unexploded ordnances
  unexploded_ordnance_data,
  
  # BOEM active lease areas
  boem_active_oil_gas_data,
  
  # BOEM significant sediments
  significant_sediment_data,
  
  # anchorage areas
  anchorage_area_data,
  
  # BOEM drilling platforms
  drilling_platform_data,
  
  # submarine cable
  submarine_cable_areas_data,
  submarine_cable_noaa_data,
  
  # aids to navigation
  aids_navigation_data,
  
  # pipelines
  pipeline_data,
  
  # coral habitats
  coral_hapc_data,
  
  # BOEM lease blocks
  boem_lease_blocks_data,
  
  # NREL net value
  nrel_net_value_data
)

data_download_function(download_list, data_dir)

#####################################
#####################################

# Rename datasets
list.files(data_dir)

## NREL (2015) net value data
file.rename(from = file.path(data_dir, "170514_OSW%20cost%20analysis_output%20file%20%281%29.xlsx"),
            to = file.path(data_dir, "nrel_2015_net_value.xlsx"))

## USGS Global Island dataset
### ***Note: will need to manually unzip the data
file.rename(from = file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3.mpk"),
            to = file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3.zip"))

list.files(data_dir)
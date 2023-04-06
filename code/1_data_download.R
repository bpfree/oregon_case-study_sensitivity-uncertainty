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
               stringr,
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
      options(timeout=10000)
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
    
    # Unzip the file if the data are compressed as tar.gz
    ## Examine if the filename contains the pattern "tar.gz"
    ### grepl returns a logic statement when pattern ".zip" is met in the file
    if (grepl("tar.gz", file)){
      
      # grab text before "tar.gz" and keep only text before that
      new_dir_name <- sub("tar.gz", "", file)
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      # unzip the file
      untar(tarfile = file.path(data_dir, file),
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

## Oregon state boundary (source: http://navigator.state.or.us/sdl/data/shapefile/k24/or_state_boundary.zip)
### Metadata: https://spatialdata.oregonexplorer.info/osdl-geoportal/rest/document?id=%7BCF375EB0-FF70-42D9-9DAE-A17A776821A8%7D
### Geospatial library: https://spatialdata.oregonexplorer.info/geoportal/details;id=cf375eb0ff7042d99daea17a776821a8
oregon_boundary_data <- "http://navigator.state.or.us/sdl/data/shapefile/k24/or_state_boundary.zip"

#####################################

## Bathymetry data for Northwest Pacific (source: https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol8.nc)
### For more United States coverage and spatial resolution information, visit: https://www.ngdc.noaa.gov/mgg/coastal/crm.html
bathymetry_data <- "https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol8.nc"

#####################################

## Military operating area boundaries (source: https://marinecadastre.gov/downloads/data/mc/MilitaryOperatingAreaBoundary.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/55364
military_operating_data <- "https://marinecadastre.gov/downloads/data/mc/MilitaryOperatingAreaBoundary.zip"

#####################################

## Pacific Coast Port Access Route Study (source: https://navcen.uscg.gov/sites/default/files/pdf/PARS/PAC_PARS_22/Draft%20PAC-PARS.pdf)
### Federal Register: https://www.federalregister.gov/documents/2022/08/26/2022-18453/port-access-route-study-the-pacific-coast-from-washington-to-california
pacpars_data <- "https://navcen.uscg.gov/sites/default/files/pdf/PARS/PAC_PARS_22/Draft%20PAC-PARS.pdf"

#####################################

## Submarine cables data
### Submarine cable area (source: https://marinecadastre.gov/downloads/data/mc/SubmarineCableArea.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/66190
submarine_cable_areas_data <- "https://marinecadastre.gov/downloads/data/mc/SubmarineCableArea.zip"

#####################################

### NOAA Charted submarine cable (source: https://marinecadastre.gov/downloads/data/mc/SubmarineCable.zip)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/57238
submarine_cable_noaa_data <- "https://marinecadastre.gov/downloads/data/mc/SubmarineCable.zip"

#####################################

## Biologically Important Areas I (https://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip)
### ***Note: currently the BIA I data are informing blue whale foraging; in the future the BIA II data may affect the analysis
### ***Alternative data (same footprint, different data column): https://coast.noaa.gov/digitalcoast/data/biologicallyimportantareas.html
### BIA I Map (2015): https://cetsound.noaa.gov/biologically-important-area-map
### BIA II Paper (2023): https://www.frontiersin.org/articles/10.3389/fmars.2023.1081893/full
### BIA II Map (2023 Update): https://experience.arcgis.com/experience/51a9e25c75a1470386827439a918e056
bia_data <- "https://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip"

#####################################

## Essential fish habitat conservation areas (source: https://media.fisheries.noaa.gov/2021-02/EFH_HAPC_EFHCA_shapefiles_AM19-2006%2BAM28-2020.zip)
### Text: https://www.ecfr.gov/current/title-50/chapter-VI/part-660/subpart-C/section-660.76
efhca_data <- "https://media.fisheries.noaa.gov/2021-02/EFH_HAPC_EFHCA_shapefiles_AM19-2006%2BAM28-2020.zip"

#####################################

## Deep-sea coral and sponge habitat (source: https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/276883.1.1.tar.gz)
### NCEI: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0276883
### Metadata: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0276883;view=iso
#### Link for habitat suitability data only: https://www.nodc.noaa.gov/archive/arc0211/0276883/1.1/data/0-data/NCCOS_USWestCoast_DSC_Models_2016_2020/Coral_Richness.zip
coral_sponge_habitat <- "https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/276883.1.1.tar.gz"

#####################################

## Methane bubble streams (Merle et al. 2021) (source: https://www.pmel.noaa.gov/eoi/Cascadia/Supplemental-Tables-US-only-revised-dec30-2020.xlsx)
### ***Note: data come from the supplemental table
### Paper: https://www.frontiersin.org/articles/10.3389/feart.2021.531714/full
methane_bubble_merle <- "https://www.pmel.noaa.gov/eoi/Cascadia/Supplemental-Tables-US-only-revised-dec30-2020.xlsx"

#####################################

## Methane bubble streams (Reidel et al. 2018) (source: https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-018-05736-x/MediaObjects/41467_2018_5736_MOESM4_ESM.xlsx)
### ***Note: data come from the supplementary data 2
### Paper: https://www.nature.com/articles/s41467-018-05736-x
methane_bubble_reidel <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-018-05736-x/MediaObjects/41467_2018_5736_MOESM4_ESM.xlsx"

#####################################

## Methane bubble streams (Johnson et al. 2015) (source: https://agupubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2F2015GC005955&file=ggge20859-sup-0001-2015GC005955-SupInfo.docx)
### ***Note: data come from the supporting information document (see S2 and S3)
### ***Note: S3 does not contain any sites that fall within Oregon call areas
### Paper: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015GC005955
methane_bubble_johnson <- "https://agupubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2F2015GC005955&file=ggge20859-sup-0001-2015GC005955-SupInfo.docx"

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

## Aids to navigation data (source: https://marinecadastre.gov/downloads/data/mc/AtoN.zip)
### Metadata: https://www.fisheries.noaa.gov/inport/item/56120
aids_navigation_data <- "https://marinecadastre.gov/downloads/data/mc/AtoN.zip"

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
  
  # bathymetry
  bathymetry_data,
  
  # Oregon state boundary
  oregon_boundary_data,
  
  # military operating areas
  military_operating_data,
  
  # PACPARS
  pacpars_data,
  
  # submarine cable
  submarine_cable_areas_data,
  submarine_cable_noaa_data,
  
  # BIAs
  bia_data,
  
  # essential fish habitat conservation areas
  efhca_data,
  
  # deep-sea coral and sponge habitat
  coral_sponge_habitat,
  
  # methane bubble streams
  methane_bubble_merle,
  methane_bubble_reidel,
  methane_bubble_johnson,
  
  # vessel traffic
  ais_transit2019_data,
  ais_tracks2019_data,

  
  # aids to navigation
  aids_navigation_data,
  
  # NREL net value
  nrel_net_value_data
)

data_download_function(download_list, data_dir)

#####################################
#####################################

# Rename datasets
files <- list.files(data_dir)
files

## Deep-sea coral and sponge habitat
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "276883.1.1.")),
            to = file.path(data_dir, "deep_sea_coral_sponge_habitat"))

## Methane bubble streams (Merle et al. 2021)
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "Supplemental-Tables")),
            to = file.path(data_dir, "methane_bubble_streams_merle.csv"))

## Methane bubble streams (Reidel et al. 2018)
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "MOESM4_ESM")),
            to = file.path(data_dir, "methane_bubble_streams_reidel.csv"))

## Methane bubble streams (Johnson et al. 2015)
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "SupInfo.docx")),
            to = file.path(data_dir, "methane_bubble_streams_johnson.docx"))

## NREL (2015) net value data
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "170514_OSW")),
            to = file.path(data_dir, "nrel_2015_net_value.xlsx"))

## PACPARS report
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "PAC-PARS")),
            to = file.path(data_dir, "pacpars_draft_report.pdf"))

list.files(data_dir)

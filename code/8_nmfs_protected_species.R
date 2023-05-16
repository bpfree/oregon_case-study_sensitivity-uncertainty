#################################
### 8. NMFS Protected Species ###
#################################

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
nmfs_esa_habitat_dir <- "data/a_raw_data/NMFS_ESA_Critical_Habitat_20221017.gdb"
nmfs_efhca_dir <- "data/a_raw_data/EFH_HAPC_EFHCA_shapefiles_AM19-2006%2BAM28-2020/EFHCA shapefile_2020"
bathymetry_gpkg <- "data/b_intermediate_data/bathymetry/bathymetry.gpkg"

bathymetric_contour_dir <- "data/a_raw_data/BathymetricContour/BathymetryContours.gdb"

study_area_gpkg <- "data/b_intermediate_data/oregon_study_area.gpkg"
wind_area_gpkg <- "data/b_intermediate_data/oregon_wind_area.gpkg"

### Output directories
#### Analysis directories
fisheries_submodel <- "data/c_submodel_data/fisheries_submodel.gpkg"

#### Protected species directory
intermediate_dir <- "data/b_intermediate_data"
dir.create(paste0(intermediate_dir, "/",
                  "protected_species"))

protected_species_dir <- "data/b_intermediate_data/protected_species"

conservation_areas_gpkg <- paste(protected_species_dir, "conservation_areas.gpkg", sep = "/")
exclusion_areas_gpkg <- paste(protected_species_dir, "exclusion_areas.gpkg", sep = "/")
exclusion_miscellaneous_gpkg <- paste(protected_species_dir, "exclusion_miscellaneous.gpkg", sep = "/")
protected_species_gpkg <- paste(protected_species_dir, "protected_species.gpkg", sep = "/")

#####################################
#####################################

sf::st_layers(dsn = nmfs_esa_habitat_dir,
              do_count = TRUE)

# Layers of interest:
## 28 -- SeaTurtleLeatherback_20120126
## 34 -- WhaleHumpback_CentralAmericaDPS_20210421
## 35 -- WhaleHumpback_MexicoDPS_20210421
## 37 -- WhaleKiller_SouthernResidentDPS_20210802

#####################################
#####################################

# Clean and dissolve protected species
## This function will take the imported data and reduce it down to a single
## feature after removing exlcusion areas and limiting to Oregon call areas
protected_species_function <- function(species, exclusion_areas, call_areas){
  protected_species <- species %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")  %>% # EPSG 26910 (https://epsg.io/26910) %>%
    # remove any exclusion areas for the particular species of concern
    rmapshaper::ms_erase(target = .,
                         # remove any areas that are located within those exclusion areas
                         erase = exclusion_areas) %>%
    # obtain only species areas in the study area
    rmapshaper::ms_clip(target = .,
                        # clip area is the Oregon call areas
                        clip = call_areas) %>%
    # create field called "layer" and fill with "submarine cables" for summary
    dplyr::mutate(layer = "protected species") %>%
    # select the fields of interest
    dplyr::select(layer)
  return(protected_species)
}

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

## NMFS Essential Fish Habitat Conservation Areas (source: https://media.fisheries.noaa.gov/2021-02/EFH_HAPC_EFHCA_shapefiles_AM19-2006%2BAM28-2020.zip)
### Text: https://www.ecfr.gov/current/title-50/chapter-VI/part-660/subpart-C/section-660.76
nmfs_efhca_data <- sf::st_read(dsn = nmfs_efhca_dir, layer = "EFH_ConsArea_polygons_v20191107") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910")

#####################################

## Exclusion areas by bathymetric contour
### Load bathymetric contour
bathymetric_contour <- sf::st_read(dsn = bathymetry_gpkg, layer = "oregon_contours_50")

#####################################

## Species
### Leatherback sea turtle (species specific data: https://www.fisheries.noaa.gov/resource/map/leatherback-turtle-critical-habitat-map-and-gis-data)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65327)
#### Code of regulations:  https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.207
leatherback_areas <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "SeaTurtleLeatherback_20120126")

### Humpback whale (Central America DPS) (species specific data: https://noaa.maps.arcgis.com/home/item.html?id=9426731f9651463bac4eb9cfba6574bd)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65375
#### Code of regulations: https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.227
humpback_central_america_dps_areas <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "WhaleHumpback_MexicoDPS_20210421")

### Humpback whale (Mexico DPS) (species specific data: https://noaa.maps.arcgis.com/home/item.html?id=9426731f9651463bac4eb9cfba6574bd)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65377
#### Code of regulations: https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.227
humpback_mexico_dps_areas <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "WhaleHumpback_CentralAmericaDPS_20210421")

### Killer whale (Southern Resident) (species specific data: https://noaa.maps.arcgis.com/home/item.html?id=fe1e0f42587f437b93e6168929f03593)
#### Metadata: https://www.fisheries.noaa.gov/inport/item/65409
#### Code of regulations: https://www.ecfr.gov/current/title-50/chapter-II/subchapter-C/part-226/section-226.206
killer_whale_areas <- sf::st_read(dsn = nmfs_esa_habitat_dir, layer = "WhaleKiller_SouthernResidentDPS_20210802")

#####################################
#####################################

# Create exclusion
## EFHCA
### NMFS requests that certain parts of EFHCAs be excluded from protected species areas (killer whales, humpback whale, blue whale)
#### See point 6 on page 4 of NMFS's comments to BOEM: https://www.regulations.gov/comment/BOEM-2022-0009-0178
#### EFHCAs of interest are: Heceta Bank, Deepwater off Coos Bay, and Rogue River Reef
efhca_exclusion <- nmfs_efhca_data %>%
  # obtain only EFHCAs of interest
  dplyr::filter(AREA_NAME %in% c("Heceta Bank", "Deepwater off Coos Bay", "Rogue River Reef")) %>%
  # only areas within call areas
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas) %>%
  # have dataframes match each other
  dplyr::mutate(layer = "protected species") %>%
  # select only needed fields
  dplyr::select(layer)

## Leatherback
### NMFS requests that certain parts of EFHCAs be excluded from leatherback sea turtle areas
#### See point 5 on page 4 of NMFS's comments to BOEM: https://www.regulations.gov/comment/BOEM-2022-0009-0178
##### EFHCA of interest is: Heceta Bank
leatherback_exclusion <- nmfs_efhca_data %>%
  # obtain only EFHCAs of interest
  dplyr::filter(AREA_NAME == "Heceta Bank") %>%
  # only areas within call areas
  rmapshaper::ms_clip(target = .,
                      clip = oregon_call_areas) %>%
  # have dataframes match each other
  dplyr::mutate(layer = "protected species") %>%
  # select only needed fields
  dplyr::select(layer)

#####################################

## Bathymetry exclusion
### ***Note: Comments from NOAA (NMFS, NCCOS, & IOOS) seeks to close any areas between 0 and 200m and 250m of depth
####         The 0-200m bathymetric contours were for the southern resident killer whale
####         The 0-250m bathymetric contours were for the humpback whale (Central America and Mexico)

##### ***WANRNING: In the initial analysis for offshore wind siting, the dataset reference was CAORWALL.
#####              However, the provenance of the dataset is unclear. It is possible that the original dataset came
#####              from this site: https://coastalmap.marine.usgs.gov/GISdata/regional/westcoast/bathymetry/caorwall.htm
#####              as it is referenced in this presentation: https://www.boem.gov/sites/default/files/documents/regions/pacific-ocs-region/environmental-science/west-coast-science-exchange-20191113.pdf
#####              Yet the mapping page is down for maintenance and no metadata exist for the page.
#####              For that dataset the -250m bathymetric contour was a 50-meter bathymetric buffer added to the -200m contour

### NOAA (NMFS, NCCOS, & IOOS) Comments: https://www.regulations.gov/comment/BOEM-2022-0009-0178 
### See points 1 and 2 at bottom of page 3

### -200m contour exclusion
#### Filter for -200m contour line
bath200 <- bathymetric_contour %>%
  # subset ony the -200m bathymetric contour
  dplyr::filter(level == -200)

#### Call areas by bathymetric contour
call_areas200 <- oregon_call_areas %>%
  # split the call area by the -200m bathymetry
  lwgeom::st_split(x = .,
                   # split is the -200m bathymetry line
                   y = bath200)

#### Get areas within call areas that are -200m or above
call_areas200_polygons <- call_areas200 %>%
  # extract only the polygons within the GeometryCollective
  sf::st_collection_extract(x = .,
                            type = "POLYGON") %>%
  # create area field (***note: this will be used to remove areas )
  dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
  # arrange areas by total area
  dplyr::arrange(area) %>%
  # take 4 smallest areas (these will be the top 4 after being arranged)
  # as the other two areas are past the 200m bathymetric contour
  dplyr::slice(1:4) %>%
  # have dataframes match each other
  dplyr::mutate(layer = "protected species") %>%
  dplyr::rename(geometry = geom) %>%
  # select only needed fields
  dplyr::select(layer)

### -250m contour exclusion
#### Filter for -250m contour line
bath250 <- bathymetric_contour %>%
  # subset only the -250m bathymetric contour
  dplyr::filter(level == -250)

#### Call areas by bathymetric contour
call_areas250 <- oregon_call_areas %>%
  # split the call area by the -250m bathymetry
  lwgeom::st_split(x = .,
                   # split is the -250 bathymetry line
                   y = bath250)

#### Get areas within call areas that are -250m or above
call_areas250_polygons <- call_areas250 %>%
  # extract only the polygons within the GeometryCollective
  sf::st_collection_extract(x = .,
                            type = "POLYGON") %>%
  # create area field (***note: this will be used to remove areas )
  dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
  # arrange areas by total area
  dplyr::arrange(area) %>%
  # create a new field called "fid" and fill with the row number
  dplyr::mutate(fid = row_number()) %>%
  # subset the areas that are -250m or above (3 areas)
  dplyr::filter(!fid %in% c(119, 118, 114)) %>%
  # have dataframes match each other
  dplyr::mutate(layer = "protected species") %>%
  # renanme field so it matches other data
  dplyr::rename(geometry = geom) %>%
  # select only needed fields
  dplyr::select(layer)

#####################################

## Brookings foraging exclusion
### ***Note: Comments from NOAA (NMFS, NCCOS, & IOOS) seeks to close any areas between 42-degrees and 10 minutes
###          and south to the Oregon-California border. These are closed as foraging grounds to the blue whale.
###          The comments reference 9 areas for blue whales for the biologically important areas. None of these
###          areas (https://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip) intersect the Oregon
###          call areas. Those data came from the 2015 dataset. In 2023, a new update was conducted
###          (https://www.frontiersin.org/articles/10.3389/fmars.2023.1081893/full). Presently (31 March 2023)
###          the data are limited to Alaska, yet it is expected data for the west coast will be produced.

### NOAA (NMFS, NCCOS, & IOOS) Comments: https://www.regulations.gov/comment/BOEM-2022-0009-0178

#### Oregon and California border is detailed as the 42nd parallel in both of their constitutions.
##### Oregon (Article XVI - Boundaries): https://www.oregonlegislature.gov/bills_laws/Pages/OrConst.aspx
##### Oregon's admittance to the US (see Admission of State--Boundaries) : https://sos.oregon.gov/blue-book/Pages/facts/history/congress-act.aspx
##### California (see Article 3 Section 170): https://leginfo.legislature.ca.gov/faces/codes_displayText.xhtml?division=1.&chapter=1.&lawCode=GOV&title=1.&article=3.

#### California-Oregon boundary line
cal_ore_boundary <- rbind(c("point",-124.1,39),
                          c("point",-124.3,39),
                          c("point",-124.6,39),
                          c("point",-124.3,39),
                          c("point",-124.9,39),
                          c("point",-125.2,39),
                          c("point",-125.5,39),
                          c("point",-125.8,39))

#### Oregon Call area line
oregon_4210_line <- rbind(c("point",-125.8,42.1666667),
                          c("point",-125.5,42.1666667),
                          c("point",-125.2,42.1666667),
                          c("point",-124.9,42.1666667),
                          c("point",-124.3,42.1666667),
                          c("point",-124.6,42.1666667),
                          c("point",-124.3,42.1666667),
                          c("point",-124.1,42.1666667))

#### Foraging box around Brookings southern portion
brookings_foraging_box <- rbind(cal_ore_boundary,
                                oregon_4210_line) %>%
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
  # convert to polygon simple feature
  sf::st_cast("POLYGON") %>%
  # convert back to sf
  sf::st_as_sf()

#### Brookings foraging area
brookings_foraging_area <- oregon_call_areas %>%
  sf::st_make_valid() %>%
  rmapshaper::ms_clip(clip = brookings_foraging_box) %>%
  dplyr::mutate(layer = "protected species") %>%
  dplyr::select(layer)

plot(brookings_foraging_area)

#####################################

## Species exclusions
### Killer whale exclusion areas (areas between 0-200m + EFHCAs)
killer_whale_exclusion <- call_areas200_polygons %>%
  # EFHCAs
  rbind(efhca_exclusion)

### Humpback exclusion areas (Brookings foraging + areas between 0 - 250m + EFHCAs)
humpback_exclusion <- brookings_foraging_area %>%
  rbind(call_areas250_polygons,
        efhca_exclusion)

### Blue whale exclusion areas (Brookings foraging + EFHCAs)
blue_whale_exclusion <- brookings_foraging_area %>%
  rbind(efhca_exclusion)

#####################################

## Species data (source: https://noaa.maps.arcgis.com/home/item.html?id=f66c1e33f91d480db7d1b1c1336223c3)
### NMFS ESA Critical Habitat Mapper: https://noaa.maps.arcgis.com/apps/webappviewer/index.html?id=68d8df16b39c48fe9f60640692d0e318
### NOAA InPort: https://www.fisheries.noaa.gov/inport/item/65207

### For West Coast Specific the download is here: https://www.webapps.nwfsc.noaa.gov/portal7/home/item.html?id=40d9b14ae87e4023ae07361cf3067007
### West Coast Region Protected Resources App: https://www.webapps.nwfsc.noaa.gov/portal/apps/webappviewer/index.html?id=7514c715b8594944a6e468dd25aaacc9

### Leatherback
oregon_leatherback_areas <- leatherback_areas %>%
  protected_species_function(species = .,
                             exclusion_areas = leatherback_exclusion,
                             call_areas = oregon_call_areas)

### Humpback whale (Central America DPS)
oregon_humpback_central_america_dps_areas <- humpback_central_america_dps_areas %>%
  protected_species_function(species = .,
                             exclusion_areas = humpback_exclusion,
                             call_areas = oregon_call_areas)

### Humpback whale (Mexico DPS)
oregon_humpback_mexico_dps_areas <- humpback_mexico_dps_areas %>%
  protected_species_function(species = .,
                             exclusion_areas = humpback_exclusion,
                             call_areas = oregon_call_areas)

### Killer whale
oregon_killer_whale_areas <- killer_whale_areas %>%
  protected_species_function(species = .,
                             exclusion_areas = killer_whale_exclusion,
                             call_areas = oregon_call_areas)

### Blue whale
oregon_blue_whale_areas <- oregon_call_areas %>%
  rmapshaper::ms_erase(target = .,
                       erase = blue_whale_exclusion)

#####################################

# Oregon hex grid by species
oregon_hex_leatherback <- oregon_hex[oregon_leatherback_areas, ] %>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_leatherback_areas,
              join = st_intersects)

oregon_hex_humpback_ca_dps <- oregon_hex[oregon_humpback_central_america_dps_areas, ]%>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_humpback_central_america_dps_areas,
              join = st_intersects)

 oregon_hex_humpback_mexico_dps <- oregon_hex[oregon_humpback_mexico_dps_areas, ]%>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_humpback_mexico_dps_areas,
              join = st_intersects)

oregon_hex_killer_whale <- oregon_hex[oregon_killer_whale_areas, ]%>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_killer_whale_areas,
              join = st_intersects)

oregon_hex_blue_whale <- oregon_hex[oregon_blue_whale_areas, ]%>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_blue_whale_areas,
              join = st_intersects)

#####################################
#####################################

# Export data
## Fisheries submodel
sf::st_write(obj = oregon_hex_leatherback, dsn = fisheries_submodel, layer = "oregon_hex_leatherback", append = F)
sf::st_write(obj = oregon_hex_humpback_ca_dps, dsn = fisheries_submodel, layer = "oregon_hex_humpback_ca_dps", append = F)
sf::st_write(obj = oregon_hex_humpback_mexico_dps, dsn = fisheries_submodel, layer = "oregon_hex_humpback_mexico_dps", append = F)
sf::st_write(obj = oregon_hex_killer_whale, dsn = fisheries_submodel, layer = "oregon_hex_killer_whale", append = F)
sf::st_write(obj = oregon_hex_blue_whale, dsn = fisheries_submodel, layer = "oregon_hex_blue_whale", append = F)

## Geopackages
### Conservation areas
sf::st_write(obj = nmfs_efhca_data, dsn = conservation_areas_gpkg, layer = "nmfs_efhca", append = F)

### Exclusion areas
sf::st_write(obj = efhca_exclusion, dsn = exclusion_areas_gpkg, layer = "efhca_exclusion", append = F)
sf::st_write(obj = leatherback_exclusion, dsn = exclusion_areas_gpkg, layer = "leatherback_exclusion", append = F)
sf::st_write(obj = call_areas200_polygons, dsn = exclusion_areas_gpkg, layer = "call_area_200m_exclusion", append = F)
sf::st_write(obj = call_areas250_polygons, dsn = exclusion_areas_gpkg, layer = "call_area_250m_exclusion", append = F)
sf::st_write(obj = brookings_foraging_area, dsn = exclusion_areas_gpkg, layer = "brookings_foraging_exclusion", append = F)
sf::st_write(obj = killer_whale_exclusion, dsn = exclusion_areas_gpkg, layer = "killer_whale_exclusion", append = F)
sf::st_write(obj = humpback_exclusion, dsn = exclusion_areas_gpkg, layer = "humpback_exclusion", append = F)
sf::st_write(obj = blue_whale_exclusion, dsn = exclusion_areas_gpkg, layer = "blue_whale_exclusion", append = F)

### Miscellaneous exclusion
saveRDS(cal_ore_boundary, paste(protected_species_dir, "cal_ore_boundary.RDS", sep = "/"))
saveRDS(oregon_4210_line, paste(protected_species_dir, "oregon_4210_line.RDS", sep = "/"))
sf::st_write(obj = brookings_foraging_box, dsn = exclusion_miscellaneous_gpkg, layer = "brookings_foraging_box_exclusion", append = F)

### Bathymetric contours
sf::st_write(obj = bath200, dsn = protected_species_gpkg, layer = "bathymetric_countor_200m", append = F)
sf::st_write(obj = bath250, dsn = protected_species_gpkg, layer = "bathymetric_countor_250m", append = F)

### Protected species areas
sf::st_write(obj = oregon_leatherback_areas, dsn = protected_species_gpkg, layer = "oregon_leatherback_areas", append = F)
sf::st_write(obj = oregon_humpback_central_america_dps_areas, dsn = protected_species_gpkg, layer = "oregon_humpback_ca_dps_areas", append = F)
sf::st_write(obj = oregon_humpback_mexico_dps_areas, dsn = protected_species_gpkg, layer = "oregon_humpback_mexico_dps_areas", append = F)
sf::st_write(obj = oregon_killer_whale_areas, dsn = protected_species_gpkg, layer = "oregon_killer_whale", append = F)
sf::st_write(obj = oregon_blue_whale_areas, dsn = protected_species_gpkg, layer = "oregon_blue_whale_areas", append = F)

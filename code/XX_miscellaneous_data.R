## Miscellaneous code

# Code 4 - PACPARS

d13_coastal_table <- data.frame(table[[2]]) %>%
  # clean table
  janitor::row_to_names(row_number = 2,
                        # remove row after setting as column names
                        remove_row = TRUE,
                        # remove above rows
                        remove_rows_above = TRUE) %>%
  # clean column names
  janitor::clean_names() %>%
  # place NA into cell in row 5, column 6
  dplyr::na_if(.[5,6]) %>%
  # place NA into cell in row 6, column 6
  dplyr::na_if(.[6,6]) %>%
  # split out column to be two columns
  tidyr::separate(latitude_longitude, into=c("latitude2", "longitude2"), sep=" ", remove=T, convert = T) %>%
  # split out column to be two columns
  tidyr::separate(latitude_longitude_2, into=c("latitude3", "longitude3"), sep=" ", remove=T, convert = T) %>%
  # split the data across columns by 3 (point, lat, lon)
  split.default(., (seq_along(.)-1) %/% 3) %>%
  # recombine so in long format (all features share same fields)
  dplyr::bind_rows()

#####################################

### Code 8 -- Protected Species

### ***Note: Comments from NOAA (NMFS, NCCOS, & IOOS) seeks to close any areas between 0 and 200m and 250m of depth
### The 0-200m bathymetric contours were for the southern resident killer whale
### The 0-250m bathymetric contours were for the humpback whale (Central America and Mexico)
### The 250m bathymetric contour was a 200m contour with a 50m bathymetric buffer added

### NOAA (NMFS, NCCOS, & IOOS) Comments: https://www.regulations.gov/comment/BOEM-2022-0009-0178 
### See points 1 and 2 at bottom of page 3

#### Bathymetric contours (isobath contour) (source: https://marinecadastre.gov/downloads/data/mc/BathymetricContour.zip)
##### Metadata: https://www.fisheries.noaa.gov/inport/item/54364
bathymetric_contour <- sf::st_read(dsn = bathymetric_contour_dir, layer = "BathymetryContours") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") # EPSG 26910 (https://epsg.io/26910)

##### 200m bathymetric contour
bath200 <- bathymetric_contour %>%
  # obtain only bathymetric contours that are at 200m
  dplyr::filter(Contour == -200)

##### Call areas split into polygons by the 200m contour
call_areas200 <- oregon_call_areas %>%
  # split the call area by the 200m bathymetry
  lwgeom::st_split(x = .,
                   y = bath200)

##### Areas that fall in the range of 0 and 200m of depth
call_areas200_polygons <- call_areas200 %>%
  # extract only the polygons within the GeometryCollective
  sf::st_collection_extract(x = .,
                            type = "POLYGON") %>%
  # create area field (***note: this will be used to remove areas )
  dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
  # arrange areas by total area
  dplyr::arrange(area) %>%
  # take 4 smallest areas (these will be the top 4 after being arranged)
  # as the other two areas are past the 250m bathymetric contour
  dplyr::slice(1:4) %>%
  # have dataframes match each other
  dplyr::mutate(layer = "protected species") %>%
  dplyr::rename(geometry = geom) %>%
  # select only needed fields
  dplyr::select(layer)


#### 250m bathymetric contour
bath250 <- bathymetric_contour %>%
  # obtain only bathymetric contours that are at 250m
  dplyr::filter(Contour == -250)

#### Call areas split into polygons by the 250m contour
call_areas250 <- oregon_call_areas %>%
  # split the call area by the 250m bathymetry
  lwgeom::st_split(x = .,
                   y = bath250)

#### Areas that fall in the range of 0 and 250m of depth
call_areas250_polygons <- call_areas250 %>%
  # extract only the polygons within the GeometryCollective
  sf::st_collection_extract(x = .,
                            type = "POLYGON") %>%
  # create area field (***note: this will be used to remove areas )
  dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
  # arrange areas by total area
  dplyr::arrange(area) %>%
  # take 5 smallest areas (these will be the top 5 after being arranged)
  # as the other two areas are past the 250m bathymetric contour
  dplyr::slice(1:5) %>%
  # have dataframes match each other
  dplyr::mutate(layer = "protected species") %>%
  dplyr::rename(geometry = geom) %>%
  # select only needed fields
  dplyr::select(layer)

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
natural_resources_submodel <- "data/c_submodel_data/natural_resources_submodel.gpkg"

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
    # create field called "layer" and fill with "protected species"
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
## EFHCA exclusions
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
  # have data frames match each other
  dplyr::mutate(layer = "protected species")

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
  # areas between 0 and 250m
  rbind(call_areas250_polygons,
        # EFHCAs
        efhca_exclusion)

### Blue whale exclusion areas (Brookings foraging + EFHCAs)
blue_whale_exclusion <- brookings_foraging_area %>%
  # EFHCAs
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

oregon_hex_humpback_ca_dps <- oregon_hex[oregon_humpback_central_america_dps_areas, ] %>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_humpback_central_america_dps_areas,
              join = st_intersects) %>%
  # due to overlapping exclusion areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

oregon_hex_humpback_mexico_dps <- oregon_hex[oregon_humpback_mexico_dps_areas, ] %>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_humpback_mexico_dps_areas,
              join = st_intersects) %>%
  # due to overlapping exclusion areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

oregon_hex_killer_whale <- oregon_hex[oregon_killer_whale_areas, ] %>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_killer_whale_areas,
              join = st_intersects) %>%
  # due to overlapping exclusion areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

oregon_hex_blue_whale <- oregon_hex[oregon_blue_whale_areas, ] %>%
  # spatially join protected species values to Oregon hex cells 
  sf::st_join(x = .,
              y = oregon_blue_whale_areas,
              join = st_intersects)

#####################################
#####################################

# Export data
## Natural resources submodel
sf::st_write(obj = oregon_hex_leatherback, dsn = natural_resources_submodel, layer = "oregon_hex_leatherback", append = F)
sf::st_write(obj = oregon_hex_humpback_ca_dps, dsn = natural_resources_submodel, layer = "oregon_hex_humpback_ca_dps", append = F)
sf::st_write(obj = oregon_hex_humpback_mexico_dps, dsn = natural_resources_submodel, layer = "oregon_hex_humpback_mexico_dps", append = F)
sf::st_write(obj = oregon_hex_killer_whale, dsn = natural_resources_submodel, layer = "oregon_hex_killer_whale", append = F)
sf::st_write(obj = oregon_hex_blue_whale, dsn = natural_resources_submodel, layer = "oregon_hex_blue_whale", append = F)

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
sf::st_write(obj = oregon_killer_whale_areas, dsn = protected_species_gpkg, layer = "oregon_killer_whale_areas", append = F)
sf::st_write(obj = oregon_blue_whale_areas, dsn = protected_species_gpkg, layer = "oregon_blue_whale_areas", append = F)


#####################################

### Code 11

## High habitat suitability
high_habitat_polygon <- terra::as.polygons(x = high_habitat) %>%
  # change to simple feature (sf)
  sf::st_as_sf() %>%
  # cast to polygon
  sf::st_cast("POLYGON") %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>%
  # simplify column name to "richness" (this is the first column of the object, thus the colnames(.)[1] means take the first column name from the high_habitat object)
  dplyr::rename(richness = colnames(.)[1]) %>%
  # obtain areas with species (richness >= 1)
  dplyr::filter(richness >= 1) %>%
  # add 500-meter setback
  sf::st_buffer(dist = 500) %>%
  # obtain data within Oregon call areas
  rmapshaper::ms_clip(clip = oregon_call_areas)


## extend the call area by 500m then clip points, then add 500m setback to then reclip to orignal call areas

high_habitat_point <- terra::as.points(x = high_habitat) %>%
  # change to simple feature (sf)
  sf::st_as_sf() %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 10N) that will convert units from degrees to meters
  sf::st_transform("EPSG:26910") %>%
  # simplify column name to "richness" (this is the first column of the object, thus the colnames(.)[1] means take the first column name from the high_habitat object)
  dplyr::rename(richness = colnames(.)[1]) %>%
  # obtain areas with species (richness >= 1)
  dplyr::filter(richness >= 1) %>%
  # add 500-meter setback
  sf::st_buffer(dist = 500) %>%
  # cast to polygon
  sf::st_cast("MULTIPOLYGON")

#####################################

### Code 14 -- Bird vulnerabilities

# Create vulnerability metric tables
## Adams et al. (2017)
### Population vulnerability table (Table 4 -- page 12-13 of report / page 20-21 of PDF)
pv_table <- pv_csv %>%
  dplyr::select(Common_Name,
                AlphaCode,
                POP_Score,
                POP_Uncertainty_Value,
                AO_Score,
                CCSpop_Score,
                CCSpop_Uncertainty_Value,
                TS_Score,
                BR_Score,
                AS_Score,
                AS_Uncertainty_value,
                PV_Lower,
                PV_Best_Estimate,
                PV_Upper) %>%
  dplyr::rename(species_name = Common_Name,
                species_code = AlphaCode,
                pop = POP_Score,
                pop_uncertain = POP_Uncertainty_Value,
                ao = AO_Score,
                pocs_pop = CCSpop_Score,
                pocs_pop_uncertain = CCSpop_Uncertainty_Value,
                ts = TS_Score,
                br = BR_Score,
                as = AS_Score,
                as_uncertain = AS_Uncertainty_value,
                pv_lower = PV_Lower,
                pv_best_estimate = PV_Best_Estimate,
                pv_upper = PV_Upper) %>%
  # calculate minimum and maximum values for each metric
  ## ***Note: metric scores are kept within a 1 - 5 value range. Thus if a minimum after removing
  ##          uncertainty were less than one the floor is 1; in contrast the ceiling for the
  ##          maximum value (after adding the uncertainty) is 5
  dplyr::mutate(pop_min = ifelse(pop - pop_uncertain < 1, 1, pop - pop_uncertain),
                pop_max = ifelse(pop + pop_uncertain > 5, 5, pop + pop_uncertain),
                pocs_min = ifelse(pocs_pop - pocs_pop_uncertain < 1, 1, pocs_pop - pocs_pop_uncertain),
                pocs_max = ifelse(pocs_pop + pocs_pop_uncertain > 5, 5, pocs_pop + pocs_pop_uncertain),
                as_min = ifelse(as - as_uncertain < 1, 1, as - as_uncertain),
                as_max = ifelse(as + as_uncertain > 5, 5, as + as_uncertain)) %>%
  # recreate population vulnerability indexes
  dplyr::mutate(pv_lower_value = (pop_min) + (ao * (pocs_min)) + ts + (br * (as_min)),
                pv_be_value = (pop) + (ao * pocs_pop) + ts + (br * as),
                pv_upper_value = (pop_max) + (ao * (pocs_max)) + ts + (br * (as_max))) %>%
  # ***TEMPORARY: verification that values are the same
  dplyr::mutate(pv_lower_verification = pv_lower_value - pv_lower,
                pv_be_verification = pv_be_value - pv_best_estimate,
                pv_upper_verification = pv_upper_value - pv_upper)

### Collision vulnerability table (Table 5 -- page 24-26 of report / page 32-34 of PDF)
cv_table <- cv_csv %>%
  # select fields
  dplyr::select(Common_Name,
                AlphaCode,
                NFA_Score,
                NFA_Uncertainty_Value,
                DFA_Score,
                DFA_Uncertainty_Value,
                RSZt_Score,
                RSZt_Uncertainty_Value,
                MA_Collision_Score,
                MA_Uncertainty_Value,
                CV_Lower,
                CV_Best_Estimate,
                CV_Upper) %>%
  # rename fields
  dplyr::rename(species_name = Common_Name,
                species_code = AlphaCode,
                nfa = NFA_Score,
                nfa_uncertain = NFA_Uncertainty_Value,
                dfa = DFA_Score,
                dfa_uncertain = DFA_Uncertainty_Value,
                rszt = RSZt_Score,
                rszt_uncertain = RSZt_Uncertainty_Value,
                ma = MA_Collision_Score,
                ma_uncertain = MA_Uncertainty_Value,
                cv_lower = CV_Lower,
                cv_best_estimate = CV_Best_Estimate,
                cv_upper = CV_Upper) %>%
  # calculate minimum and maximum values for each metric
  ## ***Note: metric scores are kept within a 1 - 5 value range. Thus if a minimum after removing
  ##          uncertainty were less than one the floor is 1; in contrast the ceiling for the
  ##          maximum value (after adding the uncertainty) is 5
  dplyr::mutate(nfa_min = ifelse(nfa - nfa_uncertain < 1, 1, nfa - nfa_uncertain),
                nfa_max = ifelse(nfa + nfa_uncertain > 5, 5, nfa + nfa_uncertain),
                dfa_min = ifelse(dfa - dfa_uncertain < 1, 1, dfa - dfa_uncertain),
                dfa_max = ifelse(dfa + dfa_uncertain > 5, 5, dfa + dfa_uncertain),
                rszt_min = ifelse(rszt - rszt_uncertain < 1, 1, rszt - rszt_uncertain),
                rszt_max = ifelse(rszt + rszt_uncertain > 5, 5, rszt + rszt_uncertain),
                ma_min = ifelse(ma - ma_uncertain < 1, 1, ma - ma_uncertain),
                ma_max = ifelse(ma + ma_uncertain > 5, 5, ma + ma_uncertain)) %>%
  # recreate population vulnerability indexes
  dplyr::mutate(cv_lower_value = (((nfa_min) + (dfa_min)) / 2) + (rszt_min) + (ma_min),
                cv_be_value = (((nfa) + (dfa))/ 2) + (rszt) + (ma),
                cv_upper_value = (((nfa_max) + (dfa_max)) / 2) + (rszt_max) + (ma_max)) %>%
  # ***TEMPORARY: verification that values are the same
  dplyr::mutate(cv_lower_verification = cv_lower_value - cv_lower,
                cv_be_verification = cv_be_value - cv_best_estimate,
                cv_upper_verification = cv_upper_value - cv_upper)

### Displacement vulnerability table (Table 6 -- page 29-30 of report / page 37-38 of PDF)
dv_table <- dv_csv %>%
  # select fields
  dplyr::select(Common_Name,
                AlphaCode,
                MA_Score_Displacement,
                MA_Uncertainty_Value,
                HF_Score,
                HF_Uncertainty_Value,
                DV_Lower,
                DV_Best_Estimate,
                DV_Upper) %>%
  # rename fields
  dplyr::rename(species_name = Common_Name,
                species_code = AlphaCode,
                ma = MA_Score_Displacement,
                ma_uncertain = MA_Uncertainty_Value,
                hf = HF_Score,
                hf_uncertain = HF_Uncertainty_Value,
                dv_lower = DV_Lower,
                dv_best_estimate = DV_Best_Estimate,
                dv_upper = DV_Upper) %>%
  # calculate minimum and maximum values for each metric
  ## ***Note: metric scores are kept within a 1 - 5 value range. Thus if a minimum after removing
  ##          uncertainty were less than one the floor is 1; in contrast the ceiling for the
  ##          maximum value (after adding the uncertainty) is 5
  dplyr::mutate(ma_min = ifelse(ma - ma_uncertain < 1, 1, ma - ma_uncertain),
                ma_max = ifelse(ma + ma_uncertain > 5, 5, ma + ma_uncertain),
                hf_min = ifelse(hf - hf_uncertain < 1, 1, hf - hf_uncertain),
                hf_max = ifelse(hf + hf_uncertain > 5, 5, hf + hf_uncertain)) %>%
  # recreate population vulnerability indexes
  dplyr::mutate(dv_lower_value = ma_min + hf_min,
                dv_be_value = ma + hf,
                dv_upper_value = ma_max + hf_max) %>%
  # ***TEMPORARY: verification that values are the same
  dplyr::mutate(dv_lower_verification = dv_lower_value - dv_lower,
                dv_be_verification = dv_be_value - dv_best_estimate,
                dv_upper_verification = dv_upper_value - dv_upper)

## Kelsey et al. (2018)
### Population vulnerability table
population_vulnerability_table <- population_vulnerability_function(pdf, 13) %>%
  rbind(population_vulnerability_function(pdf, 14),
        population_vulnerability_function(pdf, 15)) %>%
  dplyr::left_join(x = .,
                   y = vulnerability_table,
                   by = "species_name") %>%
  dplyr::select(-pv_lower_value.y,
                -pv_be_value.y,
                -pv_upper_value.y,
                -cv_lower_value,
                -cv_be_value,
                -cv_upper_value,
                -cv_rank,
                -dv_lower_value,
                -dv_be_value,
                -dv_upper_value,
                -dv_rank) %>%
  dplyr::rename(pv_lower_value = pv_lower_value.x,
                pv_be_value = pv_be_value.x,
                pv_upper_value = pv_upper_value.x) %>%
  dplyr::relocate(species_code, .before = species_name) %>%
  # recreate population vulnerability indexes (***WARNING: there are errors for certain species)
  dplyr::mutate(pop_vul_min = (POP - pop_uncertain) + (AO * (POCS.pop - pocs_pop_uncertain)) + TS + (BR * (AS - as_uncertain)),
                pop_vul_be = (POP) + (AO * POCS.pop) + TS + (BR * AS),
                pop_vul_max = (POP + pop_uncertain) + (AO * (POCS.pop + pocs_pop_uncertain)) + TS + (BR * (AS + as_uncertain)))

## Collision vulnerability table
### First part of Appendix Table 3 (page 15, or page 243 of Kelsey et al. 2018)
col_vulnerability_table1 <- tabulizer::extract_tables(pdf,
                                                      # page tables are found (243 - 244) -- Appendix Table 3
                                                      pages = 15,
                                                      # it is the 2nd table on the 15th page
                                                      output = "data.frame")[[2]] %>%
  
  # delete the first two rows as they do not contain relevant information
  dplyr::filter(!row_number() %in% c(1)) %>%
  
  # spearate population vulnerability into lower, best estimate, and upper values
  tidyr::separate(Average.Flight.Activity, into = c("avg_flgt_act_lower_value", "avg_flgt_act_be_value", "avg_flgt_act_upper_value"), sep = " ", remove = T, convert = T) %>%
  tidyr::separate(Collision.Vulnerability, into = c("cv_lower_value", "cv_be_value", "cv_upper_value"), sep = " ", remove = T, convert = T) %>%
  
  # ***Note: see methods from Kelsey et al. (2018) to see more detail on average flight activity (average of nocturnal flight activity (NFA) and diurnal flight activity (DFA))
  
  # rename fields
  dplyr::rename(species_name = Species.name,
                # uncertainty for percent time spent in rotor sweep zone
                rszt_uncertain = u,
                # uncertainty for macro-avoidance
                mac_uncertain = u.1,
                # percent of best values
                percent = X)

### Second part of Appendix Table 3 (page 16, or page 243 of Kelsey et al. 2018)
col_vulnerability_table2 <- tabulizer::extract_tables(pdf,
                                                      # page tables are found (243 - 244) -- Appendix Table 3
                                                      pages = 16,
                                                      # it is the 1st table on the 16th page
                                                      output = "data.frame")[[1]] %>%
  
  # delete the first two rows as they do not contain relevant information
  dplyr::filter(!row_number() %in% c(1)) %>%
  
  # spearate population vulnerability into lower, best estimate, and upper values
  tidyr::separate(Average.Flight.Activity, into = c("avg_flgt_act_lower_value", "avg_flgt_act_be_value", "avg_flgt_act_upper_value"), sep = " ", remove = T, convert = T) %>%
  tidyr::separate(Collision.Vulnerability, into = c("cv_lower_value", "cv_be_value", "cv_upper_value"), sep = " ", remove = T, convert = T) %>%
  
  # ***Note: see methods from Kelsey et al. (2018) to see more detail on average flight activity (average of nocturnal flight activity (NFA) and diurnal flight activity (DFA))
  
  # rename fields
  dplyr::rename(species_name = Species.name,
                # uncertainty for percent time spent in rotor sweep zone
                rszt_uncertain = u,
                # uncertainty for macro-avoidance
                mac_uncertain = u.1,
                # percent of best values
                percent = X)

collision_vulnerability_table <- col_vulnerability_table1 %>%
  rbind(col_vulnerability_table2) %>%
  dplyr::left_join(x = .,
                   y = vulnerability_table,
                   by = "species_name") %>%
  dplyr::select(-pv_lower_value,
                -pv_be_value,
                -pv_upper_value,
                -pv_rank,
                -cv_lower_value.y,
                -cv_be_value.y,
                -cv_upper_value.y,
                -dv_lower_value,
                -dv_be_value,
                -dv_upper_value,
                -dv_rank) %>%
  dplyr::rename(cv_lower_value = cv_lower_value.x,
                cv_be_value = cv_be_value.x,
                cv_upper_value = cv_upper_value.x) %>%
  dplyr::relocate(species_code, .before = species_name) %>%
  # recreate collision vulnerability index (WARNING: not possible to score as calculations for nocturnal nor diurnal flight
  # activities are available)
  dplyr::mutate(col_vul_index_lower = )

## Displacement vulnerability table
displacement_vulnerability_table <- displacement_vulnerability_function(pdf, 17) %>%
  rbind(displacement_vulnerability_function(pdf, 18)) %>%
  dplyr::left_join(x = .,
                   y = vulnerability_table,
                   by = "species_name") %>%
  dplyr::select(-pv_lower_value,
                -pv_be_value,
                -pv_upper_value,
                -pv_rank,
                -cv_lower_value,
                -cv_be_value,
                -cv_upper_value,
                -cv_rank,
                -dv_lower_value.y,
                -dv_be_value.y,
                -dv_upper_value.y) %>%
  dplyr::rename(dv_lower_value = dv_lower_value.x,
                dv_be_value = dv_be_value.x,
                dv_upper_value = dv_upper_value.x) %>%
  dplyr::relocate(species_code, .before = species_name)

#####################################

### Code 24 -- LISA

# ELSA
## LISA ()
oregon_lisa <- elsa::lisa(x = oregon_model_areas,
                          # minimum distance
                          d1 = 0,
                          # search distance went to 8400m
                          d2 = 8400,
                          # statistic is "local Moran's"
                          statistic = "i",
                          # tell which field to use for calculation ("model_geom_mean")
                          zcol = "model_geom_mean")

oregon_hex_lisa <- oregon_lisa %>%
  sf::st_as_sf() %>%
  cbind(., oregon_model_areas) %>%
  dplyr::select(index,
                # Ii = Moran's I statistic, Z.Ii = z-score of Moran's I
                Ii, Z.Ii)

# rgeoda
## calculate important fields
test <- oregon_model_areas_sf %>%
  cbind(rgeoda::lisa_pvalues(gda_lisa = lisa),
        rgeoda::lisa_clusters(gda_lisa = lisa, cutoff = 0.05),
        rgeoda::lisa_values(gda_lisa = lisa),
        rgeoda::lisa_num_nbrs(gda_lisa = lisa)) %>%
  dplyr::rename("p_vals" = "rgeoda..lisa_pvalues.gda_lisa...lisa.",
                "c_vals" = "rgeoda..lisa_clusters.gda_lisa...lisa..cutoff...0.05.",
                "lisa_values" = "rgeoda..lisa_values.gda_lisa...lisa.",
                "num_neigh" = "rgeoda..lisa_num_nbrs.gda_lisa...lisa." )

lisa_labels <- rgeoda::lisa_labels(gda_lisa = lisa) %>%
  as.data.frame() %>%
  dplyr::rename("labels" = ".") %>%
  # Predefined values
  ## 0 Not significant
  ## 1 High-High
  ## 2 Low-Low
  ## 3 Low-High
  ## 4 High-Low
  ## 5 Undefined
  ## 6 Isolated
  dplyr::mutate(c_vals = c(0, 1, 2, 3, 4, 5, 6))

lisa_colors <- rgeoda::lisa_colors(gda_lisa = lisa) %>%
  as.data.frame() %>%
  dplyr::rename("colors" = ".") %>%
  # Predefined values
  ## 0 #eeeeee (not significant)
  ## 1 #FF0000 (high-high)
  ## 2 #0000FF (low-low)
  ## 3 #a7adf9 (low-high)
  ## 4 #f4ada8 (high-low)
  ## 5 #464646 (undefined)
  ## 6 #999999 (isolated)
  dplyr::mutate(c_vals = c(0, 1, 2, 3, 4, 5, 6))

#####################################

### Code 27 -- Sensitivity

# 4 - 26
for (i in 4:4){
  start2 <- Sys.time()
  
  i <- 4
  
  name <- names(data)[i]
  
  sensitivity_jackknife <- sensitivity_jackknife %>%
    
    # when field is elected (column i) fill with NA values so as to "remove" it from analysis
    dplyr::mutate(across(.cols = i,
                         ~replace(i, !is.na(i), NA))) %>%
    
    # recalculate the geometric means for each submodel (geometric mean = nth root of the product of the variable values)
    ## calculate across rows
    dplyr::rowwise() %>%
    ## industry and operations
    dplyr::mutate(!!paste0("io_geom_mean_", name) := exp(mean(log(c_across(c("sc500_value",
                                                                             "sc1000_value",
                                                                             "eastwest_value",
                                                                             "eastwest_add_value",
                                                                             "sstat_value",
                                                                             "stransect_value"))),
                                                              # remove any values that are NA when calculating the mean
                                                              na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    #dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    ## natural resources
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the product of all protected species values
    dplyr:::mutate(species_product = prod(leatherback_value,
                                          killerwhale_value,
                                          humpback_ca_value,
                                          humpback_mx_value,
                                          bluewhale_value,
                                          non_protected_value,
                                          # remove NA values for product 
                                          na.rm = T)) %>%
    ### calculate minimum value across the habitat subdatasets
    dplyr::rowwise() %>%
    dplyr::mutate(habitat_value = pmin(efhca_value,
                                       rreef_map_value,
                                       rreef_prob_value,
                                       deep_coralsponge_value,
                                       continental_shelf_value,
                                       methane_bubble_value,
                                       na.rm = T)) %>%
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("nr_geom_mean_", name) := exp(mean(log(c_across(c("species_product",
                                                                             "habitat_value",
                                                                             "marine_bird_value"))),
                                                              # remove any values that are NA when calculating the mean
                                                              na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    #dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    ## fisheries
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("fish_geom_mean_", name) := exp(mean(log(c_across(c("fisheries_value"))),
                                                                # remove any values that are NA when calculating the mean
                                                                na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    #dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    ## wind
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("wind_geom_mean_", name) := exp(mean(log(c_across(c("wind_value"))),
                                                                # remove any values that are NA when calculating the mean
                                                                na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    #dplyr::mutate_all(~ifelse(is.na(.), NA, .)) %>%
    
    # recalculate the geometric means for each final model (geometric mean = nth root of the product of the variable values)
    ## final model
    ### calculate across rows
    dplyr::rowwise() %>%
    #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
    dplyr::mutate(!!paste0("model_geom_mean_", name) := exp(mean(log(c_across(c("io_geom_mean",
                                                                                "nr_geom_mean",
                                                                                "fish_geom_mean",
                                                                                "wind_geom_mean"))),
                                                                 # remove any values that are NA when calculating the mean
                                                                 na.rm = T))) %>%
    # change all the NaN values back to NA (using is.na() given data are a dataframe -- avoid is.nana())
    #dplyr::mutate_all(~ifelse(is.na(.), NA, .))
    
    print(paste(Sys.time() - start2, "minutes to complete creating and adding", name, "data to dataframe", sep = " ")) # print how long it takes to calculate
}

sensitivity_jackknife <- sensitivity_jackknife %>%
  sf::st_make_valid() %>%
  sf::st_as_sf(x = ., wkt = "geom")

# assign(paste("sensitivity_jackknife_removed", fields[i], sep = "_"), test)

## Version 2


# dplyr::mutate(species_product_value = prod(leatherback_value,
#                                            killerwhale_value,
#                                            humpback_ca_value,
#                                            humpback_mx_value,
#                                            bluewhale_value,
#                                            # remove NA values from the minimum calculation
#                                            na.rm = T)) %>%


#####################################

# ### calculate across rows
# dplyr::rowwise() %>%
# #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
# dplyr::mutate(!!paste0("nr_geom_mean_", name) := exp(mean(log(c_across(c("species_product_value",
#                                                                          "habitat_value",
#                                                                          "marine_bird_value"))),
#                                                           # remove any values that are NA when calculating the mean
#                                                           na.rm = T))) %>%

# dplyr::mutate(!!paste0("fish_geom_mean_", name) := exp(mean(log(c_across(c("fisheries_value"))),
#                                                             # remove any values that are NA when calculating the mean
#                                                             na.rm = T))) %>%

# dplyr::mutate(!!paste0("wind_geom_mean_", name) := exp(mean(log(c_across(c("wind_value"))),
#                                                             # remove any values that are NA when calculating the mean
#                                                             na.rm = T))) %>%

# recalculate the geometric means for each final model (geometric mean = nth root of the product of the variable values)
## final model
### calculate across rows
# dplyr::rowwise() %>%
# #### calculate the geometric mean (geometric mean = nth root of the product of the variable values)
# dplyr::mutate(!!paste0("model_geom_mean_", name) := exp(mean(log(c_across(c(paste0("io_geom_mean_", name),
#                                                                             paste0("nr_geom_mean_", name),
#                                                                             paste0("fish_geom_mean_", name),
#                                                                             paste0("wind_geom_mean_", name)))),
#                                                              # remove any values that are NA when calculating the mean
#                                                              na.rm = T))) %>%



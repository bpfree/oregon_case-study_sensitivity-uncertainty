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

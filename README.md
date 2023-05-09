# Case Study: Sensitivity and Uncertainty Analyses for Oregon wind siting

Creating a case study on sensitivity and uncertainty analyses for Oregon wind siting.

#### **Repository Structure**

-   **data**
    -   **raw_data:** the raw data integrated in the analysis (**Note:** original data name and structure were kept except when either name was not descriptive or similar data were put in same directory to simplify input directories)
    -   **intermediate_data:** disaggregated processed data
    -   **submodel_data:**
    -   **suitability_data:**
    -   **rank_data:**
    -   **sensitivity_data:**
    -   **uncertainty_data:**
-   **code:** scripts for cleaning, processing, and analyzing data
-   **figures:** figures generated to visualize analysis
-   **methodology:** detailed [methods](/methodology.pdf) for the data and analysis

***Note for PC users:*** The code was written on a Mac so to run the scripts replace "/" in the pathnames for directories with two "\\".

Please contact Brian Free ([brian.free\@noaa.gov](mailto:brian.free@noaa.gov){.email}) with any questions.

#### **Data sources**
##### *Generic Data*
| Layer | Data Source | Data Name | Metadata  | Notes |
|---------------|---------------|---------------|---------------|---------------|
| Wind Areas | BOEM | [Renewable Energy Leases and Planning Areas](https://www.boem.gov/renewable-energy/boem-renewable-energy-geodatabase) | [Metadata](https://metadata.boem.gov/geospatial/boem_renewable_lease_areas.xml), [Renewable Energy GIS Data](https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data) | Data are also accessible for download on [MarineCadastre](https://marinecadastre.gov/) (under "Active Renewable Energy Leases") |
| State Boundary | State of Oregon | [Administrative Boundary](http://navigator.state.or.us/sdl/data/shapefile/k24/or_state_boundary.zip) | [Metadata](https://spatialdata.oregonexplorer.info/osdl-geoportal/rest/document?id=%7BCF375EB0-FF70-42D9-9DAE-A17A776821A8%7D) |  [Geospatial library](https://spatialdata.oregonexplorer.info/geoportal/details;id=cf375eb0ff7042d99daea17a776821a8) |
| Bathymetry | [NOAA](https://www.ncei.noaa.gov/products/coastal-relief-model)  | [Coastal Relief Model Volume 7: Central Pacific](https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol7.nc) | [Metadata](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem:348) | [More US coverage](https://www.ngdc.noaa.gov/mgg/coastal/crm.html), [alternative download link](https://www.ngdc.noaa.gov/thredds/catalog/crm/catalog.html?dataset=crmDatasetScan/crm_vol7.nc) |
| Bathymetry | NOAA | [Central Oregon](https://www.ngdc.noaa.gov/thredds/fileServer/regional/central_oregon_13_navd88_2015.nc) | [Metadata](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem:11500;view=iso) | [NOAA Bathymetry Viewer](https://www.ncei.noaa.gov/maps/bathymetry/) |
| Bathymetry | NOAA | [Port Orford](https://www.ngdc.noaa.gov/thredds/fileServer/regional/port_orford_13_mhw_2008.nc) | [Metadata](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:410/html) | [NOAA Bathymetry Viewer](https://www.ncei.noaa.gov/maps/bathymetry/) |
| Bathymetry | NOAA | [Crescent City](https://www.ngdc.noaa.gov/thredds/fileServer/regional/crescent_city_13_navd88_2010.nc) | [Metadata](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:693/html) | [NOAA Bathymetry Viewer](https://www.ncei.noaa.gov/maps/bathymetry/) |
| Species Habitat | [NOAA](https://oceannoise.noaa.gov/biologically-important-areas) | [Biologically Important Areas](https://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip) | [Alternative dataset (same footprint, different datatable)](https://coast.noaa.gov/digitalcoast/data/biologicallyimportantareas.html), [BIA Map (2015)](https://cetsound.noaa.gov/biologically-important-area-map), [BIA II Paper (2023)](https://www.frontiersin.org/articles/10.3389/fmars.2023.1081893/full), [BIA II Map Viewer (2023)](https://experience.arcgis.com/experience/51a9e25c75a1470386827439a918e056) |

##### *Constraints Data*
| Layer | Data Source | Data Name | Metadata  | Notes |
|---------------|---------------|---------------|---------------|---------------|
| Military | Department of Defense | Combined Wind Assessment for the Oregon Offshore, BOEM, OPNAV, May 2022 | [Metadata](https://www.coastalatlas.net/waf/boem/OPNAV_CombinedAssesment_May2022.xml) | [Data Portal](https://offshorewind.westcoastoceans.org/) (Human > Military > Combined Oregon Offshore Wind Assessment, OPNAV, May 2022), [Data source provider](https://gis.lcd.state.or.us/server/rest/services/Projects/OCMP_OceanPlanning_Human/MapServer/21), [Alternative link for dataset](https://portal.westcoastoceans.org/geoportal/rest/metadata/item/45b6aa29abe7427a91d8f430eac0ab75/html), [InPort](https://www.fisheries.noaa.gov/inport/item/48875)
| Military | United States Coast Guard | [Pacific Coast Port Access Route Study](https://navcen.uscg.gov/sites/default/files/pdf/PARS/PAC_PARS_22/Draft%20PAC-PARS.pdf) | | [Federal Registrar](https://www.federalregister.gov/documents/2022/08/26/2022-18453/port-access-route-study-the-pacific-coast-from-washington-to-california), Analysis completed in Districts 11 and 13

##### *Industry and Operations Data*
| Layer | Data Source | Data Name | Metadata  | Notes |
|---------------|---------------|---------------|---------------|---------------|
| Submarine Cable | NOAA | [NOAA Chartered Submarine Cable](https://marinecadastre.gov/downloads/data/mc/SubmarineCable.zip) | [Metadata](https://www.fisheries.noaa.gov/inport/item/57238)| |


##### *Natural Resources Data*
| Layer | Data Source | Data Name | Metadata  | Notes |
|---------------|---------------|---------------|---------------|---------------|
| Conservation Areas | NOAA | [Essential Fisheries Habitat Conservation Areas](https://media.fisheries.noaa.gov/2021-02/EFH_HAPC_EFHCA_shapefiles_AM19-2006%2BAM28-2020.zip) | | [Text](https://www.ecfr.gov/current/title-50/chapter-VI/part-660/subpart-C/section-660.76) |

##### *Fisheries Data*
| Layer | Data Source | Data Name | Metadata  | Notes |
|---------------|---------------|---------------|---------------|---------------|

##### *Wind Data*
| Layer | Data Source | Data Name | Metadata  | Notes |
|---------------|---------------|---------------|---------------|---------------|

#### Data commentary
Datasets explored but not included in analyses due to not located geographically in study area:
- [BOEM Active Lease Areas](https://www.data.boem.gov/Main/Mapping.aspx#ascii) ([Geodatabase download link](https://www.data.boem.gov/Mapping/Files/ActiveLeasePolygons.gdb.zip),[Shapefile download link](https://www.data.boem.gov/Mapping/Files/actlease.zip), [Metadata](https://www.data.boem.gov/Mapping/Files/actlease_meta.html))
- [Anchorage Areas](https://marinecadastre.gov/downloads/data/mc/Anchorage.zip) ([Metadata](https://www.fisheries.noaa.gov/inport/item/48849))
- [BOEM Lease Blocks](https://www.data.boem.gov/Mapping/Files/Blocks.gdb.zip) ([Metadata](https://www.data.boem.gov/Mapping/Files/blocks_meta.html))
- [Lightering Zones](https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip) ([Metadata](https://www.fisheries.noaa.gov/inport/item/66149), [more information](https://www.govinfo.gov/content/pkg/CFR-2018-title33-vol2/xml/CFR-2018-title33-vol2-part156.xml#seqnum156.300)) - [Pipelines](https://www.data.boem.gov/Mapping/Files/Pipelines.gdb.zip) ([Option page](https://www.data.boem.gov/Main/Mapping.aspx#ascii), [Metadata](https://www.data.boem.gov/Mapping/Files/ppl_arcs_meta.html))
- [Shipping Lanes](http://encdirect.noaa.gov/theme_layers/data/shipping_lanes/shippinglanes.zip) (Federal)
- [Unexploded ordnances](https://marinecadastre.gov/downloads/data/mc/UnexplodedOrdnance.zip) (Points and areas, [metadata](https://www.fisheries.noaa.gov/inport/item/66208))
- [BOEM Drilling Platforms](https://www.data.boem.gov/Mapping/Files/Platforms.gdb.zip) ([Metadata](https://www.data.boem.gov/Mapping/Files/platform_meta.html), [Mapping Page](https://www.data.boem.gov/Main/Mapping.aspx#ascii), [Alternative Platform Structure dataset](https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx))

#### *Known issues*
The hexagonal grid in R is slightly different than a tessellation grid that ArcGIS Pro generates in its [data management toolbox](https://pro.arcgis.com/en/pro-app/latest/tool-reference/data-management/generatetesellation.htm). This is likely due to differences in the number of significant digits used for the calculations and converting acres into meters.

Hexagon nodes in the grid R creates fall approximately 1.3 meters away from similar nodes for the tessellation generated by ArcGIS. In a 10-acre area, this difference is accounts for 0.64% in distance.

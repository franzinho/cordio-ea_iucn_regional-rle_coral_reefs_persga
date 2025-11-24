##
##  Name:       create_ecoregion_coral_reefs.R
##
##  Objective:  Extract coral reefs from ecoregions in
##              region
##
##  Approach:   Point to regional ecoregions and global reef
##              spatial layers, filter for relevant ecoregions,
##              extract and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-02-26
##

##
## 1. Set up
##
 ## -- import core data objects -- ## ----

 ## -- call to ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- call to reef layer -- ##
  # point to https locale
    https_locale <-
      paste0("https://datadownload-production.s3.us-east-1.amazonaws.com/",
             "WCMC008_CoralReefs2021_v4_1.zip")

  # create temporary file for downloading
    temp_file <- tempfile()

  # set temporary file directory
    # temp_directory <- tempfile()
    temp_directory <- "data_raw/geophysical/coral_reefs/"

  # download coral reefs
    https_locale %>% download.file(temp_file)

  # extract files
    temp_file %>%
        unzip(exdir = temp_directory)

  # point to data locale
    data_locale <-
      paste0(temp_directory,
             "14_001_WCMC008_CoralReefs2021_v4_1/01_Data/") %>%
      str_replace("\\//", "\\/")

 ## -- import coral reef polygons -- ## ----
  # point to data file
    data_file <- "WCMC008_CoralReef2021_Py_v4_1.shp"

  # import coral reef polygons
    coral_reefs_polygons <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- remove reef downloads -- ##
  # remove temp directory
    paste0(temp_directory, "14_001_WCMC008_CoralReefs2021_v4_1") %>%
      fs::dir_delete()


##
## 2. Groom data
##
 ## -- clean polygons -- ## ----
  # clean reef polygons
    coral_reefs_polygons %<>% st_make_valid()

  # clean ecoregions
    regional_ecoregions %<>% st_make_valid()

 ## -- extract coral reefs -- ## ----
  # set s2 to false
    sf_use_s2(FALSE)

  # extract
    regional_coral_reefs <-
      coral_reefs_polygons %>%
        # st_transform(32637) %>%
        st_intersection(regional_ecoregions %>%
                          st_transform(4326))

  # return to wgs84 (if required) ----
    # regional_coral_reefs %<>%
      # st_transform(4326)
# Simple feature collection with 1730 features and 27 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 96.79861 ymin: -31.96877 xmax: 159.1413 ymax: -9.224654
# Geodetic CRS:  WGS 84
# # A tibble: 1,730 × 28
   # LAYER_NAME METADATA_I ORIG_NAME    FAMILY  GENUS SPECIES DATA_TYPE
 # * <chr>           <dbl> <chr>        <chr>   <chr> <chr>   <chr>    
 # 1 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 2 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 3 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 4 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 5 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 6 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 7 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 8 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
 # 9 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
# 10 CRR                 1 Not Reported Not Re… Not … Not Re… Not Repo…
# # ℹ 1,720 more rows
# # ℹ 21 more variables: START_DATE <chr>, END_DATE <chr>,
# #   DATE_TYPE <chr>, VERIF <chr>, NAME <chr>, LOC_DEF <chr>,
# #   SURVEY_MET <chr>, GIS_AREA_K <dbl>, Shape_Leng <dbl>,
# #   Shape_Area <dbl>, REP_AREA_K <chr>, Eco_code <dbl>,
# #   Ecoregion <chr>, Prov_code <dbl>, Province <chr>,
# #   Rlm_code <dbl>, Realm <chr>, Alt_code <dbl>, Eco_code_x <dbl>, …
# # ℹ Use `print(n = ...)` to see more rows

   # get column names
     regional_coral_reefs %>% names()
 # [1] "LAYER_NAME" "METADATA_I" "ORIG_NAME"  "FAMILY"     "GENUS"     
 # [6] "SPECIES"    "DATA_TYPE"  "START_DATE" "END_DATE"   "DATE_TYPE" 
# [11] "VERIF"      "NAME"       "LOC_DEF"    "SURVEY_MET" "GIS_AREA_K"
# [16] "Shape_Leng" "Shape_Area" "REP_AREA_K" "Eco_code"   "Ecoregion" 
# [21] "Prov_code"  "Province"   "Rlm_code"   "Realm"      "Alt_code"  
# [26] "Eco_code_x" "Lat_zone"   "geometry"  

  # simplify object
    regional_coral_reefs %<>%
      dplyr::select(Ecoregion,
                    # Eco_code,
                    # Province,
                    # Prov_code,
                    # Shape_Leng,
                    # Shape_Area,
                    geometry)

 # ## -- visualise -- ## ----
  # # set palette
    # c_palette <-
      # wesanderson::wes_palette("Cavalcanti1", 15, "continuous")
 
  # # plot
    # regional_coral_reefs %>%
      # ggplot() +
      # geom_sf(aes(colour = Ecoregion)) +
      # theme_void() +
      # scale_colour_manual(values = c_palette)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coral_reefs/"

  # set file name
    data_file <- "regional_coral_reefs.rda"

  # save ecoregions to file
    save(regional_coral_reefs,
      file = paste0(save_locale, data_file))


# ## -- export to shapefile -- ## ----
#  # point to save locale
#    save_locale <- "data/geophysical/coral_reefs/wcmc_coral_reefs_2021_v4.1/"
#
#  # export geometry only
#    coral_reefs %>%
#      st_geometry() %>%
#      st_write(paste0(save_locale, "wcmc_coral_reefs_2021_v4.1.shp"))
# Writing layer `wcmc_coral_reefs_2021_v4.1' to data source
#   `data/geophysical/coral_reefs/wcmc_coral_reefs_2021_v4.1/wcmc_coral_reefs_2021_v4.1.shp'
#    using driver `ESRI Shapefile'
# Writing 17504 features with 0 fields and geometry type Multi Polygon.

##
## 4. Clean up workspace
##
  # clean up paths
    rm(https_locale,
       data_locale,
       data_file,
       save_locale)

  # remove original objects
    rm(coral_reefs_polygons)

  # remove intermediate objects
    rm(regional_ecoregions)

  # remove core objects
    rm(regional_coral_reefs)


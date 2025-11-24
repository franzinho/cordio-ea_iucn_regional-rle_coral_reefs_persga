##
##  Name:       assign_ecoregions_to_regional_percent_cover.R
##
##  Objective:  Assign ecoregion (or area of assessment) to
##                regional percent cover object
##
##  Approach:   Load regional ecoregions and regional percent
##                cover data, convert to sf objects, 
##                overlay and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2025-08-04
##


##
## 1. Set up
##
 ## -- call to ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # load data
    load(paste0(data_locale, data_file))


 ## -- call to regional percent cover -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/sessiles/"

  # set data file name
    data_file <- "regional_percent_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look at cover data ----
    regional_percent_cover
# # A tibble: 3,171 × 14
   # date       region_name    island site_code latitude longitude
   # <date>     <chr>          <chr>  <chr>        <dbl>     <dbl>
 # 1 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 2 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 3 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 4 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 5 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 6 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 7 2013-07-12 Northwestern … Kure   KUR-50        28.4     -178.
 # 8 2013-07-12 Northwestern … Kure   KUR-51        28.4     -178.
 # 9 2013-07-12 Northwestern … Kure   KUR-51        28.4     -178.
# 10 2013-07-12 Northwestern … Kure   KUR-51        28.4     -178.
# # ℹ 3,161 more rows
# # ℹ 8 more variables: reef_zone <chr>, depth_bin <chr>,
# #   replicate <chr>, tier_1 <chr>, category_name <chr>,
# #   n_occurrences <int>, n_points <int>, percent_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # have a loook at ecoregions
    regional_ecoregions
# Simple feature collection with 23 features and 9 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 6785945 ymin: 2246585 xmax: 16869370 ymax: 13330260
# Projected CRS: WGS 84 / Equi7 Oceania
# # A tibble: 23 × 10
   # Eco_code Ecoregion Prov_code Province Rlm_code Realm Alt_code
 # *    <dbl> <chr>         <dbl> <chr>       <dbl> <chr>    <dbl>
 # 1    20134 Bismarck…        31 Eastern…        6 Cent…      128
 # 2    20150 Coral Sea        35 Tropica…        6 Cent…      143
 # 3    20124 East Car…        29 Tropica…        6 Cent…      139
 # 4    25147 Fiji Isl…        35 Tropica…        6 Cent…      144
 # 5    25152 Hawaii           37 Hawaii          7 East…      158
 # 6    20155 Line Isl…        39 Central…        7 East…      150
 # 7    20123 Mariana …        29 Tropica…        6 Cent…      140
 # 8    20158 Tuamotus         40 Southea…        7 East…      156
 # 9    20153 Marshall…        38 Marshal…        7 East…      149
# 10    20149 New Cale…        35 Tropica…        6 Cent…      145
# # ℹ 13 more rows
# # ℹ 3 more variables: Eco_code_x <dbl>, Lat_zone <chr>,
# #   geometry <GEOMETRY [m]>
# # ℹ Use `print(n = ...)` to see more rows

  # get names
    regional_percent_cover %>% names()
 # [1] "date"          "region_name"   "island"       
 # [4] "site_code"     "latitude"      "longitude"    
 # [7] "reef_zone"     "depth_bin"     "replicate"    
# [10] "tier_1"        "category_name" "n_occurrences"
# [13] "n_points"      "percent_cover"

 ## -- link ecoregions -- ## ----
  # set to spatial object
    regional_percent_cover_sf <-
      regional_percent_cover %>%
      mutate(long = longitude,
             lat  = latitude) %>%
      dplyr::select(longitude,
                    latitude,
                    long,
                    lat,
                    region_name,
                    island,
                    site_code,
                    date,
                    reef_zone,
                    depth_bin,
                    replicate,
                    tier_1,
                    category_name,
                    percent_cover) %>%
      st_as_sf(coords = c("long", "lat"),
               crs    = 4326)

  # link with ecoregions
    regional_percent_cover_sf %<>%
      st_transform(32637) %>%
    st_intersection(regional_ecoregions %>%
                      dplyr::select(Ecoregion,
                                    geometry))

 ## -- clean object -- ## ----
  # check reef zones
    regional_percent_cover_sf %>% pull(reef_zone) %>% unique()
# [1] NA                "Forereef"        "Lagoon"         
# [4] "Protected Slope" "Backreef" 

  # return to data frame
    regional_percent_cover <-
      regional_percent_cover_sf %>%
        st_drop_geometry() %>%
      dplyr::select(Ecoregion,
                    longitude,
                    latitude,
                    region_name,
                    island,
                    site_code,
                    date,
                    reef_zone,
                    depth_bin,
                    replicate,
                    tier_1,
                    category_name,
                    percent_cover) 

  # have a quick look
    regional_percent_cover %>% quickview()
  # Ecoregion longitude latitude                   region_name
# 1    Hawaii -178.3765 28.37653 Northwestern Hawaiian Islands
# 2    Hawaii -178.3765 28.37653 Northwestern Hawaiian Islands
# 3    Hawaii -178.3765 28.37653 Northwestern Hawaiian Islands
  # island site_code       date reef_zone depth_bin replicate
# 1   Kure    KUR-50 2013-07-12  Forereef       Mid         A
# 2   Kure    KUR-50 2013-07-12  Forereef       Mid         A
# 3   Kure    KUR-50 2013-07-12  Forereef       Mid         A
  # tier_1  category_name percent_cover
# 1    CCA Coralline Alga     2.2580645
# 2  CORAL          Coral     0.9677419
# 3     MA      Macroalga    18.3870968


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/sessiles/"

  # save ecoregions to file
    save(regional_percent_cover,
      file = paste0(save_locale, "regional_percent_cover.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_ecoregions,
       regional_percent_cover_sf)

  # remove core objects
    rm(regional_percent_cover)


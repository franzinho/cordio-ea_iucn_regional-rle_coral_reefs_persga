##
##  Name:       create_coral_percent_cover.R
##
##  Objective:  Create coral and other benthic cover
##                data object from gcrmn sources
##
##  Approach:   Call to clean data compilation from gcrmn
##              sources, format and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Benthic cover data are primary source for 
##                   Criterion A.  Data should include a column 
##                   assigning each record to the respective 
##                   geographic unit of assessment.
##              2. Import skips first row with details of utc, 
##                   degrees N, degrees E for positions [ fs: 2025-07-30 ]
##              3. Image analysis produce three functional 
##                   group levels of benthic cover: 
##                   - Tier 1 (e.g., hard coral, 
##                                   soft coral, 
##                                   macroalgae, 
##                                   turf algae, 
##                                   etc.)
##                   - Tier 2 (e.g., Hard Coral = massive, 
##                                                branching, 
##                                                foliose, 
##                                                encrusting, 
##                                                etc.; 
##                                   Macroalgae = upright macroalgae, 
##                                                encrusting macroalgae, 
##                                                bluegreen macroalgae, and 
##                                                Halimeda, 
##                                                etc.), and 
##                   - Tier 3 (e.g., Hard Coral = Astreopora sp, 
##                                                Favia sp, 
##                                                Pocillopora, 
##                                                etc.; 
##                                   Macroalgae = Caulerpa sp, 
##                                                Dictyosphaeria sp, 
##                                                Padina sp, 
##                                                etc.)

##
## 1. Set up
##
 ## -- call to percent cover data -- ##
  # point to data locale
    data_locale <- "data_raw/biological/sessiles/noaa/"

  # set data file name
    data_file <- "CRCP_Benthic_Cover_Climate_Stations_Pacific_5dce_b44e_bbdc.xlsx"

  # import data
    regional_percent_cover <-
      paste0(data_locale, data_file) %>%
      read_excel()


##
## 2. Groom data
##
  # have a look ----
    regional_percent_cover
# # A tibble: 108,441 × 32
   # time  latitude longitude ROUNDID MISSIONID REGION_NAME ISLAND
   # <chr> <chr>    <chr>       <dbl> <chr>     <chr>       <chr> 
 # 1 UTC   degrees… degrees_…      NA <NA>      <NA>        <NA>  
 # 2 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 3 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 4 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 5 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 6 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 7 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 8 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
 # 9 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
# 10 2014… 19.6807… 145.3922…      88 HA1401    Mariana Ar… Asunc…
# # ℹ 108,431 more rows
# # ℹ 25 more variables: SITE <chr>, OCC_SITEID <lgl>,
# #   SITEVISITID <chr>, REEF_ZONE <chr>, DEPTH_BIN <chr>,
# #   PERM_SITE <dbl>, CLIMATE_STATION_YN <dbl>, MIN_DEPTH <chr>,
# #   MAX_DEPTH <chr>, DATE_ <chr>, IMAGE_NAME <chr>,
# #   Image_URL <lgl>, OBS_YEAR <dbl>, REPLICATE <chr>,
# #   PHOTOID <dbl>, ANALYST <chr>, TIER_1 <chr>, …
# # ℹ Use `print(n = ...)` to see more rows

  # compressed view
    regional_percent_cover %>% quickview()
                  # time           latitude          longitude
# 1                  UTC      degrees_north       degrees_east
# 2 2014-04-25T00:00:00Z 19.680720000000001 145.39224999999999
# 3 2014-04-25T00:00:00Z 19.680720000000001 145.39224999999999
  # ROUNDID MISSIONID         REGION_NAME   ISLAND   SITE
# 1      NA      <NA>                <NA>     <NA>   <NA>
# 2      88    HA1401 Mariana Archipelago Asuncion ASC-02
# 3      88    HA1401 Mariana Archipelago Asuncion ASC-02
  # OCC_SITEID SITEVISITID REEF_ZONE DEPTH_BIN PERM_SITE
# 1         NA        <NA>      <NA>      <NA>        NA
# 2         NA         NaN  Forereef       Mid         1
# 3         NA         NaN  Forereef       Mid         1
  # CLIMATE_STATION_YN MIN_DEPTH MAX_DEPTH                DATE_
# 1                 NA        ft        ft                  UTC
# 2                  0        37        37 2014-04-25T00:00:00Z
# 3                  0        37        37 2014-04-25T00:00:00Z
            # IMAGE_NAME Image_URL OBS_YEAR REPLICATE PHOTOID
# 1                 <NA>        NA       NA      <NA>      NA
# 2 ASC-02_2014_A_01.JPG        NA     2014         A       1
# 3 ASC-02_2014_A_01.JPG        NA     2014         A       1
  # ANALYST TIER_1 CATEGORY_NAME TIER_2
# 1    <NA>   <NA>          <NA>   <NA>
# 2     EEL     TW Tape and wand   WAND
# 3     EEL   TURF     Turf Alga  TURFH
                # SUBCATEGORY_NAME TIER_3 GENERA_NAME X_POS Y_POS
# 1                           <NA>   <NA>        <NA>    NA    NA
# 2                           Wand   <NA>        <NA>   536  2633
# 3 Turf growing on hard substrate   <NA>        <NA>  2454   392
                            # accession_url
# 1                                    <NA>
# 2 https://accession.nodc.noaa.gov/0157721
# 3 https://accession.nodc.noaa.gov/0157721

  # remove first row 
    regional_percent_cover %<>%
      dplyr::slice(-1)

  # get unique sites
    regional_percent_cover %>%
      rename(region_name = REGION_NAME) %>%
      group_by(region_name,
               latitude,
               longitude) %>%
      summarise(n_years = OBS_YEAR %>% unique() %>% length())
# `summarise()` has grouped output by 'region_name', 'latitude'.
# You can override using the `.groups` argument.
# # A tibble: 338 × 4
# # Groups:   region_name, latitude [335]
   # region_name    latitude            longitude          n_years
   # <chr>          <chr>               <chr>                <int>
 # 1 American Samoa -11.04568868        -171.0770818             1
 # 2 American Samoa -11.0457            -171.076979999999…       1
 # 3 American Samoa -11.05074205        -171.092232999999…       1
 # 4 American Samoa -11.050789999999999 -171.09222               1
 # 5 American Samoa -11.050940000000001 -171.065859999999…       1
 # 6 American Samoa -11.05261           -171.064709999999…       2
 # 7 American Samoa -11.05762           -171.091460000000…       1
 # 8 American Samoa -11.058590000000001 -171.091019999999…       2
 # 9 American Samoa -11.068289999999999 -171.08122               2
# 10 American Samoa -14.152125          -170.811919999999…       1
# # ℹ 328 more rows
# # ℹ Use `print(n = ...)` to see more rows


 ## -- clean up columns -- ## ----
  # set date
    regional_percent_cover %<>%
      separate(time,
               into = c("date", "time"),
               sep  = "T") %>%
      mutate(date = date %>% as_date())

  # set coordinates to numeric
    regional_percent_cover %<>%
      mutate(latitude  = latitude  %>% as.numeric(),
             longitude = longitude %>% as.numeric())


 ## -- select key columns -- ## ----
  # get names
    regional_percent_cover %>% names()
 # [1] "time"               "latitude"          
 # [3] "longitude"          "ROUNDID"           
 # [5] "MISSIONID"          "REGION_NAME"       
 # [7] "ISLAND"             "SITE"              
 # [9] "OCC_SITEID"         "SITEVISITID"       
# [11] "REEF_ZONE"          "DEPTH_BIN"         
# [13] "PERM_SITE"          "CLIMATE_STATION_YN"
# [15] "MIN_DEPTH"          "MAX_DEPTH"         
# [17] "DATE_"              "IMAGE_NAME"        
# [19] "Image_URL"          "OBS_YEAR"          
# [21] "REPLICATE"          "PHOTOID"           
# [23] "ANALYST"            "TIER_1"            
# [25] "CATEGORY_NAME"      "TIER_2"            
# [27] "SUBCATEGORY_NAME"   "TIER_3"            
# [29] "GENERA_NAME"        "X_POS"             
# [31] "Y_POS"              "accession_url" 


  # select key columns
    regional_percent_cover <-
      regional_percent_cover %>%
        dplyr::select(date,
                      region_name      = REGION_NAME,
                      island           = ISLAND,
                      site_code        = SITE,
                      latitude,
                      longitude,
                      reef_zone        = REEF_ZONE,
                      depth_bin        = DEPTH_BIN,
                      depth_min        = MIN_DEPTH,
                      depth_max        = MAX_DEPTH,
                      replicate        = REPLICATE,
                      tier_1           = TIER_1,
                      category_name    = CATEGORY_NAME,
                      tier_2           = TIER_2,
                      subcategory_name = SUBCATEGORY_NAME,
                      tier_3           = TIER_3,
                      genera_name      = GENERA_NAME,
                      x_pos            = X_POS,
                      y_pos            = Y_POS,
                      url              = accession_url)

 ## -- calculate percent cover tier 1 -- ## ----
  # summarise points
    sample_points <-
      regional_percent_cover %>%
        group_by(date,
                 region_name,
                 island,
                 site_code,
                 latitude,
                 longitude,
                 reef_zone,
                 depth_bin,
                 replicate) %>%
        reframe(n_points = x_pos %>% unique() %>% length())

   # summarise tier 1 taxa
     regional_percent_cover_summary <-
       regional_percent_cover %>%
        group_by(date,
                 region_name,
                 island,
                 site_code,
                 latitude,
                 longitude,
                 reef_zone,
                 depth_bin,
                 replicate,
                 tier_1,
                 category_name) %>%
         reframe(n_occurrences = x_pos %>% unique() %>% length())

   # join to core object
     regional_percent_cover_summary %<>%
       left_join(sample_points)

   # calculate percent cover
     regional_percent_cover <-
       regional_percent_cover_summary %>%
         mutate(percent_cover = (n_occurrences / n_points) * 100)

   # quickview
     regional_percent_cover %>% quickview()
        # date                   region_name island site_code
# 1 2013-07-12 Northwestern Hawaiian Islands   Kure    KUR-50
# 2 2013-07-12 Northwestern Hawaiian Islands   Kure    KUR-50
# 3 2013-07-12 Northwestern Hawaiian Islands   Kure    KUR-50
  # latitude longitude reef_zone depth_bin replicate tier_1
# 1 28.37653 -178.3765  Forereef       Mid         A    CCA
# 2 28.37653 -178.3765  Forereef       Mid         A  CORAL
# 3 28.37653 -178.3765  Forereef       Mid         A     MA
   # category_name n_occurrences n_points percent_cover
# 1 Coralline Alga             7      310     2.2580645
# 2          Coral             3      310     0.9677419
# 3      Macroalga            57      310    18.3870968


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/sessiles/"

  # save to file
    save(regional_percent_cover,
      file = paste0(save_locale, "regional_percent_cover.rda"))


##
## 4. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove core objects
    rm(regional_percent_cover)


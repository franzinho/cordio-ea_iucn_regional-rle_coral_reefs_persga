##
##  Name:       create_regional_monitoring_sites.R
##
##  Objective:  Create data object for monitoring site
##              coordinates and other characteristics
##
##  Approach:   Import data table with site coordinates,
##              summarise and generate spatial object.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-13
##

##  Notes:      1. Need to convert occurrence data to
##                 percent cover to create summary object [ fs: 2025-07-30 ]
##              2. Simplifying monitoring site creation by
##                 assigning ecoregion in separate script
##              3. Data object currently only holds tier 1
##                 categores.  Should include other tiers [ fs: 2025-08-04 ]

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


 ## -- call to sno corail monitoring sites -- ##
  # point to data locale
    data_locale <- "data_raw/spatial/monitoring_sites/"

  # point to data file
    # data_file <- "sno-corail_study_sites.xlsx"
    data_file <- "CORAIL_GS_20250902.xlsx"

  # call to data
    sno_corail_sites <-
      paste0(data_locale, data_file) %>%
      read_xlsx()

 ## -- call to percent cover data -- ##
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
# # A tibble: 3,171 × 13
   # Ecoregion longitude latitude region_name     island site_code
   # <chr>         <dbl>    <dbl> <chr>           <chr>  <chr>    
 # 1 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 2 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 3 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 4 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 5 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 6 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 7 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-50   
 # 8 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-51   
 # 9 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-51   
# 10 Hawaii        -178.     28.4 Northwestern H… Kure   KUR-51   
# # ℹ 3,161 more rows
# # ℹ 7 more variables: date <date>, reef_zone <chr>,
# #   depth_bin <chr>, replicate <chr>, tier_1 <chr>,
# #   category_name <chr>, percent_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # create spatial object
    regional_percent_cover_sf <-
      regional_percent_cover %>%
        mutate(long = longitude,
               lat  = latitude) %>%
      st_as_sf(coords = c("long", "lat"),
               crs    = 4326)      

  # set to utm
    regional_percent_cover_sf %<>%
      st_transform(32637)

 ## -- clean up sno corail sites -- ## ----
  # have a look at sno corail sites
    sno_corail_sites
# # A tibble: 331 × 17
   # network station         country localisation latitude longitude
   # <chr>   <chr>           <chr>   <chr>           <dbl>     <dbl>
 # 1 CORAIL  Aratika         Tuamotu French Poly…   -15.5      -145.
 # 2 CORAIL  Aratika         Tuamotu French Poly…   -15.5      -145.
 # 3 CORAIL  Bora Bora       Société French Poly…   -16.5      -152.
 # 4 CORAIL  Bora Bora       Société French Poly…   -16.5      -152.
 # 5 CORAIL  Bora Bora       Société French Poly…   -16.5      -152.
 # 6 CORAIL  Chrismas Island Kiriba… South Pacif…     1.96     -157.
 # 7 CORAIL  Chrismas Island Kiriba… South Pacif…     1.96     -157.
 # 8 CORAIL  Chrismas Island Kiriba… South Pacif…     1.96     -157.
 # 9 CORAIL  Chrismas Island Kiriba… South Pacif…     1.96     -157.
# 10 CORAIL  Chrismas Island Kiriba… South Pacif…     1.96     -157.
# # ℹ 321 more rows
# # ℹ 11 more variables: coverage <chr>, date_in <dbl>,
# #   date_out <dbl>, instrument <chr>, depth <chr>,
# #   frequency <chr>, variable <chr>, access <chr>, doi <lgl>,
# #   resp <chr>, mail <chr>
# # ℹ Use `print(n = ...)` to see more rows

  # clean up depth
    sno_corail_sites %<>%
      mutate(depth = depth %>% str_remove_all("m"),
             depth = depth %>% as.numeric())

  # get range
    sno_corail_sites %>% pull(depth) %>% range(na.rm = TRUE)
# [1]  0 55

  # screen veriables
    sno_corail_sites %>% pull(variable) %>% unique() 
 # [1] "Coral"              "Fish"              
 # [3] "Temperature"        "Significant height"
 # [5] "Significant period" "Reef rugosity"     
 # [7] "Reef 3D model"      "Conductivity"      
 # [9] "Oxygen"             "pH"                
# [11] "Salinity"           "fluorimetry"       
# [13] "Turbidity"          "Macro invertebrate"
# [15] "Phosphate"          "nitrates"          
# [17] "nitrites"           "carbonates"        
# [19] "Silica"             "Ammonium" 

  # set variables to maintain
    variables_of_interest <-
      c("Coral",
        "Fish")

  # convert to spatial object
    sno_corail_sites_sf <-
      sno_corail_sites %>%
        dplyr::filter(variable %in% variables_of_interest) %>%
        mutate(Longitude = longitude,
               Latitude  = latitude) %>%
        dplyr::filter(!longitude %>% is.na(),
                      !latitude  %>% is.na()) %>%
        dplyr::select(Locality = country,
                      Site = station,
                      Latitude,
                      Longitude,
                      latitude,
                      longitude) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),
               crs    = 4326)

  # set to utm
    sno_corail_sites_sf %<>%
      st_transform(32637)

  # link with ecoregions
    sno_corail_sites_sf %<>%
    st_intersection(regional_ecoregions %>%
                      dplyr::select(Ecoregion,
                                    geometry))


 ## -- generate criteria object -- ## ----
  # review categories
    # regional_percent_cover %>% pull(tier_1) %>% unique()
 # [1] "TURF"  "CORAL" "TW"    "MA"    "SED"   "UC"    "CCA"  
 # [8] "I"     "MF"    "SC" 

  # review category names
    # regional_percent_cover %>% pull(category_name) %>% unique()
 # [1] "Turf Alga"            "Coral"               
 # [3] "Tape and wand"        "Macroalga"           
 # [5] "Sediment"             "Unclassified"        
 # [7] "Coralline Alga"       "Sessile Invertebrate"
 # [9] "Mobile Fauna"         "Soft Coral"  

  # # set level 1 code of interest
    # level1_of_interest <-
      # c("Coral")

 ## -- summarise for criterion a                                         ##
 ##      no_years                = no. of time points                    ##
 ##      year_gap                = years between 1st and final record    ##
 ##      recent_coral_cover      = returns latest coral cover            ##
 ##      original_coral_cover    = returns earliest coral cover          ##
 ##      mean_coral_cover_thresh = 2013 onwards                       -- ##

  # # filter & summarise
    # criterion_a_data_table_coral_cover <-
      # regional_percent_cover %>%
        # dplyr::filter(level1_code %in% level1_of_interest) %>%
        # group_by(Ecoregion,
                 # site_name) %>%
      # summarise(first_year  = Year %>% min(na.rm = TRUE),
                # recent_year = Year %>% max(na.rm = TRUE),
                # lat         = Latitude  %>% mean(na.rm = TRUE),
                # long        = Longitude %>% mean(na.rm = TRUE),
                # no_years    = Year %>% unique() %>% length(),
                # year_gap    = (recent_year - first_year) + 1,
                # recent_coral_cover      = percent_cover_mean %>% tail(n = 1),
                # original_coral_cover    = percent_cover_mean %>% head(n = 1),
                # mean_coral_cover_thresh = percent_cover_mean[Year >= y_thresh] %>% 
                                                             # mean(na.rm = TRUE))
                # # n           = n(),
                # # n_diff      = n - no_years)

  # # have a look
    # regional_percent_cover_summary %>% quickview()

 # # ## -- create spatial object -- ##
 # #  # create Ecoregion
 # #    regional_percent_cover_summary %<>%
 # #      mutate(Ecoregion = ecoregion_country %>% str_split_i("_", 1))

  # # order columns
    # regional_percent_cover_summary %<>%
      # ungroup() %>%
      # dplyr::select(Ecoregion,
                    # Country,
                    # # ecoregion_country,   ## -- evaluating if this is redundant -- ##
                    # # site_id,
                    # lat,
                    # long,
                    # first_year,
                    # recent_year,
                    # no_years,
                    # year_gap,
                    # recent_coral_cover,
                    # original_coral_cover,
                    # mean_coral_cover)

  # # generate spatial object
    # regional_monitoring_sites <-
      # regional_percent_cover_summary %>%
        # st_as_sf(coords = c("long", "lat"),
                 # crs    = 4326)

 ## -- splice for plotting raw data -- ## ----
  # add data source for noaa
    regional_percent_cover_sf %<>%
      mutate(Source = "NOAA")

  # add data source for noaa
    sno_corail_sites_sf %<>%
      mutate(Source = "CORAIL")

  # create object
    regional_monitoring_sites <-
      regional_percent_cover_sf %>%
        # dplyr::select(Ecoregion,
                      # region_name,
                      # island,
                      # site_code,        
                      # longitude,
                      # latitude,
                      # date,
                      # reef_zone,
                      # tier_1,
                      # category_name,
                      # tier_2,
                      # subcategory_name)
        mutate(site_id = paste(island, site_code, sep = "_")) %>%
        dplyr::select(Ecoregion,
                      Locality = region_name,
                      # island,
                      Site = site_id,
                      longitude,
                      latitude,
                      Source) %>%
        distinct() %>%
      rbind(sno_corail_sites_sf)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/monitoring_sites/"

  # save ecoregions to file
    save(regional_monitoring_sites,
      file = paste0(save_locale, "regional_monitoring_sites.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_percent_cover,
       level1_of_interest,
       regional_percent_cover_summary)

  # remove core objects
    rm(regional_monitoring_sites)


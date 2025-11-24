##
##  Name:       create_regional_fish_abundance.R
##
##  Objective:  Create fish abundance data object for
##                pacific region
##
##  Approach:   Call to clean data compilation fromm noaa &
##               other sources, format, join with
##               regional ecoregions and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mathilde Maslin
##              CORDIO East Africa & SAS Marepolis
##
##  Date:       2025-08-09
##

##  Notes:      1. Using publically available data from
##                   noaa to initiate analysis for criterion
##                   d - biological degradation  [ fs: 2025-08-09 ]
##              2. Should include routine for building
##                   taxonomy from eol and coeficients directly
##                   from fishbase               [ fs: 2025-08-09 ]
##              3. Need to investigate habitat types and
##                   substrate data for calibrating [ fs: 2025-08-09 ]
##              4. Should plot site locations to link with
##                   sessile monitoring data     [ fs: 2025-08-09 ]


##
## 1. Set up
##
 # -- call to regional ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- call to fish abundance data -- ##
  # point to data locale
    data_locale <- "data_raw/biological/fishes/noaa/"

  # get list of files
    file_list <-
      data_locale %>% dir_ls(glob = "*.xlsx")

  # create empty object to hold results
    regional_fish_abundance <- tibble()

  # loop to import
    for(i in 1:length(file_list)){

      # print progress to screen
        cat(paste0("...processing ", file_list[i] %>%
                                       str_remove(data_locale),
                   " [ ", i, " of ", length(file_list), " ]\n"))

      # call to data
        dat <-
          file_list[i] %>%
            read_xlsx()

      # remove initial row
        dat %<>%
          slice(-1)

      # convert formats
        dat %<>% mutate(LW_A = LW_A %>% as.numeric())

      # harvest results
        regional_fish_abundance %<>%
          bind_rows(dat)

    }


##
## 2. Groom data
##
  # have a look ----
    regional_fish_abundance
# # A tibble: 445,364 × 68
#    time  latitude longitude OBJECTID ROUNDID MISSIONID REGION_NAME
#    <chr> <chr>    <chr>        <dbl>   <dbl> <chr>     <chr>
#  1 2023… -14.290… -170.749…  2567949     191 RA2301_L… American S…
#  2 2023… -14.290… -170.749…  2567950     191 RA2301_L… American S…
#  3 2023… -14.290… -170.749…  2567951     191 RA2301_L… American S…
#  4 2023… -14.290… -170.749…  2567952     191 RA2301_L… American S…
#  5 2023… -14.290… -170.749…  2567953     191 RA2301_L… American S…
#  6 2023… -14.290… -170.749…  2567954     191 RA2301_L… American S…
#  7 2023… -14.290… -170.749…  2567955     191 RA2301_L… American S…
#  8 2023… -14.290… -170.749…  2567956     191 RA2301_L… American S…
#  9 2023… -14.290… -170.749…  2567957     191 RA2301_L… American S…
# 10 2023… -14.290… -170.749…  2567958     191 RA2301_L… American S…
# # ℹ 445,354 more rows
# # ℹ 61 more variables: ISLAND <chr>, SITE <chr>, REEF_ZONE <chr>,
# #   DEPTH_BIN <chr>, SITEVISITID <dbl>, DATE_ <chr>,
# #   OBS_YEAR <dbl>, DIVER <chr>, REPLICATEID <dbl>, REP <chr>,
# #   METHOD <chr>, PHOTOGRAPHER <dbl>, TRAINING_YN <dbl>,
# #   DEPTH <chr>, SURVEY_RADIUS_M <chr>, HARD_CORAL <chr>,
# #   SOFT_CORAL <chr>, MA <chr>, CCA <chr>, TA <chr>, …
# # ℹ Use `print(n = ...)` to see more rows


 ## -- clean up columns -- ## ----
  # set date
    regional_fish_abundance %<>%
      separate(time,
               into = c("date", "time"),
               sep  = "T") %>%
      mutate(date = date %>% as_date())

  # set coordinates to numeric
    regional_fish_abundance %<>%
      mutate(latitude  = latitude  %>% as.numeric(),
             longitude = longitude %>% as.numeric())


 ## -- select key columns -- ## ----
  # get names
    regional_fish_abundance %>% names()
 # [1] "date"                     "time"
 # [3] "latitude"                 "longitude"
 # [5] "OBJECTID"                 "ROUNDID"
 # [7] "MISSIONID"                "REGION_NAME"
 # [9] "ISLAND"                   "SITE"
# [11] "REEF_ZONE"                "DEPTH_BIN"
# [13] "SITEVISITID"              "DATE_"
# [15] "OBS_YEAR"                 "DIVER"
# [17] "REPLICATEID"              "REP"
# [19] "METHOD"                   "PHOTOGRAPHER"
# [21] "TRAINING_YN"              "DEPTH"
# [23] "SURVEY_RADIUS_M"          "HARD_CORAL"
# [25] "SOFT_CORAL"               "MA"
# [27] "CCA"                      "TA"
# [29] "SAND"                     "TUNICATE"
# [31] "ZOANTHID"                 "CORALLIMORPH"
# [33] "CLAM"                     "CYANO"
# [35] "SPONGE"                   "OTHER"
# [37] "OTHER_TYPE"               "HABITAT_CODE"
# [39] "HABITAT_TYPE"             "CURRENT_STRENGTH"
# [41] "VISIBILITY"               "MIN_DEPTH"
# [43] "MAX_DEPTH"                "SUBSTRATE_HEIGHT_0"
# [45] "SUBSTRATE_HEIGHT_20"      "SUBSTRATE_HEIGHT_50"
# [47] "SUBSTRATE_HEIGHT_100"     "SUBSTRATE_HEIGHT_150"
# [49] "MAX_HEIGHT"               "URCHIN_DACOR"
# [51] "BORING_URCHIN_DACOR"      "SPECIES"
# [53] "TAXONNAME"                "COMMON_NAME"
# [55] "COMMONFAMILYALL"          "FAMILY"
# [57] "SCIENTIFIC_NAME"          "RANK"
# [59] "TROPHIC"                  "TROPHIC_MONREP"
# [61] "LW_A"                     "LW_B"
# [63] "LMAX"                     "LENGTH_CONVERSION_FACTOR"
# [65] "COUNT"                    "SIZE_"
# [67] "OBS_TYPE"                 "OBS_DESC"
# [69] "accession_url"

  # select key columns
    regional_fish_abundance <-
      regional_fish_abundance %>%
        dplyr::select(date,
                      region_name       = REGION_NAME,
                      island            = ISLAND,
                      site_code         = SITE,
                      latitude,
                      longitude,
                      reef_zone         = REEF_ZONE,
                      depth_bin         = DEPTH_BIN,
                      depth             = DEPTH,
                      depth_min         = MIN_DEPTH,
                      depth_max         = MAX_DEPTH,
                      replicate_id      = REPLICATEID,
                      replicate         = REP,
                      method            = METHOD,
                      training_yn       = TRAINING_YN,
                      habitat_code      = HABITAT_CODE,
                      habitat_type      = HABITAT_TYPE,
                      current_strength  = CURRENT_STRENGTH,
                      visibility        = VISIBILITY,
                      survey_radius_m   = SURVEY_RADIUS_M,
                      species           = SPECIES,
                      taxon_name        = TAXONNAME,
                      common_name       = COMMON_NAME,
                      family            = FAMILY,
                      scientific_name   = SCIENTIFIC_NAME,
                      rank              = RANK,
                      trophic           = TROPHIC,
                      trophic_monrep    = TROPHIC_MONREP,
                      lw_a              = LW_A,
                      lw_b              = LW_B,
                      l_max             = LMAX,
                      length_conversion = LENGTH_CONVERSION_FACTOR,
                      abundance         = COUNT,
                      size_class        = SIZE_,
                      obs_description   = OBS_DESC,
                      obs_type          = OBS_TYPE,
                      url              = accession_url)

   
  # quickview
     regional_fish_abundance %>% quickview()
        # date    region_name  island site_code  latitude longitude
# 1 2023-08-05 American Samoa Tutuila  TUT-5404 -14.29057 -170.7495
# 2 2023-08-05 American Samoa Tutuila  TUT-5404 -14.29057 -170.7495
# 3 2023-08-05 American Samoa Tutuila  TUT-5404 -14.29057 -170.7495
  # reef_zone depth_bin depth          depth_min          depth_max
# 1  Forereef       Mid    11 10.058400000000001 12.801600000000001
# 2  Forereef       Mid    11 10.058400000000001 12.801600000000001
# 3  Forereef       Mid    11 10.058400000000001 12.801600000000001
  # replicate_id replicate method training_yn habitat_code
# 1   1845971413         A   nSPC           0          AGR
# 2   1845971413         A   nSPC           0          AGR
# 3   1845971413         A   nSPC           0          AGR
    # habitat_type current_strength visibility survey_radius_m
# 1 Aggregate Reef             None         30             7.5
# 2 Aggregate Reef             None         30             7.5
# 3 Aggregate Reef             None         30             7.5
  # species            taxon_name         common_name        family
# 1    CEFL Centropyge flavissima Lemonpeel angelfish Pomacanthidae
# 2    CEFL Centropyge flavissima Lemonpeel angelfish Pomacanthidae
# 3    PYDI Pygoplites diacanthus     Royal angelfish Pomacanthidae
        # scientific_name    rank trophic trophic_monrep   lw_a
# 1 Centropyge flavissima Species       H        PRIMARY 0.0314
# 2 Centropyge flavissima Species       H        PRIMARY 0.0314
# 3 Pygoplites diacanthus Species      SI      SECONDARY 0.0276
    # lw_b l_max length_conversion abundance size_class
# 1 2.7995    14                 1         2          9
# 2 2.7995    14                 1         1          5
# 3 3.0000    25                 1         1         17
  # obs_description obs_type
# 1   Instantaneous        I
# 2   Instantaneous        I
# 3   Instantaneous        I
                                                                                      # url
# 1 https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0290375
# 2 https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0290375
# 3 https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0290375

 ## -- clean columns -- ## ----
  # get high level functional groups
    regional_fish_abundance %>% pull(trophic_monrep) %>% unique()
# [1] "PISCIVORE"   "PRIMARY"     "PLANKTIVORE" "SECONDARY"  
# [5] NA  

  # set to title
    regional_fish_abundance %<>%
      mutate(trophic_monrep = trophic_monrep %>% str_to_title())

 ## -- join with ecoregions -- ## ----
  # create spatial object
    regional_fish_abundance_sf <-
      regional_fish_abundance %>%
        mutate(lat  =  latitude %>% as.numeric(),
               long = longitude %>% as.numeric()) %>%
        dplyr::filter(!lat  %>% is.na(),
                      !long %>% is.na()) %>%
      st_as_sf(coords = c("long", "lat"),
               crs    = 4326)

  # set to utm
    regional_fish_abundance_sf %<>%
      st_transform(32637)

  # link with ecoregions
    regional_fish_abundance_sf %<>%
      st_intersection(regional_ecoregions %>%
                        st_transform(32637) %>%
                        dplyr::select(Ecoregion,
                                      geometry))


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/fishes/"

  # save to file
    save(regional_fish_abundance_sf,
      file = paste0(save_locale, "regional_fish_abundance_sf.rda"))


##
## 4. Clean up workspace
##
  # remove paths
    rm(data_locale,
       file_list,
       save_locale)

  # remove intermediate objects
    rm(regional_ecoregions)

  # remove core objects
    rm(regional_fish_abundance,
       regional_fish_abundance_sf)


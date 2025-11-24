##
##  Name:       create_criteria_b1_extent_of_occurrence.R
##
##  Objective:  Standardise & format data for analysing criterion B1:
##                Extent of occurrence
##
##  Approach:   Call to coral reefs and ecoregions for wio,
##              calculate extent of occurrence and
##              classify to threat categories:
##              - Critically Endangered (CR) if number of EOO units <= 2
##              - Endangered (EN) if number of EOO units == 20
##              - Vulnerable (VU) if number of EOO units <= 50
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. For this analysis, Criterion B1 will be assessed
##                 based on the *extent of occurrence* of coral reef
##                 ecosystem of the WIO assessed within 12 defined
##                 ecoregions.
##              2. This is a two step process that entails generating
##                 the smallest convex polygon encompassing an ecosystem
##                 (coral reef) and calculation of area (Km sq) of
##                 the developed EOO geometry.
##              3. As the coral reef layer has already been extracted
##                 from the ecoregions, this simplifies the calculation
##                 of area.
##              4. Changing projection to epsg:32637 instead of wio_crs, but
##                 need to confirm with james                 [ fs: 2024-03-13 ]
##

##
## 1. Set up
##
  # call to additional functionality
    # library(redlistr)

 ## -- load core objects -- ## ----

 ## -- call to monitoring site positions -- ##
  # # point to data locale
  #   data_locale <- "data_intermediate/spatial/monitoring_sites/"
  #
  # # point to data file
  #   data_file <- "regional_monitoring_sites.rda"
  #
  # # import site positions
  #   load(paste0(data_locale, data_file))


 ## -- import coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # set data file name
    data_file <- "regional_coral_reefs.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- review data objects -- ##
  # have a look
    regional_coral_reefs
# Simple feature collection with 505 features and 1 field
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.3451 ymin: 11.03142 xmax: 52.14235 ymax: 29.93821
# Geodetic CRS:  WGS 84
# # A tibble: 505 × 2
   # Ecoregion                                                 geometry
   # <chr>                                               <GEOMETRY [°]>
 # 1 Gulf of Aden MULTIPOLYGON (((43.69068 11.052, 43.69079 11.05186, …
 # 2 Gulf of Aden MULTIPOLYGON (((43.09082 11.84132, 43.09122 11.8454,…
 # 3 Gulf of Aden POLYGON ((43.15281 11.90656, 43.14776 11.90656, 43.1…
 # 4 Gulf of Aden MULTIPOLYGON (((43.15427 11.92145, 43.15455 11.92173…
 # 5 Gulf of Aden POLYGON ((42.5247 11.57635, 42.5348 11.57635, 42.533…
 # 6 Gulf of Aden MULTIPOLYGON (((42.7715 11.5682, 42.77124 11.5654, 4…
 # 7 Gulf of Aden POLYGON ((42.69764 11.58857, 42.6929 11.58857, 42.69…
 # 8 Gulf of Aden POLYGON ((43.25697 11.46631, 43.25191 11.46631, 43.2…
 # 9 Gulf of Aden MULTIPOLYGON (((42.6886 11.49236, 42.68791 11.49162,…
# 10 Gulf of Aden MULTIPOLYGON (((42.67031 11.56004, 42.67124 11.55915…
# # ℹ 495 more rows
# # ℹ Use `print(n = ...)` to see more rows


##
## 3. Calculate extent of occupancy & threat categories
##
  # get list of ecoregions
    ecoregion_list <-
      # regional_monitoring_sites %>% pull(Ecoregion) %>% unique()
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
# [1] "Gulf of Aden"                 "Northern and Central Red Sea"
# [3] "Southern Red Sea"  

 ## -- loop for analysis -- ## ----
  # create empty object to hold results
    criterion_b1_extent_of_occurrence <- tibble()

  # loop ecoregions
    for(i in 1:length(ecoregion_list)){

      # create eeo object
        eco_eoo <-
          regional_coral_reefs %>%
          filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32637) %>%
          as_Spatial() %>%
          redlistr::makeEOO()

      # calculate area
        eco_eoo_area <-
           eco_eoo %>%
             redlistr::getAreaEOO()

      # set name
        dat <-
          tribble(       ~Ecoregion,  ~`EOO Area`,
                  ecoregion_list[i], eco_eoo_area)

      # harvest results
        criterion_b1_extent_of_occurrence %<>%
          bind_rows(dat)


    }

 ## -- set threat categories -- ## ----
  # classify
    criterion_b1_extent_of_occurrence %<>%
      mutate(Status = ifelse(`EOO Area` <= 2e3,                  "CR",     NA),
             Status = ifelse(`EOO Area` %>% between(2e3, 20e3),  "EN", Status),
             Status = ifelse(`EOO Area` %>% between(20e3, 50e3), "VU", Status),
             Status = ifelse(`EOO Area` > 50e3,               "NT/LC", Status))
# # A tibble: 3 × 3
  # Ecoregion                    `EOO Area` Status
  # <chr>                             <dbl> <chr> 
# 1 Gulf of Aden                    227742. NT/LC 
# 2 Northern and Central Red Sea    330843. NT/LC 
# 3 Southern Red Sea                259441. NT/LC 


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # set file name
    data_file <- "criterion_b1_extent_of_occurrence.rda"

  # save to file
    save(criterion_b1_extent_of_occurrence,
      file = paste0(save_locale, data_file))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs,
       ecoregion_list)

  # remove core data objects
    rm(criterion_b1_extent_of_occurrence)


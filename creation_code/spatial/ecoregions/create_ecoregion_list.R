##
##  Name:       create_ecoregion_list.R
##
##  Objective:  Create list of ecoregions for regional analysis
##                from marine ecoregions of the wold
##
##  Approach:   Import ecoregion list from gcrmn global report
##              2020 and save.
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
 ## -- call to global ecoregions -- ## ----
  # point to data locale
    data_locale <- "data_raw/spatial/ecoregions/"

  # point to data file
    data_file <- "gcrmn_meow_ecoregions.xlsx"

  # import ecoregions
    meow_ecoregions <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


##
## 2. Groom data
##
 ## -- review objects -- ## ----
  # have a look
    meow_ecoregions
# # A tibble: 108 × 3
   # `GCRMN Region` `GRCRMN Subregion` `MEOW Ecoregion`
   # <chr>          <chr>              <chr>
 # 1 Australia      Australia.1        142: Torres Strait Northern Gre…
 # 2 Australia      Australia.2        143: Central and Southern Great…
 # 3 Australia      Australia.3        202: Tweed-Moreton
 # 4 Australia      Australia.2        140: Arnhem Coast to Gulf of Ca…
 # 5 Australia      Australia.3        141: Bonaparte Coast
 # 6 Australia      Australia.4        144: Exmouth to Broome
 # 7 Australia      Australia.5        145: Ningaloo
 # 8 Australia      Australia.6        210: Shark Bay
 # 9 Australia      Australia.7        211: Houtman
# 10 Australia      Australia.3        120: Cocos-Keeling/Christmas Is…
# # ℹ 98 more rows
# # ℹ Use `print(n = ...)` to see more rows


  # get list of gcrmn regions
    meow_ecoregions %>% pull(`GCRMN Region`) %>% unique()
 # [1] "Australia"  "Brazil"     "Caribbean"  "East Asia"  "ETP"
 # [6] "PERSGA"     "ROPME"      "South Asia" "WIO"        "Pacific"


 ## -- extract ecoregions -- ## ----
  # identify relevant ecoregions for
    region_of_interest <-
      c("PERSGA")

  # get list of ecoregions
    ecoregion_list <-
      meow_ecoregions %>%
        dplyr::filter(`GCRMN Region` %in% region_of_interest) %>%
        separate(`MEOW Ecoregion`,
                 into = c("id", "Ecoregion"),
                 sep  = ": ") %>%
        pull(Ecoregion) %>% unique()
# [1] "Northern and Central Red Sea" "Southern Red Sea"            
# [3] "Gulf of Aden"

##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/ecoregions/"

  # set file name
    data_file <- "ecoregion_list.rda"

  # save ecoregions to file
    save(ecoregion_list,
      file = paste0(save_locale, data_file))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(meow_ecoregions,
       region_of_interest)

  # remove core objects
    rm(ecoregion_list)


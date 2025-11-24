##
##  Name:       create_reef_area_ecoregions.R
##
##  Objective:  Extract coral reefs from ecoregions for
##              regional analysis
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
 ## -- call to custom  ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to  ecoregions
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- get reef areas for ecoregions -- ##
  # get areas
    reef_area_ecoregions <-
      regional_coral_reefs %>%
        st_transform(32637) %>%
        group_by(Ecoregion) %>%
        reframe(geometry = geometry %>% st_union()) %>%
      st_as_sf() %>%
        group_by(Ecoregion) %>%
        reframe(reef_area = geometry %>% st_area())

  # compare to original values
    reef_area_ecoregions %<>%
      mutate(reef_area = (reef_area %>% as.numeric()) / 1e6)
# # A tibble: 3 × 2
  # Ecoregion                    reef_area
  # <chr>                            <dbl>
# 1 Gulf of Aden                     9648.
# 2 Northern and Central Red Sea    23479.
# 3 Southern Red Sea                32924.

 ## -- set proportional area -- ##
  # calculate proportion
    reef_area_ecoregions %<>%
      mutate(prop_area = (reef_area / sum(reef_area)) %>% round(4))
# # A tibble: 3 × 3
  # Ecoregion                    reef_area prop_area
  # <chr>                            <dbl>     <dbl>
# 1 Gulf of Aden                     9648.     0.146
# 2 Northern and Central Red Sea    23479.     0.356
# 3 Southern Red Sea                32924.     0.498


  # get ecoregion list
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
# [1] "Gulf of Aden"                 "Northern and Central Red Sea"
# [3] "Southern Red Sea" 


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coral_reefs/"

  # save reef area to file
    save(reef_area_ecoregions,
      file = paste0(save_locale, "reef_area_ecoregions.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs)

  # remove core objects
    rm(reef_area_ecoregions)


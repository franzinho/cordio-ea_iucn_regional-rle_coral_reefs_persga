##
##  Name:       create_regional_algal_coral_ratio.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using algal-coral ratio method
##
##  Approach:   Import percent cover data and calculate ratio
##                of macroalgae to live coral cover (ACR)
##
##              For each eco-region, ACR is calculated by:
##                algal_coral_ratio = FA / (FA + HC)
##
##              Summary object saved as *.rda
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. For this analysis, algal cover combines:
##                   - "Turf Alga"
##                   - "Macroalga"
##                 but excludes "Coralline Alga".  Should
##                 distinguish between encrusting and
##                 erect forms of coralline algae  [ fs: 2025-08-20 ]

##
## 1. Set up
##
 ## -- call to raw gcrmn percent cover data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/sessiles/"

  # set data file name
    data_file <- "regional_percent_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))

##
## 2. Groom data
##
  # have a look
    regional_percent_cover
# # A tibble: 3,171 × 13
   # Ecoregion longitude latitude region_name       island site_code
   # <chr>         <dbl>    <dbl> <chr>             <chr>  <chr>
 # 1 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 2 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 3 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 4 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 5 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 6 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 7 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-50
 # 8 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-51
 # 9 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-51
# 10 Hawaii        -178.     28.4 Northwestern Haw… Kure   KUR-51
# # ℹ 3,161 more rows
# # ℹ 7 more variables: date <date>, reef_zone <chr>,
# #   depth_bin <chr>, replicate <chr>, tier_1 <chr>,
# #   category_name <chr>, percent_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # set site id
    regional_percent_cover %<>%
      mutate(site_id = paste(region_name, island, site_code, reef_zone, sep = "_"))

  # set year
    regional_percent_cover %<>%
      mutate(Year = date %>% year())

 ## -- review algal & coral cover -- ## ----
  # get category names
    regional_percent_cover %>% pull(category_name) %>% unique()
 # [1] "Coralline Alga"       "Coral"
 # [3] "Macroalga"            "Sediment"
 # [5] "Turf Alga"            "Tape and wand"
 # [7] "Unclassified"         "Mobile Fauna"
 # [9] "Sessile Invertebrate" "Soft Coral"

  # set level 1 coral codes
    level1_corals <-
      c("Coral")

  # set level 1 algal codes
    level1_algae <-
      c("Macroalga",
        # "Coralline Alga",
        "Turf Alga")

  # filter & summarise
    regional_algal_coral_ratio <-
      regional_percent_cover %>%
        dplyr::filter(category_name %in% c(level1_corals, level1_algae)) %>%
        mutate(category_name = ifelse(category_name %in% level1_algae,
                                      "Algae", category_name)) %>%
        group_by(Ecoregion,
                 site_id,
                 Year,
                 category_name) %>%
        summarise(cover_sum = percent_cover %>% sum(na.rm = TRUE))

 ## -- calculate current algal-coral ratio -- ## ----
  # calculate acr
    regional_algal_coral_ratio <-
      regional_algal_coral_ratio %>%
        # dplyr::filter(!cover_sum %>% is.na()) %>%
      spread(category_name, cover_sum,
             fill = 0) %>%
      mutate(algal_coral_ratio = Algae / (Algae + Coral))
# # A tibble: 359 × 6
# # Groups:   Ecoregion, site_id, Year [359]
   # Ecoregion site_id            Year Algae Coral algal_coral_ratio
   # <chr>     <chr>             <dbl> <dbl> <dbl>             <dbl>
 # 1 Hawaii    Main Hawaiian Is…  2013  60.2 15.7              0.793
 # 2 Hawaii    Main Hawaiian Is…  2013  55.5 33.9              0.621
 # 3 Hawaii    Main Hawaiian Is…  2013  87.0  2.73             0.970
 # 4 Hawaii    Main Hawaiian Is…  2016  94.3  3.68             0.962
 # 5 Hawaii    Main Hawaiian Is…  2019 354.  15.4              0.958
 # 6 Hawaii    Main Hawaiian Is…  2013  63.3 17.6              0.782
 # 7 Hawaii    Main Hawaiian Is…  2019 337.  13.8              0.961
 # 8 Hawaii    Main Hawaiian Is…  2013  53.4 32.1              0.624
 # 9 Hawaii    Main Hawaiian Is…  2019 337.  46.6              0.878
# 10 Hawaii    Main Hawaiian Is…  2013  33.8 45.0              0.429
# # ℹ 349 more rows
# # ℹ Use `print(n = ...)` to see more rows

##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/algal_coral_ratio/"

  # save to file
    save(regional_algal_coral_ratio,
      file = paste0(save_locale, "regional_algal_coral_ratio.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove variables
    rm(level1_corals,
       level1_algae)

  # remove intermediate objects
    rm(regional_percent_cover)

  # remove core objects
    rm(regional_algal_coral_ratio)


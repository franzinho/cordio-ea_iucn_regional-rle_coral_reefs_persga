##
##  Name:       create_regional_fish_abundance_functional_groups.R
##
##  Objective:  Link gaspar functional and diet information
##                to pacific fish abundance data from noaa
##
##  Approach:   Call to gaspar traits and fish abundance
##                data, groom and join.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mathilde Maslin
##              CORDIO East Africa & SAS Marepolis
##
##  Date:       2025-09-16
##

##  Notes:      1. May need to update taxonomy from gaspar.
##                 Many missing taxa with left_join()  [ fs: 2025-09-16 ]

##
## 1. Set up
##
 ## -- call to gaspar traits -- ##
  # point to data locale
    data_locale <- "data_raw/biological/fishes/traits/"

  # point to data file
    data_file <- "global_traits_species.xlsx"

  # import traits
    global_traits_species <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


 ## -- call to fish abundance data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/fishes/"

  # point to data file
    data_file <- "regional_fish_abundance.rda"

  # load data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # review abundance data ----
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

 ## -- clean gaspar data -- ## ----
  # separate genus x species
    global_traits_species <-
      global_traits_species %>%
        separate(genus_species,
                 into = c("Genus", "specific_epithet"),
                 sep  = "_",
                 remove = FALSE)

  # set taxonomy to sentence
    global_traits_species %<>%
      rename(Family = family) %>%
      mutate(Family = Family %>% str_to_sentence(),
             Genus  = Genus  %>% str_to_sentence())

  # have a look
    global_traits_species %>% glimpse()
# Rows: 1,304
# Columns: 15
# $ gaspar_code              <chr> "972", "11793", "55868", "11811…
# $ Family                   <chr> "Belonidae", "Pomacentridae", "…
# $ genus_species            <chr> "ablennes_hians", "abudefduf_co…
# $ Genus                    <chr> "Ablennes", "Abudefduf", "Abude…
# $ specific_epithet         <chr> "hians", "concolor", "declivifr…
# $ constant_a               <dbl> 0.000000173, 0.024600000, 0.024…
# $ allometric_coefficient_b <dbl> 3.3227, 2.8500, 2.8500, 2.8500,…
# $ tl_correction_multiply   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 10, 10,…
# $ home_range               <chr> "w", "m", "m", "m", "m", "m", "…
# $ activity                 <chr> "b", "d", "d", "d", "d", "d", "…
# $ schooling                <chr> "m", "f", "f", "f", "f", "f", "…
# $ level_water              <chr> "h", "l", "l", "l", "l", "l", "…
# $ size_class               <dbl> 6, 3, 3, 4, 3, 4, 4, 4, 1, 1, 2…
# $ size_max                 <dbl> 140.0, 19.0, 18.0, 20.0, 15.0, …
# $ diet_gaspar              <chr> "fc", "om", "om", "om", "om", "…

    # get diet categories
      global_traits_species %>% pull(diet_gaspar) %>% unique()
# [1] "fc" "om" "pk" "im" "is" "hd" "hm"

    # convert diet categories
      global_traits_species %<>%
        mutate(diet_gaspar = diet_gaspar %>% factor() %>%
                               fct_recode(`Higher carnivore`      = "fc",
                                          Omnivore                = "om",                      
                                          Planktivore             = "pk",
                                          `Mobile invertivore`    = "im",
                                          `Sessile invertivore`   = "is",
                                          `Herbivore detritivore` = "hd",
                                          `Herbivore macroalgae`  = "hm"))   ## -- need to verify -- ##

    # set taxon name
      global_traits_species %<>%
        mutate(taxon_name = genus_species %>% str_to_sentence(),
               taxon_name = taxon_name    %>% str_replace_all("_", " "))      

    # rename size class column
      global_traits_species %<>%
        rename(size_group = size_class)

 ## -- join objects -- ## ----
  # link to abundance data
    regional_fish_abundance_functional_groups <-
      regional_fish_abundance %>%
        left_join(global_traits_species)

  # check for missing taxa
    regional_fish_abundance_functional_groups %>%
      dplyr::filter(diet_gaspar %>% is.na()) %>%
      pull(taxon_name) %>% unique() %>% sort()

##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/fishes/"

  # save to file
    save(regional_fish_abundance_functional_groups,
      file = paste0(save_locale, "regional_fish_abundance_functional_groups.rda"))


##
## 4. Clean up workspace
##
  # remove paths
    rm(data_locale,
       file_list,
       save_locale)

  # remove intermediate objects
    rm(regional_fish_abundance)

  # remove core objects
    rm(regional_fish_abundance_functional_groups)



##
##  Name:       create_criterion_d_acr_baseline.R
##
##  Objective:  Create reference (or "baseline") table for
##                algal-coral ratio
##
##  Approach:   Import percent cover data table and
##                summarise for estimating "baseline" or
##                reference values for analysis.
##
##              Output saved as *.rda
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2025-08-20
##

##
## 1. Set up
##
 ## -- call to algal coral ratio data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/algal_coral_ratio/"

  # set data file name
    data_file <- "regional_algal_coral_ratio.rda"

  # call to data
    load(paste0(data_locale, data_file))

##
## 2. Groom data
##
  # have a look
    regional_algal_coral_ratio
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

 ## -- summarise algal:coral ratio -- ## ----
  # set year for time-series cut-off
    y_thresh <- 2013

  # set cut off & summarise
    regional_algal_coral_ratio_summary <-
      regional_algal_coral_ratio %>%
        dplyr::filter(Year >= y_thresh) %>%
      group_by(Ecoregion) %>%
      reframe(first_year       = Year %>% min(na.rm = TRUE),
              recent_year      = Year %>% max(na.rm = TRUE),
              no_years         = Year %>% unique() %>% length(),
              acr_first_mean   = algal_coral_ratio[Year == first_year]  %>% mean(na.rm = TRUE),
              acr_first_sd     = algal_coral_ratio[Year == first_year]  %>%   sd(na.rm = TRUE),
              acr_recent_mean  = algal_coral_ratio[Year == recent_year] %>% mean(na.rm = TRUE),
              acr_recent_sd    = algal_coral_ratio[Year == recent_year] %>%   sd(na.rm = TRUE),
              acr_overall_mean = algal_coral_ratio %>% mean(na.rm = TRUE),
              acr_overall_sd   = algal_coral_ratio %>%   sd(na.rm = TRUE))

 ## -- review stats -- ## ----
  # review data summary
    regional_algal_coral_ratio_summary %>% quickview()
        # Ecoregion first_year recent_year no_years acr_first_mean
# 1          Hawaii       2013        2019        4      0.7811795
# 2    Line Islands       2015        2015        1      0.5532568
# 3 Mariana Islands       2014        2017        2      0.7979987
  # acr_first_sd acr_recent_mean acr_recent_sd acr_overall_mean
# 1    0.2106991       0.8544694    0.19447178        0.8176199
# 2    0.2119764       0.5532568    0.21197637        0.5532568
# 3    0.1514464       0.8845172    0.09719459        0.8337597
  # acr_overall_sd
# 1      0.1979565
# 2      0.2119764
# 3      0.1378301

 ## -- visualise -- ## ----
  # # open window
    # quartz("algal coral ratio", 7, 7)

  # # quick view
    # regional_algal_coral_ratio_summary %>%
      # dplyr::select(Ecoregion,
                    # acr_first_mean,
                    # acr_recent_mean,
                    # acr_overall_mean) %>%              
      # GGally::ggpairs()

 ## -- create baseline object -- ##
  # select reference values
    criterion_d_acr_baseline <-
      regional_algal_coral_ratio_summary %>%
        dplyr::select(Ecoregion,
                      acr_first_mean,
                      acr_first_sd)
# # A tibble: 6 × 3
  # Ecoregion                            acr_first_mean acr_first_sd
  # <chr>                                         <dbl>        <dbl>
# 1 Hawaii                                        0.781        0.211
# 2 Line Islands                                  0.553        0.212
# 3 Mariana Islands                               0.798        0.151
# 4 Marshall Islands                              0.435        0.188
# 5 Phoenix/Tokelau/Northern Cook Islan…          0.604        0.214
# 6 Samoa Islands                                 0.598        0.210


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_d_acr_baseline,
      file = paste0(save_locale, "criterion_d_acr_baseline.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove filter objects
    rm(y_thresh)

  # remove intermediate objects
    rm(regional_algal_coral_ratio,
       regional_algal_coral_ratio_summary)

  # remove core objects
    rm(criterion_d_acr_baseline)


##
##  Name:       create_criterion_a_data_table.R
##
##  Objective:  Standardise & format data for analysing criterion A:
##                Reduction in geographic distribution
##
##  Approach:   Call to clean data compilation, filter by time
##                period, geographic region, summarise and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Analysis assumes *current* state represents a 50-year
##                   period and that all sites were above threshold
##                   50 years ago.
##                 This is sub-criterion A1 (i.e. past 50 years)
##              2. A cut-off year for most recent year (e.g. 2010), sets
##                   all sites at same time period
##              3. *Current* hard coral cover is the average of all time-points
##                   from cut-off year (e.g. 2013)
##              4. Need to parse out geographic coordinates for spatial
##                   visualisation


##
## 1. Set up
##
 ## -- call to regional percent cover data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/sessiles/"

  # point to data file
    data_file <- "regional_percent_cover.rda"

  # load data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look at percent cover data ----
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

  # create site_id
    regional_percent_cover %<>%
      mutate(site_id = paste(region_name, island, site_code, reef_zone, depth_bin, sep = "_"))

 ## -- generate criteria data table object -- ## ----
  # get tier 1 categories
    regional_percent_cover %>% pull(category_name) %>% unique()
 # [1] "Coralline Alga"       "Coral"
 # [3] "Macroalga"            "Sediment"
 # [5] "Turf Alga"            "Tape and wand"
 # [7] "Unclassified"         "Mobile Fauna"
 # [9] "Sessile Invertebrate" "Soft Coral"

  # set level 1 code of interest
    level1_of_interest <-
      c("Coral")

  # review years
    regional_percent_cover %>% pull(date) %>% min(na.rm = TRUE)
# [1] "2013-07-12"

  # set year threshold
    y_thresh <-
      c("2013")

 ## -- summarise for criterion a                                         ##
 ##      no_years                = no. of time points                    ##
 ##      year_gap                = years between 1st and final record    ##
 ##      recent_coral_cover      = returns latest coral cover            ##
 ##      original_coral_cover    = returns earliest coral cover          ##
 ##      mean_coral_cover_thresh = 2013 onwards                       -- ##

  # filter & summarise
    criterion_a_data_table <-
      regional_percent_cover %>%
        mutate(Year = date %>% year()) %>%
        dplyr::filter(category_name %in% level1_of_interest) %>%
        rename(Latitude  = latitude,
               Longitude = longitude) %>%
        group_by(Ecoregion,
                 site_id) %>%
      summarise(first_year  = Year %>% min(na.rm = TRUE),
                recent_year = Year %>% max(na.rm = TRUE),
                lat         = Latitude  %>% mean(na.rm = TRUE),
                long        = Longitude %>% mean(na.rm = TRUE),
                no_years    = Year %>% unique() %>% length(),
                year_gap    = (recent_year - first_year) + 1,
                recent_coral_cover      = percent_cover %>% tail(n = 1),
                original_coral_cover    = percent_cover %>% head(n = 1),
                mean_coral_cover_thresh = percent_cover[Year >= y_thresh] %>%
                                                             mean(na.rm = TRUE))
                # n           = n(),
                # n_diff      = n - no_years)


  # check n diff
    criterion_a_data_table_coral_cover %>% pull(no_years) %>% unique() %>% sort()
# [1] 1 2 3

 ## -- get station frequencies per year -- ##
  # calculate frequencies
    station_frequencies <-
      criterion_a_data_table %>%
        group_by(recent_year) %>%
        reframe(n_stations = site_id %>% unique() %>% length())

  # set cumulative percentages
    station_frequencies %>%
      ungroup() %>%
      mutate(cumulative_percentage = 100*cumsum(n_stations) / sum(n_stations),
             cumulative_percentage = cumulative_percentage %>% round(2)) %>%
      data.frame()
  # recent_year n_stations cumulative_percentage
# 1        2013         44                 16.36
# 2        2014         33                 28.62
# 3        2015         66                 53.16
# 4        2016          6                 55.39
# 5        2017         33                 67.66
# 6        2018         29                 78.44
# 7        2019         58                100.00


 ## -- remove stations below time & cover thresholds -- ##
  # set coral cover threshold
    c_thresh <- 10

  # set first year threshold
    f_thresh <- 1998

  # filter naturally low cover sites
    criterion_a_data_table %<>%
      dplyr::filter(!original_coral_cover <= c_thresh,
                    !first_year           <  f_thresh)
# # A tibble: 182 × 12
   # Ecoregion site_id first_year recent_year   lat  long no_years
   # <chr>     <chr>        <dbl>       <dbl> <dbl> <dbl>    <int>
 # 1 Hawaii    Main H…       2013        2013  19.0 -156.        1
 # 2 Hawaii    Main H…       2013        2013  19.0 -156.        1
 # 3 Hawaii    Main H…       2013        2019  19.2 -156.        2
 # 4 Hawaii    Main H…       2013        2019  19.7 -155.        3
 # 5 Hawaii    Main H…       2013        2016  19.5 -155.        2
 # 6 Hawaii    Main H…       2013        2013  19.4 -155.        1
 # 7 Hawaii    Main H…       2013        2016  19.1 -156.        2
 # 8 Hawaii    Main H…       2013        2019  19.9 -156.        3
 # 9 Hawaii    Main H…       2013        2019  19.9 -156.        3
# 10 Hawaii    Main H…       2013        2019  20.9 -157.        3
# # ℹ 172 more rows
# # ℹ 5 more variables: year_gap <dbl>, mean_percent_cover <dbl>,
# #   recent_coral_cover <dbl>, original_coral_cover <dbl>,
# #   mean_coral_cover_thresh <dbl>
# # ℹ Use `print(n = ...)` to see more rows


 ## -- review data distribution per ecoregion -- ##
  # get frequencies
    criterion_a_data_table %>%
      group_by(Ecoregion) %>%
      reframe(n_stations = site_id %>% unique() %>% length())
# # A tibble: 6 × 2
  # Ecoregion                             n_stations
  # <chr>                                      <int>
# 1 Hawaii                                        59
# 2 Line Islands                                  23
# 3 Mariana Islands                               35
# 4 Marshall Islands                               7
# 5 Phoenix/Tokelau/Northern Cook Islands         17
# 6 Samoa Islands                                 41


 ## -- *current* coral cover can be set as average across years or        ##
 ##     single value (e.g. ave_coral).  Otherwise, average can            ##
 ##     be calculated of all time-points from cut-off year (e.g. 2013) -- ##

 ## -- remove stations with no data after cut-off -- ##
  # set recent year threshold
    r_thresh <- 2013

  # filter stations with no data
    criterion_a_data_table %<>%
      dplyr::filter(recent_year >= r_thresh)
# # A tibble: 182 × 12
   # Ecoregion site_id first_year recent_year   lat  long no_years
   # <chr>     <chr>        <dbl>       <dbl> <dbl> <dbl>    <int>
 # 1 Hawaii    Main H…       2013        2013  19.0 -156.        1
 # 2 Hawaii    Main H…       2013        2013  19.0 -156.        1
 # 3 Hawaii    Main H…       2013        2019  19.2 -156.        2
 # 4 Hawaii    Main H…       2013        2019  19.7 -155.        3
 # 5 Hawaii    Main H…       2013        2016  19.5 -155.        2
 # 6 Hawaii    Main H…       2013        2013  19.4 -155.        1
 # 7 Hawaii    Main H…       2013        2016  19.1 -156.        2
 # 8 Hawaii    Main H…       2013        2019  19.9 -156.        3
 # 9 Hawaii    Main H…       2013        2019  19.9 -156.        3
# 10 Hawaii    Main H…       2013        2019  20.9 -157.        3
# # ℹ 172 more rows
# # ℹ 5 more variables: year_gap <dbl>, mean_percent_cover <dbl>,
# #   recent_coral_cover <dbl>, original_coral_cover <dbl>,
# #   mean_coral_cover_thresh <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # set values
    criterion_a_data_table %<>%
      mutate(current_coral_cover = recent_coral_cover)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_a_data_table,
      file = paste0(save_locale, "criterion_a_data_table.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove parameter objects
    rm(y_thresh,
       c_thresh,
       f_thresh,
       r_thresh,
       level1_of_interest)

  # remove intermediate objects
    rm(regional_percent_cover)

  # remove core data objects
    rm(criterion_a_data_table)


##
##  Name:       create_criteria_a_reduction_geographic_distribution.R
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
##              5. Need to adjust creation code to match original
##                   criterion a table

##
## 1. Set up
##
 # ## -- call to trend data -- ##
  # # point to data locale
    # data_locale <- "data_intermediate/assessment/criteria/"

  # # point to data file name
    # data_file <- "hard_coral_percent_cover_trend.rda"

  # # load data
    # load(paste0(data_locale, data_file))

 ## -- call to relative reef areas -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # set data file name
    data_file <- "reef_area_ecoregions.rda"

  # call to data
    load(paste0(data_locale, data_file))


 ## -- call to percent cover data -- ##
  # point to data locale
    data_locale <- "data_intermediate/assessment/criteria/"

  # set data file name
    data_file <- "criterion_a_data_table.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    criterion_a_data_table
# # A tibble: 182 × 13
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
# # ℹ 6 more variables: year_gap <dbl>, mean_percent_cover <dbl>,
# #   recent_coral_cover <dbl>, original_coral_cover <dbl>,
# #   mean_coral_cover_thresh <dbl>, current_coral_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows


 ## -- match ecoregion names -- ## ----
  # from criterion table
    criterion_a_data_table %>% pull(Ecoregion) %>% unique()
# [1] "Hawaii"                               
# [2] "Line Islands"                         
# [3] "Mariana Islands"                      
# [4] "Marshall Islands"                     
# [5] "Phoenix/Tokelau/Northern Cook Islands"
# [6] "Samoa Islands"                        

  # check from reef area table
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Bismarck Sea"                         
 # [2] "Coral Sea"                            
 # [3] "East Caroline Islands"                
 # [4] "Fiji Islands"                         
 # [5] "Hawaii"                               
 # [6] "Line Islands"                         
 # [7] "Mariana Islands"                      
 # [8] "Marshall Islands"                     
 # [9] "New Caledonia"                        
# [10] "Ogasawara Islands"                    
# [11] "Phoenix/Tokelau/Northern Cook Islands"
# [12] "Rapa-Pitcairn"                        
# [13] "Samoa Islands"                        
# [14] "Society Islands"                      
# [15] "Solomon Archipelago"                  
# [16] "Solomon Sea"                          
# [17] "Southeast Papua New Guinea"           
# [18] "Southern Cook/Austral Islands"        
# [19] "Tonga Islands"                        
# [20] "Tuamotus"                             
# [21] "Vanuatu"                              
# [22] "West Caroline Islands" 


##
## 3. Evaluate criterion
##
  # set start threshold percent cover
    s_cover <- 1

  # set final threshold cover
    f_cover <- 30

  # set test interval
    t_interval <- 1

  # create empty object to hold results
    criterion_a_reduction_geographic_distribution <- tibble()

  # loop to calculate   # p=7  ## -- for testing -- ##
    for (p in seq(from = s_cover,
                  to   = f_cover,
                  by   = t_interval)){

      # set threshold
        dat <-
          criterion_a_data_table %>%
            mutate(threshold = p)

      # create ratio
        dat %<>%
          mutate(ratio = current_coral_cover / threshold)

     ## -- set to assign a 1 if it has collapsed -- ##
      # set colapse
        dat %<>%
          mutate(collapse = ifelse(ratio <= 1, 1, 0))

      # for each eco-region caculate proportion of sites which collapsed
        dat_collapse_raw <-
          dat %>%
            group_by(Ecoregion) %>%
            summarise(prop_collapse = (sum(collapse) / length(collapse)) %>% round(2))

     ## -- calculate weighted -- ##
      # link data
        dat_collapse_weighted <-
          dat_collapse_raw %>%
            left_join(reef_area_ecoregions %>%
                        dplyr::select(Ecoregion,
                                      prop_area))

      # calculate weighting
        dat_collapse_weighted %<>%
          mutate(weighted_prop = (prop_collapse * prop_area) %>% round(5))

     ## -- calculate regional statistics -- ##
      # calculate regional collapsed
        dat_collapse_regional <-
          dat_collapse_weighted %>%
            summarise(weighted_prop = weighted_prop %>% mean(na.rm = TRUE) %>% round(2),
                      prop_collapse = prop_collapse %>% mean(na.rm = TRUE) %>% round(2))

      # add weighted ecoregion identifier
        dat_collapse_regional %<>%
          mutate(Ecoregion = "Region")

     ## -- combine objects -- ##
      # join weighted & unweighted objects
        dat_collapse <-
          dat_collapse_weighted %>%
            dplyr::select(Ecoregion,
                          prop_collapse,
                          weighted_prop)

      # add regional statistics
        dat_collapse %<>%
          bind_rows(dat_collapse_regional)


     ## -- set threat categories -- ##
      # set threshold cover
        dat_collapse %<>%
          mutate(threshold = p)

      # set to long format
        dat_collapse %<>%
          rename(Unweighted = prop_collapse,
                 Weighted   = weighted_prop) %>%
          gather(Variable, prop_collapse,
                 -Ecoregion,
                 -threshold)

      # classify
        dat_collapse %<>%
          mutate(status = ifelse(prop_collapse >= 0.80,                        "CR",     NA),
                 status = ifelse(prop_collapse >= 0.50 & prop_collapse < 0.80, "EN", status),
                 status = ifelse(prop_collapse >= 0.30 & prop_collapse < 0.50, "VU", status),
                 status = ifelse(prop_collapse >= 0.27 & prop_collapse < 0.30, "NT", status),
                 status = ifelse(prop_collapse  < 0.27,                        "LC", status))


      # harvest results
        criterion_a_reduction_geographic_distribution %<>%
          bind_rows(dat_collapse)


       }


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_a_reduction_geographic_distribution,
      file = paste0(save_locale, "criterion_a_reduction_geographic_distribution.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove parameter objects
    rm(s_cover,
       f_cover,
       t_interval)

  # remove intermediate objects
    rm(criterion_a_data_table)

  # remove core data objects
    rm(criterion_a_reduction_geographic_distribution)


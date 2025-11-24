##
##  Name:       create_criterion_d_biotic_disruption_algal_coral_ratio.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using algal-coral ratio method
##
##  Approach:   Import data tables from original assessment, including:
##                 - raw algal coral ratio data table
##                 - baseline acr data
##              Compare with summary from gcrmn raw data
##
##              Loop through calculations from baseline estimations,
##                categorise threat status and summarise.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. Currently taking mean across depth strata [ fs: 2025-08-12 ]
##              2. Need to update year threshold `y_thresh`.
##                   Using cut-off from WIO as an example  [ fs: 2025-08-20 ]
##              3. Should include `relative_extent` as well as
##                   `relative_severity` for criterion D.  [ fs: 2025-08-20 ]

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


 ## -- call to baseline (reference) values -- ##
  # point to data locale
    data_locale <- "data_intermediate/assessment/criteria/"

  # set data file name
    data_file <- "criterion_d_acr_baseline.rda"

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

   # create acr table
     criterion_d_acr_data_table <-
       regional_algal_coral_ratio %>%
         dplyr::filter(Year >= y_thresh) %>%
       group_by(site_id,
                Ecoregion) %>%
       summarise(first_year  = Year %>% min(na.rm = TRUE),
                 recent_year = Year %>% max(na.rm = TRUE),
                 no_years    = Year %>% unique() %>% length(),
                 acr_recent  = algal_coral_ratio %>% head(1),
                 acr_mean    = algal_coral_ratio %>% mean(na.rm = TRUE))
# # A tibble: 276 × 7
# # Groups:   site_id [276]
   # site_id    Ecoregion first_year recent_year no_years acr_recent
   # <chr>      <chr>          <dbl>       <dbl>    <int>      <dbl>
 # 1 American … Samoa Is…       2015        2018        2      0.696
 # 2 American … Samoa Is…       2015        2015        1      0.464
 # 3 American … Samoa Is…       2015        2015        1      0.756
 # 4 American … Samoa Is…       2015        2018        2      0.579
 # 5 American … Samoa Is…       2015        2018        2      0.457
 # 6 American … Samoa Is…       2018        2018        1      0.675
 # 7 American … Samoa Is…       2015        2015        1      0.649
 # 8 American … Samoa Is…       2015        2015        1      0.263
 # 9 American … Samoa Is…       2015        2015        1      0.341
# 10 American … Samoa Is…       2015        2015        1      0.52 
# # ℹ 266 more rows
# # ℹ 1 more variable: acr_mean <dbl>
# # ℹ Use `print(n = ...)` to see more rows

##
## 3. Evaluate criterion
##
 ## -- modify column names -- ##
  # modify acr baseline
    criterion_d_acr_baseline %<>%
      rename(baseline_mean = acr_first_mean,
             baseline_sd   = acr_first_sd)

  # modify acr data table
    criterion_d_acr_data_table %<>%
      rename(current_acr = acr_recent)

 ## -- set collapse thresholds -- ##

 ## -- value of 0.833 sets algae as    ##
 ##      5/6 of coral cover         -- ##
  # set acr threshold
    threshold_acr <- 0.833

  # set iteration levels
    i_min <- 10
    i_max <- 1e3

  # set iteration interval
    i_interval <- 10

  # create empty object to hold results
    criterion_d_biotic_disruption_algal_coral_ratio <- tibble()

  # loop through intervals # i=3  ## -- for testing -- ##
    for(i in seq(from = i_min,
                 to   = i_max,
                 by   = i_interval)){

     ## -- calculate severity -- ##
      # set seed for reproducibility
        set.seed(i + 81)

      # randomly assign baseline values
        dat <-
          criterion_d_acr_data_table %>%
            left_join(criterion_d_acr_baseline %>%
                        dplyr::select(Ecoregion,
                                      baseline_mean,
                                      baseline_sd)) %>%
            mutate(baseline_acr = rnorm(1, mean = baseline_mean,
                                             sd = baseline_sd))

      # calculate relative severity
        dat %<>%
          mutate(relative_severity = 100 * (baseline_acr - current_acr) /
                                           (baseline_acr - threshold_acr))

      # bound by 0 and 100
        dat %<>%
          mutate(relative_severity = relative_severity %>% 
                                       scales::rescale(to = c(0, 100)))

     ## -- determine extent -- ##
      # get proportion of stations for relative severity classes
        dat %<>%
          group_by(Ecoregion) %>%
            summarise(rel_sev_30 = 100 * sum(relative_severity >= 30 &
                                             relative_severity < 50) /
                                               length(relative_severity),
                      rel_sev_50 = 100 * sum(relative_severity >= 50 &
                                             relative_severity < 80) /
                                               length(relative_severity),
                      rel_sev_80 = 100 * sum(relative_severity >= 80 &
                                             relative_severity <= 100) /
                                               length(relative_severity))


 ## -- correction from mishal 2024-04-04 -- ##
  # need to re-evaluate from updated script
# # correct rel severity levels
# t_coral2$rel_30 <- rowSums(t_coral2[, c("rel_sev_30", "rel_sev_50", "rel_sev_80")])
# t_coral2$rel_50 <- rowSums(t_coral2[, c("rel_sev_50", "rel_sev_80")])

      # correct rel severity levels
        dat %>%
          mutate(rel_30 = (rel_sev_30 + rel_sev_50 + rel_sev_80),
                 rel_50 = (rel_sev_50 + rel_sev_80))


     ## -- assign threat status -- ##
      # set status
        dat %<>%
          mutate(status_30 = ifelse(rel_sev_30 >= 80 & rel_sev_30 <= 100, 2,        NA),
                 status_50 = ifelse(rel_sev_50 >= 80 & rel_sev_50 <= 100, 3,        NA),
                 status_50 = ifelse(rel_sev_50 >= 50 & rel_sev_50 < 80,   2, status_50),
                 status_80 = ifelse(rel_sev_80 >= 50 & rel_sev_80 < 80,   3,        NA),
                 status_80 = ifelse(rel_sev_80 >= 80 & rel_sev_80 <= 100, 4, status_80),
                 status_80 = ifelse(rel_sev_80 >= 30 & rel_sev_80 < 50,   2, status_80))

      # set nas to 1
        dat %<>%
          mutate(status_30 = ifelse(is.na(status_30), 1, status_30),
                 status_50 = ifelse(is.na(status_50), 1, status_50),
                 status_80 = ifelse(is.na(status_80), 1, status_80))

     ## -- pick most severe categories -- ##
      # set max from status categories
        dat %<>%
         mutate(max_threat = pmax(status_30,
                                  status_50,
                                  status_80))

      # create conversion object for threat values
        threat_conversions <-
          tribble(~threat_value, ~status,
                              # 0,    "LC",
                              # 1,    "NT",
                              1, "NT/LC",
                              2,    "VU",
                              3,    "EN",
                              4,    "CR",
                              5,    "CO")

      # convert threat values
        dat %<>%
          left_join(threat_conversions %>%
                      rename(max_threat = threat_value))


      # set iteration
        dat %<>%
          mutate(Iteration = i)

      # harvest results
        criterion_d_biotic_disruption_algal_coral_ratio %<>%
          bind_rows(dat)


      }


##
## 4. Review results
##
  # summarise
    algal_coral_ratio_summary <-
    criterion_d_biotic_disruption_algal_coral_ratio %>%
      group_by(Ecoregion,
               status,
               max_threat) %>%
      summarise(n_categories = n()) %>%
      mutate(percent = 100 * n_categories / sum(n_categories))
# `summarise()` has grouped output by 'Ecoregion', 'status'. You
# can override using the `.groups` argument.
# # A tibble: 6 × 5
# # Groups:   Ecoregion, status [6]
  # Ecoregion                 status max_threat n_categories percent
  # <chr>                     <chr>       <dbl>        <int>   <dbl>
# 1 Hawaii                    EN              3          100     100
# 2 Line Islands              EN              3          100     100
# 3 Mariana Islands           EN              3          100     100
# 4 Marshall Islands          EN              3          100     100
# 5 Phoenix/Tokelau/Northern… EN              3          100     100
# 6 Samoa Islands             EN              3          100     100

      ## -- for each country/eco-region, need to take percentage    ##
      ##    of classifications which were either VU, EN, CR      -- ##

       # create a new column called threatened which
       # threat_prop$threatened<-threat_prop$max_threat
       # threat_prop$threatened[threat_prop$max_threat=='VU' |
       #                        threat_prop$max_threat=='EN' |
       #                        threat_prop$max_threat=='CR' ] <-'TH'
       #
       # #AND NOW SUM BASED ON COLUMN 'threatened'

       # set proportion
         algal_coral_ratio_summary %>%
           group_by(Ecoregion) %>%
           summarise(percent_threat = (percent[status == 'VU'|
                                               status == 'EN'|
                                               status == 'CR']) %>% sum(na.rm = TRUE))


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_d_biotic_disruption_algal_coral_ratio,
      file = paste0(save_locale, "criterion_d_biotic_disruption_algal_coral_ratio.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove filters
    rm(y_thresh,
       threshold_acr,
       i_min,
       i_max,
       i_interval)

  # remove intermediate objects
    rm(regional_algal_coral_ratio,
       criterion_d_acr_data_table,
       algal_coral_ratio_summary)

  # remove core objects
    rm(criterion_d_biotic_disruption_algal_coral_ratio)


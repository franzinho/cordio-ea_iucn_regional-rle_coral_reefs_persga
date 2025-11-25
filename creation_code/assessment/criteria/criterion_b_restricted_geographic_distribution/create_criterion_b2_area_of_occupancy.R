##
##  Name:       create_criteria_b2_area_of_occupancy.R
##
##  Objective:  Standardise & format data for analysing criterion B2:
##                Area of occupancy (AOO)
##
##  Approach:   Call to coral reef layer and ecoregions and create
##                grid to calculate the number of cells
##                occupied by coral reefs for each Ecoregion
##
##              Ecoregions are then classified into  threat categories:
##              - Critically Endangered (CR) if number of AOO units <= 2
##              - Endangered (EN) if number of AOO units == 20
##              - Vulnerable (VU) if number of AOO units <= 50
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. Criterion B2 measures the distribution of coral reef
##                 ecosystems based on a standard 10x10km grid. AOO is
##                 determined by counting the number of 10x10 km cells
##                 occupied by >1 km2 of the reef ecosystem within 12
##                 defined ecoregions.
##              2. This process basically entails creation of 10x10km grid,
##                 calculating grid uncertainty and getting the number of
##                 grid cells that are occupied by more than 1% of the
##                 reef ecosystem..
##              3. As the coral reef layer has already been extracted
##                 from the ecoregions, this simplifies the calculation
##                 of area.
##              4. Number of Improvements for gridUncertainty() sets
##                 number of iterations
##

##
## 1. Set up
##
  # call to additional functionality
    # library(redlistr)

 ## -- point to regional coral reefs coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # set data file name
    data_file <- "regional_coral_reefs.rda"

  # call to coral reefs
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- review data objects -- ## ----
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
## 3. Calculate area of occupancy & threat categories
##
 ## -- set parameters for analysis -- ## ----
  # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
# [1] "Gulf of Aden"                 "Northern and Central Red Sea"
# [3] "Southern Red Sea"

  # set grid size [ 10 km ]
    grid_size <- 10e3

  # set number of improvements
    n_improvements <- 5

  # set percent threshold
    p_thresh <- 1

 ## -- loop to calculate -- ## ----
  # create empty object to hold results
    criterion_b2_area_of_occupancy <- tibble()

  # loop ecoregions    # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

      # print progress to screen
        cat(paste0("...processing:  ", ecoregion_list[i],
                   " [ ", i, " of ", length(ecoregion_list), " ]\n"))

      # create area of occurrence grid
        aoo_grid <-
          regional_coral_reefs %>%
          filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32637) %>%
          as_Spatial() %>%
          redlistr::makeAOOGrid(grid.size = grid_size)

      # get grid uncertainty
        grid_uncertainty <-
          aoo_grid %>%
            redlistr::gridUncertainty(grid.size         = grid_size,
                                      n.AOO.improvement = n_improvements)

     # get min grid uncertainty
       min_aoo_number <-
         grid_uncertainty$min.AOO.grid$AOO.number

     # calculate uncertainty and apply 1% rule
       aoo <-
         regional_coral_reefs %>%
           filter(Ecoregion %in% ecoregion_list[i]) %>%
           st_transform(32637) %>%
           as_Spatial() %>%
         redlistr::getAOO(grid.size        = grid_size,
                          min.percent.rule = TRUE,
                          percent          = p_thresh)

      # set name
        dat <-
          tribble(       ~Ecoregion,  ~`Grid Uncertainty`, ~ `AOO`,
                  ecoregion_list[i],       min_aoo_number,     aoo)

      # harvest results
        criterion_b2_area_of_occupancy %<>%
          bind_rows(dat)


    }


 ## -- set threat categories -- ## ----
  # set threat categories
    threat_categories <-
      tribble(   ~Status,                      ~Description,
                    "CR",  "AOO is less or equal to 2 (CR)",
                    "EN", "AOO is less or equal to 20 (EN)",
                    "VU", "AOO is less or equal to 50 (VU)",
                 "NT/LC",  "AOO is greater than 50 (NT/LC)")

  # classify
    criterion_b2_area_of_occupancy %<>%
      mutate(Status = ifelse(AOO <= 2,                   "CR",     NA),
             Status = ifelse(AOO %>% between(2, 20),     "EN", Status),
             Status = ifelse(AOO %>% between(20, 50),    "VU", Status),
             Status = ifelse(AOO > 50,                "NT/LC", Status))
# # A tibble: 3 × 4
  # Ecoregion                    `Grid Uncertainty`   AOO Status
  # <chr>                                     <int> <int> <chr>
# 1 Gulf of Aden                                 70    58 NT/LC
# 2 Northern and Central Red Sea                543   458 NT/LC
# 3 Southern Red Sea                            483   403 NT/LC

  # join descriptions
    criterion_b2_area_of_occupancy %<>%
      left_join(threat_categories)


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # set file name
    data_file <- "criterion_b2_area_of_occupancy.rda"

  # save to file
    save(criterion_b2_area_of_occupancy,
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
       ecoregion_list,
       threat_categories)

  # remove criterion parameters
    rm(grid_size,
       n_improvements,
       p_thresh)

  # remove core data objects
    rm(criterion_b2_area_of_occupancy)


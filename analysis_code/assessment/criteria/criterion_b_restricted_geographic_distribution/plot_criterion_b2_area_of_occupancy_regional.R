##
##  Name:       plot_criterion_b2_area_of_occupancy_regional.R
##
##  Objective:  Visualise extent of occurrence for etp region
##
##  Approach:   Call to regional benthic taxa, coasline and
##              visualise.
##
##              Outputs saved as *.png
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-04-28
##

##  Notes:      1. Should add `bquote()` or ēxpression() for
##                   superscript of area labels
##              2. Need to include update for optimising calculation
##                                                 [ fs: 2025-11-15 ]

##
## 1. Set up
##
 ## -- load data objects -- ## ----

 ## -- call to custom  ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- point to regional coastline -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # import coastline
    load(paste0(data_locale, data_file))


 ## -- point to ecoregion combined coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # call to coral reefs
    load(paste0(data_locale, "regional_coral_reefs.rda"))


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

 ## -- calculate criterion b2 area of occupancy -- ## ----
  # get list of ecosystem units
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
# [1] "Gulf of Aden"                 "Northern and Central Red Sea"
# [3] "Southern Red Sea"

 ## -- set parameters for area analyses -- ##
  # set grid size [ 10 km ]
    grid_size <- 10e3

  # set number of improvements
    n_improvements <- 5

  # set percent threshold
    p_thresh <- 1

  # set minimum percent threshold
    m_thresh <-
      1e-4

  # set buffer distance for linear features
    b_dist <-
     300

 ## -- create empty objects to hold results -- ## ----
  # extent of occurrence
    aoo_grid <- list()

  # label centroids
    area_centroid <- tibble()

  # aoo
    eco_aoo <- tibble()

  # loop to calculate aoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

     # print progress to screen
       cat(paste0("...processing:  ", ecoregion_list[i], " [ ",
                  i, " of ", length(ecoregion_list), " ]\n" ))

     # create area of occurrence grid
       aoo_grid[[i]] <-
          regional_coral_reefs %>%
            dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32637) %>%
          st_union() %>%
          st_buffer(dist = b_dist) %>%
          as_Spatial() %>%
         redlistr::makeAOOGrid(grid.size        = grid_size,
                               min.percent.rule = TRUE,
                               percent          = m_thresh)

     # convert to sf
       aoo_grid[[i]] %<>%
         st_as_sf() %>%
         st_set_crs(32637)

    ## -- get grid uncertainty -- ##
     # calculate
       grid_uncertainty <-
         aoo_grid[[i]] %>%
           as_Spatial() %>%
           redlistr::gridUncertainty(grid.size         = grid_size,
                                     n.AOO.improvement = n_improvements)

     # get min grid uncertainty
       min_aoo_number <-
         grid_uncertainty$min.AOO.grid$AOO.number

     # calculate uncertainty and apply 1% rule
       aoo <-
         regional_coral_reefs %>%
           dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
           st_transform(32637) %>%
           st_union() %>%
           as_Spatial() %>%
         redlistr::getAOO(grid.size        = grid_size,
                          min.percent.rule = TRUE,
                          percent          = p_thresh)

     # set names
       aoo_dat <-
         tribble(       ~Ecoregion,  ~`Grid Uncertainty`, ~`AOO`,
                 ecoregion_list[i],       min_aoo_number,    aoo)

      # harvest results
        eco_aoo %<>%
          bind_rows(aoo_dat)

     ## -- set area annotation -- ##
       # get centroid
         dat <-
           aoo_grid[[i]] %>%
             st_as_sf() %>%
             st_transform(32637) %>%
             st_centroid() %>%
             st_coordinates()

       # harvest results
         area_centroid %<>%
           bind_rows(dat %>%
                       data.frame() %>% tibble() %>%
                       mutate(Ecoregion = ecoregion_list[i]))


     }

 ## -- summarise object -- ## ----
  # get centroids for ecoregions
    ecoregion_centroids <-
      regional_ecoregions %>%
        group_by(Ecoregion) %>%
        st_centroid() %>%
        st_coordinates()

  # bind to ecoregion names
    ecoregion_centroids %<>%
      cbind(regional_ecoregions$Ecoregion %>% tibble()) %>%
      rename(Ecoregion = ".")

  # put in order
    area_centroid %<>%
      group_by(Ecoregion) %>%
      summarise(reefs_x = X %>% mean(na.rm = TRUE),
                reefs_y = Y %>% mean(na.rm = TRUE))

  # add centroids
    area_centroid %<>%
      left_join(ecoregion_centroids) %>%
      rename(easting  = X,
             northing = Y)

 ## -- combine objects -- ## ----
  # combine centoids and area
    area_centroid %<>%
      left_join(eco_aoo)

  # set zeros to 1   ## -- manual fix before testing polygon errors -- ##
    area_centroid %<>%
      mutate(AOO = ifelse(AOO == 0, 1, AOO))

  # set ecoregion order
    area_centroid %<>%
      mutate(Ecoregion = Ecoregion %>% factor(levels = ecoregion_list)) %>%
      arrange(Ecoregion)

  # have a look
    area_centroid
# # A tibble: 3 × 7
  # Ecoregion reefs_x reefs_y easting northing `Grid Uncertainty`   AOO
  # <fct>       <dbl>   <dbl>   <dbl>    <dbl>              <int> <int>
# 1 Gulf of … 984706.  1.36e6  1.89e6 1384702.                 84    58
# 2 Northern… 194932.  2.78e6  2.59e5 2655467.                628   458
# 3 Southern… 702279.  1.87e6  6.70e5 1888372.                544   403

##
## 3. Visualise
##
 ## -- set colour palettes -- ## ----
   # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # get list of ecosystem units
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 4, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set criteria b1 eoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 4, "continuous")

  # set regional bounding box
    e_zoom <-
      # regional_coral_reefs %>%
      regional_ecoregions %>%
        # st_transform(32637) %>%
        st_bbox()

 ## -- combine aoo objects -- ## ----
  # create empty object to hold results
    criterion_b2_polygons <- st_sfc(crs = 32637) %>% st_sf()

  # loop                 # i=1 ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

      # subset object and convert to sf
        dat <-
          aoo_grid[[i]]

      # add ecoregion
        dat %<>%
          mutate(Ecoregion = ecoregion_list[i])

      # organise columns
        dat %<>%
          dplyr::select(Ecoregion,
                        cover,
                        geometry)

      # harvest results
        criterion_b2_polygons %<>%
          rbind(dat)


    }


 ## -- plot regional data -- ## ----
  # set region name
    region_name <- "PERSGA"

  # open window
    # quartz("criterion b2 aoo", 7, 7)

    # create figure
      ggplot() +
        geom_sf(aes(colour = Ecoregion,
                  fill   = Ecoregion),
               alpha = 0.2,
               data   = regional_ecoregions %>%
                         # st_transform(32637) %>%
                         st_crop(e_zoom)) +
        geom_sf(fill   = r_colour,
                colour = r_colour,
                size   = 0.1,
                alpha  = 0.5,
                data   = regional_coral_reefs %>%
                           # filter(Ecoregion %in% ecoregion_list[i]) %>%
                           st_transform(32637))  +
        geom_sf(fill   = "grey75",
                colour = "grey75",
                size   = 0.1,
                data   = regional_coastline %>%
                           st_transform(32637) %>%
                           st_crop(e_zoom)) +
        geom_sf(aes(colour = Ecoregion),
                fill   = NA,
                size   = 0.5,
                alpha  = 0.1,
                data   = criterion_b2_polygons) +
         geom_sf_text(aes(# colour = Ecoregion,
                          label  = paste0(Ecoregion, "\n",
                                          "AOO = ", AOO,
                                          " x10 km^2 units")),
                      position = "jitter",
                      size  = 2.5,
                      data  = area_centroid %>%
                                st_as_sf(coords = c("easting", "northing"),
                                         crs   = 32637)) +
         theme_void() +
         coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                  ylim = c(e_zoom[2], e_zoom[4]),
                  datum = st_crs(32637)) +
         ggspatial::annotation_scale() +
         scale_colour_manual(values = c_palette) +
         scale_fill_manual(values = c_palette) +
         labs(title    = region_name,
              subtitle = "Criterion B2: Area of Occupancy") +
         theme(legend.position = "none",
               plot.title      = element_text(hjust = 0.5),
               plot.subtitle   = element_text(hjust = 0.5,
                                              face  = "italic"))


 ## -- plot regional data -- ##
  # set save locale
    figure_locale <-
      paste0("figures/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # save to file
    ggsave(paste0(figure_locale,
                  "criterion_b2_area_of_occupancy_regional.png"),
      width  = 7,
      height = 7)


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       figure_locale)

  # remove plotting variables
    rm(ecoregion_list,
       e_zoom,
       c_palette,
       r_colour,
       c_colour,
       region_name)

  # remove intermediate objects
    rm(regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

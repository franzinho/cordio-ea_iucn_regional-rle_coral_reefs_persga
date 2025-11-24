##
##  Name:       plot_criterion_b2_area_of_occupancy.R
##
##  Objective:  Visualise extent of occurrence for  region
##
##  Approach:   Call to regional benthic taxa,  coasline and
##              visualise.
##
##              Outputs saved as *.png
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-13
##

##
## 1. Set up
##
 # -- call to custom  ecoregions -- ##
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

  # # point to data locale
  #   data_locale <- "data_intermediate/spatial/monitoring_sites/"
  #
  # # point to data file
  #   data_file <- "regional_monitoring_sites.rda"
  #
  # # import site positions
  #   load(paste0(data_locale, data_file))

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

 ## -- calculate criterion b2 area of occupancy -- ##
  # get list of ecosystem units
    ecoregion_list <-
      # regional_monitoring_sites %>% pull(Ecoregion) %>% unique()
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

 ## -- create empty object to hold results -- ##
  # extent of occurrence
    aoo_grid <- list()

  # label centroids
    area_centroid <- tibble()

  # aoo
    eco_aoo <- tibble()

  # loop to calculate aoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){
    # for(i in c(1:10,
    #            12:length(ecoregion_list))){

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

 ## -- combine objects -- ##
  # combine centoids and area
    area_centroid %<>%
      left_join(eco_aoo)

  # put in order
    area_centroid %<>%
      group_by(Ecoregion) %>%
      summarise(X = X %>% mean(na.rm = TRUE),
                Y = Y %>% mean(na.rm = TRUE),
                `Grid Uncertainty` = `Grid Uncertainty` %>% mean(na.rm = TRUE),
                AOO = AOO %>% mean(na.rm = TRUE))

  # set zeros to 1   ## -- manual fix before testing polygon errors -- ##
    area_centroid %<>%
      mutate(AOO = ifelse(AOO == 0, 1, AOO))

  # have a look
    area_centroid
# # A tibble: 3 × 5
  # Ecoregion                         X      Y `Grid Uncertainty`   AOO
  # <chr>                         <dbl>  <dbl>              <dbl> <dbl>
# 1 Gulf of Aden                 9.85e5 1.36e6                 84    58
# 2 Northern and Central Red Sea 1.95e5 2.78e6                628   458
# 3 Southern Red Sea             7.02e5 1.87e6                544   403

  # set ecoregion order
    area_centroid %<>%
      mutate(Ecoregion = Ecoregion %>% factor(levels = ecoregion_list)) %>%
      arrange(Ecoregion)


##
## 3. Visualise
##
 ## -- set colour palette -- ##
   # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # get list of ecosystem units
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set criteria b2 aoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

 ## -- plot regional data -- ##
  # set save locale
    figure_locale <-
      paste0("figures/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # create pdf
    CairoPDF(paste0(figure_locale, "criterion_b2_area_of_occupancy.pdf"), 7, 7)


  # open window
    # quartz("comoros b2 aoo", 7, 5)

  ## -- loop to generate figure -- ##
   # loop            # i=3  ## -- for testing -- ##
     for(i in 1:length(ecoregion_list)){
     # for(i in c(1:10,
     #            12:length(ecoregion_list))){

       # print progress to screen
         cat(paste0("...processing:  ", ecoregion_list[i],
                    " [ ", i, " of ", length(ecoregion_list), " ]\n"))

       # set ecoregion bounding box
         e_zoom <-
           # regional_coral_reefs %>%
           regional_ecoregions %>%
             dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
             # st_transform(32637) %>%
             st_bbox()

       # create figure
         p <-
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
                              filter(Ecoregion %in% ecoregion_list[i]) %>%
                            st_transform(32637))  +
           geom_sf(fill   = "grey75",
                   colour = "grey75",
                   size   = 0.1,
                   data   = regional_coastline %>%
                              st_transform(32637) %>%
                            st_crop(e_zoom)) +
           geom_sf(aes(colour = c_colour[1]),
                   fill   = NA,
                   size   = 0.5,
                   alpha  = 0.4,
                   data   = aoo_grid[[i]]) +
           # annotate(x      = area_centroid[i, ]$X,
                    # y      = area_centroid[i, ]$Y,
           annotate(x      = regional_ecoregions %>%
                               dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
                               st_centroid() %>% st_coordinates() %>% .[1],
                    y      = regional_ecoregions %>%
                               dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
                               st_centroid() %>% st_coordinates() %>% .[2],
                  # colour = c_colour[i],
                  colour = c_colour,
                  "text",
                  label = paste0("AOO = ", area_centroid[i, ]$AOO, " x10 km^2 units")) +
           theme_void() +
           coord_sf(xlim  = c(e_zoom[1], e_zoom[3]),
                    ylim  = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32637)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = ecoregion_list[i],
                subtitle = "Criterion B2: Area of Occupancy") +
           theme(legend.position = "none",
                 plot.title      = element_text(hjust = 0.5),
                 plot.subtitle   = element_text(hjust = 0.5,
                                                face  = "italic"))

    # plot image
      p %>% print()


   }

   # close device
     dev.off()


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
       c_colour)

  # remove intermediate objects
    rm(regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

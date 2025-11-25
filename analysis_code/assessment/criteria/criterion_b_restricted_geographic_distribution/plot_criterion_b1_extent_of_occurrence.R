##
##  Name:       plot_criterion_b1_extent_of_occurrence.R
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

##  Notes:      1. Should add `bquote()` or ēxpression() for superscript
##                 for area labels
##

##
## 1. Set up
##
 ## -- load data objects -- ## ----

 ## -- call to regional ecoregions -- ##
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


 ## -- point to ecoregions + custom  coral reefs -- ##
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


 ## -- calculate criterion b1 exent of occurrence -- ##
  # get list of ecosystem units
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
# [1] "Gulf of Aden"                 "Northern and Central Red Sea"
# [3] "Southern Red Sea"

 ## -- create empty objects to hold results -- ## ----
  # extent of occurrence
    # eco_eoo <- tibble()
    eco_eoo <- list()

  # label centroids
    area_centroid <- tibble()

  # area
    eoo_area <- tibble()

  # loop to calculate eoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){
    # for(i in c(1:10,
    #            12:length(ecoregion_list))){

      # print progress to screen
        cat(paste0("...processing ", ecoregion_list[i], " [ ",
                   i, " of ", length(ecoregion_list), " ]\n"))

      # create eoo object
        eco_eoo[[i]] <-
          regional_coral_reefs %>%
            dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
            st_transform(32637) %>%
            st_union() %>%
            # st_collection_extract("POLYGON") %>%
           as_Spatial() %>%
           redlistr::makeEOO()

     ## -- set area annotation -- ##
       # get centroid
         # area_centroid[[i]] <-
           dat <-
           eco_eoo[[i]] %>%
             st_as_sf() %>%
             st_transform(32637) %>%
             st_centroid() %>%
             st_coordinates()

       # # get coordinates
         # area_coordinates <-
           # area_centroid %>%
             # st_coordinates()

       # harvest results
         area_centroid %<>%
           bind_rows(dat %>%
                       data.frame() %>% tibble() %>%
                       mutate(Ecoregion = ecoregion_list[i]))

     ## -- calculate area -- ##
       # get area
         dat <-
           eco_eoo[[i]] %>%
             st_as_sf() %>%
             st_transform(32637) %>%
             st_area()

       # convert to km2
         dat %<>%
           tibble() %>%
           mutate(Ecoregion = ecoregion_list[i]) %>%
           rename(Area = ".") %>%
           mutate(Area = Area %>% as.numeric() / 1e3,
                  Area = Area %>% round(0))

       # harvest results
         eoo_area %<>%
           bind_rows(dat)


    }

 ## -- combine objects -- ## ----
  # combine centoids and area
    area_centroid %<>%
      left_join(eoo_area)

  # put in order
    area_centroid %<>%
      dplyr::select(Ecoregion,
                    easting  = X,
                    northing = Y,
                    Area)

  # have a look
    area_centroid
# # A tibble: 3 × 4
  # Ecoregion                     easting northing      Area
  # <chr>                           <dbl>    <dbl>     <dbl>
# 1 Northern and Central Red Sea  229324. 2730504. 331157028
# 2 Southern Red Sea              666718. 1877509. 259516678
# 3 Gulf of Aden                 1345095. 1418102. 231648662

  # convert areas
    area_centroid %<>%
      mutate(Area = Area / 1e3)

  # remove decimals
    area_centroid %<>%
      mutate(Area = Area %>% round(0))


##
## 3. Visualise
##
 ## -- set colour palette -- ## ----
   # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # set number of ecoregions
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set criteria b1 eoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

 ## -- plot regional data -- ## ----
  # set save locale
    figure_locale <-
      paste0("figures/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # create pdf
    CairoPDF(paste0(figure_locale, "criterion_b1_extent_of_occurrence.pdf"), 7, 7)

  # open window
    # quartz("criterion b1 eoo", 7, 7)

  ## -- loop to generate figure -- ##
   # loop            # i=3  ## -- for testing -- ##
     for(i in 1:length(ecoregion_list)){

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
           geom_sf(# aes(colour = c_colour[i]),
                   colour = c_colour[i],
                   fill   = NA,
                   size   = 0.5,
                   alpha  = 0.4,
                   data   = eco_eoo[[i]] %>%
                              st_as_sf() %>%
                              st_transform(32637)) +
           # annotate(x      = area_centroid[i, ]$easting,
                    # y      = area_centroid[i, ]$northing,
           annotate(x      = regional_ecoregions %>%
                               dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
                               st_centroid() %>% st_coordinates() %>% .[1],
                    y      = regional_ecoregions %>%
                               dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
                               st_centroid() %>% st_coordinates() %>% .[2],
                    "text",
                    label = paste0("EOO = ",
                                   area_centroid[i, ]$Area, " ",
                                   expression(km^2))) +
           theme_void() +
           coord_sf(xlim  = c(e_zoom[1], e_zoom[3]),
                    ylim  = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32637)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = ecoregion_list[i],
                subtitle = "Criterion B1: Extent of Occurrence") +
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
    rm(e_zoom,
       c_palette,
       r_colour,
       c_colour)

  # remove intermediate objects
    rm(regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

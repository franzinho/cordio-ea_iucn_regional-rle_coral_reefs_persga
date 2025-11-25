##
##  Name:       plot_criterion_b1_extent_of_occurrence_regional.R
##
##  Objective:  Visualise extent of occurrence for region
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
 ## -- load data objects -- ## ----

 ## -- call to regional  ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- load regional coastline -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # import coastline
    load(paste0(data_locale, data_file))


 ## -- point to regioinal coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # point to data file
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


 ## -- calculate criterion b1 exent of occurrence -- ##
  # get list of ecosystem units
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
# [1] "Gulf of Aden"                 "Northern and Central Red Sea"
# [3] "Southern Red Sea"

 ## -- create empty object to hold results -- ## ----
  # extent of occurrence
    # eco_eoo <- tibble()
    eco_eoo <- list()

  # label centroids
    area_centroid <- tibble()

  # area
    eoo_area <- tibble()

  # loop to calculate eoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

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
        dat <-
          eco_eoo[[i]] %>%
            st_as_sf() %>%
            st_transform(32637) %>%
            st_centroid() %>%
            st_coordinates()

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
  # get centroids for ecoregions
    ecoregion_centroids <-
      regional_ecoregions %>%
        group_by(Ecoregion) %>%
        st_centroid() %>%
        st_coordinates()

  # add ecoregion names
    ecoregion_centroids %<>%
      cbind(regional_ecoregions$Ecoregion %>% tibble()) %>%
      rename(Ecoregion = ".")

  # combine centoids and area
    area_centroid %<>%
      left_join(eoo_area)

  # put in order
    area_centroid %<>%
      dplyr::select(Ecoregion,
                    reefs_x = X,
                    reefs_y = Y,
                    Area)

  # join to centroids
    area_centroid %<>%
      left_join(ecoregion_centroids) %>%
      rename(easting  = X,
             northing = Y)

  # have a look
    area_centroid
# # A tibble: 3 × 6
  # Ecoregion                   reefs_x reefs_y   Area easting northing
  # <chr>                         <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
# 1 Gulf of Aden                 1.35e6  1.42e6 2.32e8 -1.28e7   1.75e7
# 2 Northern and Central Red S…  2.29e5  2.73e6 3.31e8 -9.16e6   2.20e7
# 3 Southern Red Sea             6.67e5  1.88e6 2.60e8 -1.15e7   2.15e7

  # convert areas
    area_centroid %<>%
      mutate(Area = Area / 1e3)

 ## -- set threat categories -- ## ----
  # classify
    criterion_b1_extent_of_occurrence <-
      area_centroid %>%
        rename(`EOO Area` = Area) %>%
      mutate(Status = ifelse(`EOO Area` <= 2e3,                  "CR",     NA),
             Status = ifelse(`EOO Area` %>% between(2e3, 20e3),  "EN", Status),
             Status = ifelse(`EOO Area` %>% between(20e3, 50e3), "VU", Status),
             Status = ifelse(`EOO Area` > 50e3,               "NT/LC", Status))
# # A tibble: 3 × 7
  # Ecoregion        reefs_x reefs_y `EOO Area` easting northing Status
  # <chr>              <dbl>   <dbl>      <dbl>   <dbl>    <dbl> <chr>
# 1 Gulf of Aden      1.35e6  1.42e6    231649. -1.28e7   1.75e7 NT/LC
# 2 Northern and Ce…  2.29e5  2.73e6    331157. -9.16e6   2.20e7 NT/LC
# 3 Southern Red Sea  6.67e5  1.88e6    259517. -1.15e7   2.15e7 NT/LC

##
## 3. Visualise
##
 ## -- set colour palette -- ## ----
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
        st_transform(32637) %>%
        st_bbox()

 ## -- combine eoo polygons -- ## ----
  # create empty object to hold results
    # criterion_b1_polygons <- st_sfc(crs = 4326) %>% st_sf()
    criterion_b1_polygons <- st_sfc(crs = 32637) %>% st_sf()

  # loop                 # i=1 ## -- for testing -- ##
    for(i in 1:length(eco_eoo)){

      # subset object and convert to sf
        dat <-
          eco_eoo[[i]] %>%
          st_as_sf() # %>%
          # st_set_crs(4326)
          # st_transform(4326)

      # add ecoregion
        dat %<>%
          mutate(Ecoregion = ecoregion_list[i])

      # organise columns
        dat %<>%
          dplyr::select(Ecoregion,
                        geometry)

      # harvest results
        criterion_b1_polygons %<>%
          rbind(dat)


    }

 ## -- round area measurements -- ## ----
  # remove decimals
    area_centroid %<>%
      mutate(Area = Area %>% round(0))

 ## -- plot regional data -- ##
  # set region name
    region_name <- "PERSGA"

  # open window
    # quartz("criterion b1 eoo", 7, 7)

       # create figure
         ggplot() +
           geom_sf(aes(colour = Ecoregion,
                       fill   = Ecoregion),
                   alpha = 0.2,
                   data   = regional_ecoregions %>%
                              st_transform(32637) %>%
                              st_crop(e_zoom)) +
           geom_sf(fill   = r_colour,
                   colour = r_colour,
                   size   = 0.1,
                   alpha  = 0.5,
                   data   = regional_coral_reefs %>%
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
                   alpha  = 0.4,
                   data   = criterion_b1_polygons %>%
                              st_transform(32637)) +
           geom_sf_text(aes(colour = Ecoregion,
                            label  = paste0(Ecoregion, "\n",
                                            "EOO = ", Area, " ",
                                            expression(km^2))),
                        position = "jitter",
                        size  = 2.5,
                        data  = area_centroid %>%
                                  st_as_sf(coords = c("easting", "northing"),
                                        crs   = 32637)) +
           theme_void() +
           coord_sf(xlim  = c(e_zoom[1], e_zoom[3]),
                    ylim  = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32637)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = region_name,
                subtitle = "Criterion B1: Extent of Occurrence") +
           theme(legend.position = "none",
                 plot.title      = element_text(hjust = 0.5),
                 plot.subtitle   = element_text(hjust = 0.5,
                                                face  = "italic"))


 ## -- save for wiki -- ## ----
  # set save locale
    figure_locale <-
      paste0("figures/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # save to file
    ggsave(paste0(figure_locale,
                  "criterion_b1_extent_of_occurrence_regional.png"),
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


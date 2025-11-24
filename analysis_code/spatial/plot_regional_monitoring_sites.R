##
##  Name:       plot_regional_monitoring_sites.R
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
##  Authors:    Franz Smith & Mathilde Maslin
##              CORDIO East Africa & SAS Marepolis
##
##  Date:       2025-09-02
##

##  Notes:      1. Should re-arrange ecoregion list order
##                                           [ fs: 2025-09-04 ]
##              2. For an overview of fishualize palettes:
##                 https://cran.r-project.org/web/packages/fishualize/vignettes/overview_colors.html
##

##
## 1. Set up
##
 # -- call to regional ecoregions -- ##
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


 ## -- point to regional coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to coral reefs
    load(paste0(data_locale, data_file))


 ## -- call to monitoring site positions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/monitoring_sites/"

  # point to data file
    data_file <- "regional_monitoring_sites.rda"

  # import site positions
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- review core objects -- ## ----
  # have a look at ecoregions
    regional_ecoregions
# Simple feature collection with 23 features and 9 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 6785945 ymin: 2246585 xmax: 16869370 ymax: 13330260
# Projected CRS: WGS 84 / Equi7 Oceania
# # A tibble: 23 × 10
   # Eco_code Ecoregion   Prov_code Province Rlm_code Realm Alt_code
 # *    <dbl> <chr>           <dbl> <chr>       <dbl> <chr>    <dbl>
 # 1    20134 Bismarck S…        31 Eastern…        6 Cent…      128
 # 2    20150 Coral Sea          35 Tropica…        6 Cent…      143
 # 3    20124 East Carol…        29 Tropica…        6 Cent…      139
 # 4    25147 Fiji Islan…        35 Tropica…        6 Cent…      144
 # 5    25152 Hawaii             37 Hawaii          7 East…      158
 # 6    20155 Line Islan…        39 Central…        7 East…      150
 # 7    20123 Mariana Is…        29 Tropica…        6 Cent…      140
 # 8    20158 Tuamotus           40 Southea…        7 East…      156
 # 9    20153 Marshall I…        38 Marshal…        7 East…      149
# 10    20149 New Caledo…        35 Tropica…        6 Cent…      145
# # ℹ 13 more rows
# # ℹ 3 more variables: Eco_code_x <dbl>, Lat_zone <chr>,
# #   geometry <GEOMETRY [m]>
# # ℹ Use `print(n = ...)` to see more rows

  # combine national s segments
    regional_ecoregions %<>%
      group_by(Ecoregion) %>%
      summarise(geometry = geometry %>% st_combine())


 ## -- clean up coastline object -- ## ----
  # have a look
    regional_coastline
# Geometry set for 1 feature
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -180 ymin: -31.28145 xmax: 180 ymax: 31.83093
# Geodetic CRS:  WGS 84
# MULTIPOLYGON (((-178.5596 -30.55172, -178.5596 ...

  # set to utm
    regional_coastline %<>%
      st_transform(32637)


 ## -- review site location data object -- ## ----
  # have a look
    regional_monitoring_sites
# Simple feature collection with 478 features and 6 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 8490589 ymin: 3257429 xmax: 16472470 ymax: 12831730
# Projected CRS: WGS 84 / Equi7 Oceania
# # A tibble: 478 × 7
   # Ecoregion Locality              Site  longitude latitude Source
 # * <chr>     <chr>                 <chr>     <dbl>    <dbl> <chr> 
 # 1 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.4 NOAA  
 # 2 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.4 NOAA  
 # 3 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.4 NOAA  
 # 4 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.5 NOAA  
 # 5 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.4 NOAA  
 # 6 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.4 NOAA  
 # 7 Hawaii    Northwestern Hawaiia… Kure…     -178.     28.4 NOAA  
 # 8 Hawaii    Main Hawaiian Islands Hawa…     -156.     20.3 NOAA  
 # 9 Hawaii    Main Hawaiian Islands Hawa…     -156.     20.2 NOAA  
# 10 Hawaii    Main Hawaiian Islands Hawa…     -156.     19.0 NOAA  
# # ℹ 468 more rows
# # ℹ 1 more variable: geometry <POINT [m]>
# # ℹ Use `print(n = ...)` to see more rows


##
## 3. Visualise
##
 ## -- check ecoregion taxonomy -- ## ----
  # check monitoring site object ecoregions
      regional_monitoring_sites %>% pull(Ecoregion) %>% unique()
#  [1] "Hawaii"
#  [2] "Line Islands"
#  [3] "Mariana Islands"
#  [4] "Marshall Islands"
#  [5] "Phoenix/Tokelau/Northern Cook Islands"
#  [6] "Samoa Islands"
#  [7] "Tuamotus"
#  [8] "Rapa-Pitcairn"
#  [9] "Society Islands"
# [10] "Tonga Islands"
# [11] "Marquesas"
# [12] "Southern Cook/Austral Islands"

  # check reef ecoregions
    # regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # check list from ecoregions object
    regional_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Bismarck Sea"
 # [2] "Coral Sea"
 # [3] "East Caroline Islands"
 # [4] "Fiji Islands"
 # [5] "Hawaii"
 # [6] "Line Islands"
 # [7] "Mariana Islands"
 # [8] "Marquesas"
 # [9] "Marshall Islands"
# [10] "New Caledonia"
# [11] "Ogasawara Islands"
# [12] "Phoenix/Tokelau/Northern Cook Islands"
# [13] "Rapa-Pitcairn"
# [14] "Samoa Islands"
# [15] "Society Islands"
# [16] "Solomon Archipelago"
# [17] "Solomon Sea"
# [18] "Southeast Papua New Guinea"
# [19] "Southern Cook/Austral Islands"
# [20] "Tonga Islands"
# [21] "Tuamotus"
# [22] "Vanuatu"
# [23] "West Caroline Islands"

 ## -- set colour palette -- ##
  # get list of ecoregions from criterion a data table
    ecoregion_list <-
      regional_monitoring_sites %>% pull(Ecoregion) %>% unique()
#  [1] "Hawaii"
#  [2] "Line Islands"
#  [3] "Mariana Islands"
#  [4] "Marshall Islands"
#  [5] "Phoenix/Tokelau/Northern Cook Islands"
#  [6] "Samoa Islands"
#  [7] "Tuamotus"
#  [8] "Rapa-Pitcairn"
#  [9] "Society Islands"
# [10] "Tonga Islands"
# [11] "Marquesas"
# [12] "Southern Cook/Austral Islands"

  # set order for ecoregions
    # ecoregion_list <-
    #   c("Hawaii",
    #     "Line Islands",
    #     "Phoenix/Tokelau/Northern Cook Islands",
    #     "Samoa Islands",
    #     "Marshall Islands",
    #     "Mariana Islands")

  # set number of ecoregions
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set colour for noaa sites
    n_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[4]

  # set colour for corail sites
    # c_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[3]
    c_colour <- fishualize::fish(option = "Antennarius_commerson", 5)[3]

  # set criteria b1 eoo colour
    b_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

 ## -- plot regional data -- ##
  # set save locale
    figure_locale <- "figures/spatial/"

  # create pdf
    CairoPDF(paste0(figure_locale, "regional_monitoring_sites.pdf"), 7, 7)

  # open window
    # quartz("regional monitoring sites", 7, 7)

  ## -- loop to generate figure -- ##
   # loop            # i=6  ## -- for testing -- ##
     for(i in 1:length(ecoregion_list)){
     # for(i in c(1, 3:length(ecoregion_list))){

       # print progress to screen
         cat(paste0("...processing:  ", ecoregion_list[i],
                    " [ ", i, " of ", length(ecoregion_list), " ]\n"))

       # set ecoregion bounding box
         e_zoom <-
           regional_ecoregions %>%
             dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
             st_bbox()

       # create figure
         p <-
         ggplot() +
           geom_sf(aes(colour = Ecoregion,
                       fill   = Ecoregion),
                   alpha = 0.2,
                   data   = regional_ecoregions %>%
                              st_crop(e_zoom)) +
           geom_sf(fill   = r_colour,
                   colour = r_colour,
                   size   = 0.1,
                   alpha  = 0.5,
                   data   = regional_coral_reefs %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           geom_sf(fill   = "grey75",
                   colour = "grey75",
                   size   = 0.1,
                   data   = regional_coastline %>%
                              st_crop(e_zoom)) +
           geom_sf(#aes(colour = c_colour),
                   colour = c_colour,
                   fill   = NA,
                   size   = 0.8,
                   alpha  = 0.4,
                   data   = regional_monitoring_sites %>%
                              dplyr::filter(Source %in% c("CORAIL")) %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           geom_sf(#aes(colour = n_colour),
                   colour = n_colour,
                   fill   = NA,
                   size   = 0.8,
                   alpha  = 0.4,
                   data   = regional_monitoring_sites %>%
                              dplyr::filter(Source %in% c("NOAA")) %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           theme_void() +
           coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                    ylim = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32637)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = ecoregion_list[i],
                subtitle = "Monitoring sites") +
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
## 3. Clean up workspace
##
  # remove paths
    rm(data_locale,
       figure_locale)

  # remove plotting variables
    rm(ecoregion_of_interest,
       # crs_details,
       e_zoom,
       c_palette,
       r_colour,
       c_colour,
       n_colour,
       b_colour)

  # remove intermediate objects
    rm(regional_coral_reefs,
       regional_coastline,
       regional_ecoregions)

  # remove core objects
    rm(regional_monitoring_sites)


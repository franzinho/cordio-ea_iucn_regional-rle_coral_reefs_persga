##
##  Name:       plot_rle_areas_of_assessment.R
##
##  Objective:  Visualise Areas of Assessment for RLE Evaluation
##
##  Approach:   Import shape files for ecoregions, eez and other
##              relevant spatial data and visualise.
##
##              Output saved as *.png
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2025-02-18
##

##  Notes:      1. position_dodge() does not seem to be working,
##                 including a manual fix for the moment [ fs: 2025-04-16 ]
##

##
## 1. Set up
##
 ## -- call to eezs -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/eez/"

  # point to data file
    data_file <- "eez_etp_pacific.rda"

  # load data
    load(paste0(data_locale, data_file))

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



 # ## -- call to monitoring site positions -- ##
  # # point to data locale
    # data_locale <- "data_intermediate/spatial/monitoring_sites/"

  # # point to data file
    # data_file <- "benthos_sites_locations.rda"

  # # import site positions
    # load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- set to sf -- ##
  # convert
    eez_etp_pacific %<>%
      st_as_sf()
# Simple feature collection with 13 features and 5 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: -122.1791 ymin: -30.54684 xmax: -76.98544 ymax: 32.62694
# Geodetic CRS:  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
# # A tibble: 13 × 6
   # ID    easting northing poly.area Region                      geometry
   # <chr>   <dbl>    <dbl>     <dbl> <fct>                  <POLYGON [°]>
 # 1 0 0    -110.   21.0       206.   México     ((-95.19363 16.16604, -9…
 # 2 1 0    -109.   10.3        36.0  Clipperton ((-105.8159 10.33115, -1…
 # 3 2 0     -81.0   6.69       15.5  Panamá     ((-82.89616 8.037698, -8…
 # 4 3 0     -86.8   6.46       44.5  Costa Rica ((-85.72778 11.06884, -8…
 # 5 4 0     -92.2  12.5         9.02 Guatemala  ((-92.19204 14.50488, -9…
 # 6 5 0     -89.7  11.8         7.90 El Salvad… ((-87.82179 13.41156, -8…
 # 7 6 0     -88.8   3.23        1.41 Joint Cos… ((-87.1365 2.147619, -87…
 # 8 7 0     -87.7  11.3         5.27 Nicaragua  ((-85.72204 11.06661, -8…
 # 9 9 0     -81.4   1.46        1.51 Joint Ecu… ((-83.80049 1.460102, -8…
# 10 10 0   -107.  -26.8        66.3  Rapa Nui   ((-105.2075 -23.12633, -…
# 11 11 0    -80.7   3.64       26.7  Colombia   ((-77.88092 7.211137, -7…
# 12 12 0    -90.8  -0.0466     69.1  Galápagos  ((-87.14512 2.150622, -8…
# 13 13 0    -82.2  -0.976      19.4  Ecuador    ((-83.55949 1.245726, -8…

  # transform
    eez_etp_pacific %<>%
      st_transform(32637)

 ## -- review ecoregion object -- ##
  # have a look
    regional_ecoregions
# Simple feature collection with 13 features and 9 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: -2230953 ymin: -675046.6 xmax: 2684599 ymax: 3758093
# Projected CRS: WGS 84 / UTM zone 15N
# # A tibble: 13 × 10
   # Eco_code Ecoregion           Prov_code Province Rlm_code Realm Alt_code
 # *    <dbl> <chr>                   <dbl> <chr>       <dbl> <chr>    <dbl>
 # 1    20167 Chiapas-Nicaragua          43 Tropica…        8 Trop…      163
 # 2    20165 Clipperton                 43 Tropica…        8 Trop…      164
 # 3    20169 Cocos Islands              43 Tropica…        8 Trop…      165
 # 4    20060 Cortezian                  11 Warm Te…        3 Temp…       56
 # 5    20173 Eastern Galapagos …        44 Galapag…        8 Trop…      160
 # 6    20171 Guayaquil                  43 Tropica…        8 Trop…      166
 # 7    20061 Magdalena Transiti…        11 Warm Te…        3 Temp…       57
 # 8    20166 Mexican Tropical P…        43 Tropica…        8 Trop…      167
 # 9    20168 Nicoya                     43 Tropica…        8 Trop…      168
# 10    20172 Northern Galapagos…        44 Galapag…        8 Trop…      161
# 11    20170 Panama Bight               43 Tropica…        8 Trop…      169
# 12    20164 Revillagigedos             43 Tropica…        8 Trop…      170
# 13    20174 Western Galapagos …        44 Galapag…        8 Trop…      162
# # ℹ 3 more variables: Eco_code_x <dbl>, Lat_zone <chr>,
# #   geometry <GEOMETRY [m]>

  # combine national s segments
    regional_ecoregions %<>%
      group_by(Ecoregion) %>%
      summarise(geometry = geometry %>% st_combine())


 ## -- review coastline object -- ##
  # have a look
    regional_coastline
# Geometry set for 1 feature
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -118.1342 ymin: -5.979108 xmax: -73.69754 ymax: 35.81038
# Geodetic CRS:  WGS 84
# MULTIPOLYGON (((-109.228 10.29214, -109.2342 10...

  # set to utm
    regional_coastline %<>%
      st_transform(32637)


##
## 3. Visualise
##
  # check reef ecoregions
    # regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # check list from ecoregions object
    regional_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Chiapas-Nicaragua"          "Clipperton"
 # [3] "Cocos Islands"              "Cortezian"
 # [5] "Eastern Galapagos Islands"  "Guayaquil"
 # [7] "Magdalena Transition"       "Mexican Tropical Pacific"
 # [9] "Nicoya"                     "Northern Galapagos Islands"
# [11] "Panama Bight"               "Revillagigedos"
# [13] "Western Galapagos Islands"

 ## -- organise ecoregions -- ##
  # set order for ecoregions
    ecoregion_list <-
      c("Cortezian",
        "Mexican Tropical Pacific",
        "Revillagigedos",
        "Clipperton",
        "Chiapas-Nicaragua",
        "Nicoya",
        "Cocos Islands",
        "Panama Bight",
        "Guayaquil",
        "Northern Galapagos Islands",
        "Eastern Galapagos Islands",
        "Western Galapagos Islands")

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

 ## -- adjust label ppositions -- ##
  # set adjustment distance
    v_adj <- 110e3

  # modify for eastern & western galápagos
    ecoregion_centroids %<>%
      mutate(Y = ifelse(Ecoregion == "Eastern Galapagos Islands", Y - v_adj, Y),
             Y = ifelse(Ecoregion == "Western Galapagos Islands", Y + v_adj, Y))    

 ## -- set colour palette -- ##
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

 ## -- plot regional data -- ##
  # set ecoregion bounding box
    e_zoom <-
      regional_ecoregions %>%
        st_bbox()

  # open window
    # quartz("gcrmn areas of assessment", 7, 7)

  ## -- loop to generate figure -- ##
   # create figure
     ggplot() +
       geom_sf(aes(colour = Ecoregion,
                   fill   = Ecoregion),
               alpha = 0.2,
               data   = regional_ecoregions %>%
                              st_crop(e_zoom)) +
       # geom_sf(fill   = r_colour,
               # colour = r_colour,
               # size   = 0.1,
               # alpha  = 0.5,
               # data   = regional_coral_reefs) +
        geom_sf(fill   = "grey75",
                colour = "grey75",
                size   = 0.1,
                data   = regional_coastline %>%
                           st_crop(e_zoom)) +
        geom_sf_text(aes(# colour = Ecoregion,
                         # group = Ecoregion,
                         label = paste0(Ecoregion)),
                     # position = position_dodge(width = v_adj),
                     data     = ecoregion_centroids %>%
                                  st_as_sf(coords = c("X", "Y"),
                                           crs   = 32637)) +
        theme_void() +
        coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                ylim = c(e_zoom[2], e_zoom[4]),
                datum = st_crs(32637)) +
        ggspatial::annotation_scale() +
        scale_colour_manual(values = c_palette) +
        scale_fill_manual(values = c_palette) +
        labs(title    = "Eastern Tropical Pacific",
              subtitle = "Areas of Assessment") +
        theme(legend.position = "none",
              plot.title      = element_text(hjust = 0.5),
              plot.subtitle   = element_text(hjust = 0.5,
                                             face  = "italic"))


 ## -- save for wiki -- ##
  # set save locale
    figure_locale <- "figures/spatial/"

  # save to file
    ggsave(paste0(figure_locale, "rle_areas_of_assessment.png"),
      width  = 7,
      height = 7)


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
       c_colour)

  # remove intermediate objects
    rm(regional_coral_reefs,
       regional_coastline,
       regional_ecoregions)

  # remove core objects
    rm(benthos_sites_locations_sf)

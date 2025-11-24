##
##  Project Name:  Global assessment of IUCN Red List of Ecosystems
##                   for coral reefs
##
##  Objective:     Analyse criteria and diagnostics for IUCN Red
##                   List of Ecosystems for coral reefs at regional
##                   levels and for individual Ecoregions (or
##                   other Area of Assessment)
##
##  Approach:      1. Import raw data and standardise format
##                 2. Analyse individual criteria for:
##                      A. Reduction in geographic distribution
##                      B. Restricted geographic distribution
##                      C. Environmental degradation
##                      D. Biotic disruption
##                 3. Compile graphic and analytical output for
##                      standardised reporting at:
##                        i.   Regional level
##                        ii.  Ecoregion (or other Area of Assessment)
##
##
##  Authors:       PERSGA Technical team, Franz Smith, Mishal Gudka and
##                   others
##                   CORDIO East Africa and University of Melbourne
##
##  Date:          2025-11-20
##

##  Notes:         1. This file is intended to provide a guide to
##                      the basic workflow of the project, attempting
##                      to 'integrate' the different steps necessary
##                      to conduct the analyses & create visual outputs

##
##  1. Set up the core functionality
##
  # clean up
    rm(list=ls())

  # call to file system operations
    library(fs)

  # call to core packages for data manipulation
    library(dplyr)
    library(tidyr)
    library(magrittr)
    library(purrr)
    library(lubridate)
    library(hms)
    library(stringr)
    library(forcats)

  # for importing different formats
    library(readr)
    library(readxl)

  # call to visualisation & output generation
    library(ggplot2)
    library(GGally)
    library(Cairo)
    library(extrafont)
    library(RColorBrewer)
    library(viridis)
    library(wesanderson)

  # functionality for spatial analyses
    library(sf)
    library(terra)
    library(tidyterra)
    # library(raster)  ## -- packages for legacy functions -- ##
    # library(rgdal)
    # library(rgeos)

  # create helper function for reviewing data
    quickview <- function(x, n = 3L) {head(data.frame(x), n = n)}

  # point to working directory        ## -- need to adjust for local copy -- ##
    # setwd(paste0("/Users/franzinho/Desktop/research/",
    #              "cordio-ea_iucn_regional-rle_coral_reefs_persga"))


##
## 2. Generate core data objects
##
 ## -- create ecoregion list -- ##
  # point to creation locale
    creation_locale <- "creation_code/spatial/ecoregions/"

  # create ecoregion list
    source(paste0(creation_locale, "create_ecoregion_list.R"))

 ## -- extract coastline -- ##
  # point to creation locale
    creation_locale <- "creation_code/geophysical/coastline/"

  # create coastline
    source(paste0(creation_locale, "create_regional_coastline.R"))


 ## -- create ecoregional boundaries -- ##
  # point to creation locale
    creation_locale <- "creation_code/spatial/ecoregions/"

  # create ecoregions for  region -- ##
    source(paste0(creation_locale, "create_regional_ecoregions.R"))


 ## -- create coral reef layer for defined ecoregions -- ##
  # point to creation locale
    creation_locale <- "creation_code/geophysical/coral_reefs/"

  # create coral reefs for region -- ##
    source(paste0(creation_locale, "create_regional_coral_reefs.R")) ## -- takes a while to download -- ##

  # calculate reef area by ecoregion
    source(paste0(creation_locale, "create_reef_area_ecoregions.R"))


 # ## -- create biological objects -- ##
  # # point to creation locale
    # creation_locale <- "creation_code/biological/sessiles/"

  # # create percent cover data
    # source(paste0(creation_locale, "create_regional_percent_cover.R"))

  # # assign ecoregions to percent cover
    # source(paste0(creation_locale, "assign_ecoregions_to_regional_percent_cover.R"))

  # # point to creation locale
   # creation_locale <- "creation_code/spatial/monitoring_sites/"

  # # create percent cover data
    # source(paste0(creation_locale, "create_regional_monitoring_sites.R"))


##
## 3. Analyse criterion A: Reduction of geographic region
##
 ## -- create data object -- ##
 # # point to creation locale
   # creation_locale <-
     # paste0("creation_code/assessment/criteria/",
            # "criterion_a_reduction_geographic_distribution/")

  # # create criterion a data table
    # source(paste0(creation_locale,
                  # "create_criterion_a_data_table.R"))

  # # generate object
    # source(paste0(creation_locale,
                  # "create_criterion_a_reduction_geographic_distribution.R"))

 # ## -- analyse criterion -- ##
  # # point to creation locale
    # creation_locale <- "creation_code/spatial/monitoring_sites/"

  # # create monitoring site object
    # source(paste0(creation_locale, "create_regional_monitoring_sites.R"))

  # # point to analysis locale
    # analysis_locale <-
      # paste0("analysis_code/assessment/criteria/",
             # "criterion_a_reduction_geographic_distribution/")

  # # plot number of collapsed sites
    # source(paste0(analysis_locale, "plot_number_of_collapsed_sites.R"))


##
## 4. Analyse criterion B: Restricted geographic distribution
##
 ## -- create data object -- ##
  # point to creation locale
    creation_locale <-
      paste0("creation_code/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # evaluate criterion extent of occurrence
    source(paste0(creation_locale, "create_criterion_b1_extent_of_occurrence.R"))

  # evaluate area of occupancy
    source(paste0(creation_locale, "create_criterion_b2_area_of_occupancy.R"))

 ## -- generate visuals -- ##
  # point to analysis locale
    analysis_locale <-
      paste0("analysis_code/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # generate plots for extent of occurrence for ecoregions
    source(paste0(analysis_locale, "plot_criterion_b1_extent_of_occurrence.R"))

  # generate plots for area of occupancy for ecoregions
    source(paste0(analysis_locale, "plot_criterion_b2_area_of_occupancy.R"))

  # generate regional plots for extent of occurrence for ecoregions
    source(paste0(analysis_locale, "plot_criterion_b1_extent_of_occurrence_regional.R"))

  # generate regional plots for area of occupancy for ecoregions
    source(paste0(analysis_locale, "plot_criterion_b2_area_of_occupancy_regional.R"))


##
## 5. Analyse criterion C: Environmental degradation
##
 # ## -- create degree heating weeks from rcp projections -- ##
 #  # point to creation locale
 #    creation_locale <-
 #      paste0("creation_code/geophysical/forecast/",
 #             "representative_concentration_pathways/")
 #
 #  # create degree heating weeks
 #    source(paste0(creation_locale, "create_representative_concentration_pathways.R"))
 #
 # ## -- evaluate criterion -- ##
 #  # point to creation locale
 #    creation_locale <-
 #      paste0("creation_code/assessment/criteria/",
 #             "criterion_c_environmental_degradation/")
 #
 #  # evaluate criterion
 #    source(paste0(creation_locale, "create_criterion_c_environmental_degradation.R"))


##
## 6. Analyse criterion D: Biotic disruption
##
 # -- create algal coral ratio -- ##
  # # point to creation locale
    # creation_locale <- "creation_code/biological/algal_coral_ratio/"

  # # create gcrmn benthic cover data
    # source(paste0(creation_locale, "create_regional_algal_coral_ratio.R"))


 # ## -- create baseline objects -- ##
  # # point to creation locale
    # creation_locale <-
      # "creation_code/criteria/criterion_d_biotic_disruption/baseline/"

  # # evaluate with algal-coral ratio
    # source(paste0(creation_locale, "create_criterion_d_acr_baseline.R"))

 # ## -- evaluate criterion -- ##
  # # point to creation locale
    # creation_locale <-
      # "creation_code/criteria/criterion_d_biotic_disruption/evaluation/"

  # # evaluate with coral cover method b
    # source(paste0(creation_locale,
                  # "create_criterion_d_biotic_disruption_coral_cover_method_b.R"))

  # # evaluate with algal-coral ratio
    # source(paste0(creation_locale,
                  # "create_criterion_d_biotic_disruption_algal_coral_ratio.R"))

  # # evaluate with key fish consumers
    # source(paste0(creation_locale,
                  # "create_criterion_d_biotic_disruption_key_fish_consumers.R"))


##
## 7. Summarise data & generate standardised output
##



##
## 8. Clean up workspace
##
  # remove paths
    rm(creation_locale,
       analysis_locale)

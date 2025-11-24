##
##  Name:       plot_number_of_collapsed_sites.R
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

##
## 1. Set up
##
 ## -- call to sensitivity analyses-- ##
  # point to data locale
    data_locale <- "data_intermediate/assessment/criteria/"

  # point to data file
    data_file <- "criterion_a_reduction_geographic_distribution.rda"

  # load data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    criterion_a_reduction_geographic_distribution
# # A tibble: 140 × 5
   # Ecoregion             threshold Variable prop_collapse status
   # <chr>                     <dbl> <chr>            <dbl> <chr> 
 # 1 Hawaii                        1 Unweigh…             0 LC    
 # 2 Line Islands                  1 Unweigh…             0 LC    
 # 3 Mariana Islands               1 Unweigh…             0 LC    
 # 4 Marshall Islands              1 Unweigh…             0 LC    
 # 5 Phoenix/Tokelau/Nort…         1 Unweigh…             0 LC    
 # 6 Samoa Islands                 1 Unweigh…             0 LC    
 # 7 Region                        1 Unweigh…             0 LC    
 # 8 Hawaii                        1 Weighted             0 LC    
 # 9 Line Islands                  1 Weighted             0 LC    
# 10 Mariana Islands               1 Weighted             0 LC    
# # ℹ 130 more rows
# # ℹ Use `print(n = ...)` to see more rows

  # review ecoregions
    criterion_a_reduction_geographic_distribution %>% pull(Ecoregion) %>% unique()
# [1] "Hawaii"                               
# [2] "Line Islands"                         
# [3] "Mariana Islands"                      
# [4] "Marshall Islands"                     
# [5] "Phoenix/Tokelau/Northern Cook Islands"
# [6] "Samoa Islands"                        
# [7] "Region"   


##
## 3. Visualise
##
  # set region name
    region_name <- "Pacific Ocean"

  # set variable of interest
    variable_of_interest <- "Unweighted"

  # set final threshold cover
    f_cover <- 30

  # get list of regions
    ecoregion_list <-
      # criterion_a_reduction_geographic_distribution %>% pull(Ecoregion) %>% unique()
      c("Region",
        "Hawaii",
        "Line Islands",                    
        "Phoenix/Tokelau/Northern Cook Islands",
        "Samoa Islands",
        "Marshall Islands",
        "Mariana Islands")


 ## -- create figure -- ##
  # open window
    # quartz("number of collapsed sites", 7, 7)

  # plot
    criterion_a_reduction_geographic_distribution %>%
      mutate(Ecoregion = Ecoregion %>% factor(levels = ecoregion_list)) %>%
      dplyr::filter(Variable %in% variable_of_interest) %>%
    ggplot(aes(threshold,
               prop_collapse,
               group     = Ecoregion,
               colour    = Ecoregion),
               linewidth = 2) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.8,
                ymin   = 0.00,
                ymax   = 0.25,
                fill   = "lightgreen",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.8,
                ymin   = 0.25,
                ymax   = 0.30,
                fill   = "lightblue",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.5,
                ymin   = 0.30,
                ymax   = 0.50,
                fill   = "lightyellow",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.8,
                ymin   = 0.50,
                ymax   = 0.80,
                fill   = "orange",
                colour = NA) +
      geom_rect(xmin   = -Inf,
                xmax   = Inf,
                alpha  = 0.6,
                ymin   = 0.80,
                ymax   = 1,
                fill   = "red",
                colour = NA) +
       geom_line() +
       geom_point(aes(shape = Ecoregion),
                  size = 2,
                  fill = "white") +
       scale_shape_manual(values = 1:(length(ecoregion_list) + 1)) +
       scale_colour_manual(values = 1:(length(ecoregion_list) + 1)) +
       theme_minimal() +
       scale_x_continuous(breaks = seq(0, f_cover, by = 2))+
       ylab('Proportion of collapsed sites') +
       xlab('% Hard coral cover threshold') +
       ggtitle(region_name) +
       labs(colour = "Ecoregion",
            shape  = "Ecoregion")+
       annotate("label", x = 1, y = 0.07, label = "LC") +
       annotate("label", x = 1, y = 0.27, label = "NT") +
       annotate("label", x = 1, y = 0.35, label = "VU") +
       annotate("label", x = 1, y = 0.55, label = "EN") +
       annotate("label", x = 1, y = 0.82, label = "CR") +
       theme(plot.title  = element_text(hjust = 0.5))

 ## -- save for wiki -- ##
  # set save locale
    figure_locale <- 
      paste0("figures/assessment/criteria/",
             "criterion_a_reduction_geographic_distribution/")

  # print to file
    ggsave(paste0(figure_locale, "number_of_collapsed_sites.png"),
      width  = 7,
      height = 7)


##
## 3. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       figure_locale)

  # remove intermediate objects
    rm(# ecoregion_list,
       region_name,
       f_cover,
       variable_of_interest)

  # remove core objects
    rm(criterion_a_reduction_geographic_distribution)


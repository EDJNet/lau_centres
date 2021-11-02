# gisco_id <- "IT_007010"
library("leaflet")



custom_ll_map_pop_grid <- function(gisco_id,
                                   pop_grid_sf,
                                   power = 2,
                                   differentiate = TRUE, 
                                   nudge_y = 2000,
                                   nudge_x = 500,
                                   hr = FALSE,
                                   use_leaflet = FALSE) {
  
  lau_sf <- ll_get_lau_eu(gisco_id = gisco_id,
                          silent = TRUE)
  
  centroid <- sf::st_centroid(x = lau_sf) 
  
  bbox <- sf::st_bbox(lau_sf)
  
  desired_bbox <- ll_bbox(sf = lau_sf, ratio = "16:9")
  
  intersect_sf <- ll_get_population_grid(
    year = 2018,
    match_sf = lau_sf,
    match_name = stringr::str_c(gisco_id,
                                "lau_2020",
                                "pop_grid_2018",
                                "intersects",
                                sep = "-"),
    join = sf::st_intersects,
    population_grid_sf = pop_grid_sf %>%
      dplyr::filter(stringr::str_detect(CNTR_ID,
                                        stringr::str_extract(gisco_id, "[A-Z]{2}"))),
    silent = TRUE
  )
  
  
  within_sf <- ll_get_population_grid(
    year = 2018,
    match_sf = lau_sf,
    match_name = stringr::str_c(gisco_id,
                                "lau_2020",
                                "pop_grid_2018",
                                "within",
                                sep = "-"),
    join = sf::st_within,
    population_grid_sf = intersect_sf,
    silent = TRUE 
  )
  
  boundary_grid_sf <- dplyr::anti_join(intersect_sf,
                                       within_sf %>% sf::st_drop_geometry(),
                                       by = "GRD_ID")
  
  
  pop_centroid <- ll_find_pop_centre(sf_location = lau_sf,
                                     sf_population_grid = intersect_sf,
                                     power = power)
  
  pop_centroid_adjusted <- ll_find_pop_centre(sf_location = lau_sf,
                                              sf_population_grid = intersect_sf,
                                              power = power,
                                              adjusted = TRUE)
  
  if (use_leaflet == TRUE) {
    
    if (differentiate == FALSE) {
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addAwesomeMarkers(data = pop_centroid,
                          popup="Population-weighted centre",
                          icon = makeAwesomeIcon(icon = "home",
                                                 markerColor = "blue",
                                                 library="ion")) %>% 
        addAwesomeMarkers(data = centroid,
                          popup="Centroid",
                          icon = makeAwesomeIcon(icon = "home",
                                                 markerColor = "purple",
                                                 library="ion")) %>% 
        
        # addAwesomeMarkers(data = pop_centroid_adjusted,
        #                   popup= "Adjusted population-weighted centre",
        #                   icon = makeAwesomeIcon(icon = "home",
        #                                          markerColor = "green",
        #                                          library="ion")) %>% 
        
        addPolygons(data = lau_sf,
                    fillOpacity = 0) %>% 
        addPolygons(data = boundary_grid_sf,
                    label = ~TOT_P_2018,
                    weight = 1,
                    group = "Boundary grid cells") %>% 
        addPolygons(data = within_sf,
                    label = ~TOT_P_2018,
                    weight = 1,
                    group = "Within boundary grid cells")
    } else {
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addAwesomeMarkers(data = pop_centroid,
                          popup="Population-weighted centre",
                          icon = makeAwesomeIcon(icon = "home",
                                                 markerColor = "blue",
                                                 library="ion")) %>% 
        addAwesomeMarkers(data = centroid,
                          popup="Centroid",
                          icon = makeAwesomeIcon(icon = "home",
                                                 markerColor = "purple",
                                                 library="ion")) %>% 
        
        addAwesomeMarkers(data = pop_centroid_adjusted,
                          popup= "Adjusted population-weighted centre",
                          icon = makeAwesomeIcon(icon = "home",
                                                 markerColor = "green",
                                                 library="ion")) %>% 
        
        addPolygons(data = lau_sf,
                    fillOpacity = 0) %>% 
        addPolygons(data = boundary_grid_sf,
                    label = ~TOT_P_2018,
                    weight = 1,
                    group = "Boundary grid cells",
                    color = "purple",
                    fillColor = "purple") %>% 
        addPolygons(data = within_sf,
                    label = ~TOT_P_2018,
                    weight = 1,
                    group = "Within boundary grid cells") %>% 
        addLayersControl(
          overlayGroups = c( "Boundary grid cells",
                             "Within boundary grid cells"),
          options = layersControlOptions(collapsed = FALSE)
        ) 
    }
    
  } else {
    
    base_cartolight_gg <- ggplot() +
      ggspatial::annotation_map_tile(type = "cartolight",
                                     zoomin = 0,
                                     cachedir = fs::path(ll_set_folder(), "ll_data")) +
      geom_sf(data = sf::st_as_sfc(desired_bbox),
              fill = NA,
              color = NA) + 
      geom_sf(data = intersect_sf,
              mapping = if (hr==TRUE) aes(colour = TOT_P_2018) else aes(fill = TOT_P_2018), alpha = 0.5) +
      geom_sf(data = lau_sf,
              colour = "darkred",
              size = 2,
              fill = NA,
              alpha = 0.8) +
      geom_sf(data = centroid,
              colour = "darkred",
              fill = "coral",
              size = 5,
              shape = 21,
              alpha = 0.8) +
      ggrepel::geom_label_repel(data = centroid,
                                mapping = aes(geometry = geometry,
                                              label  = "Centroid"),
                                nudge_y = nudge_y,
                                nudge_x = nudge_x, alpha = 0.8, seed = 1,
                                stat = "sf_coordinates") +
      geom_sf(data = pop_centroid,
              colour = "blue4",
              fill = "cornflowerblue",
              size = 5,
              shape = 21,
              alpha = 0.8) +
      ggrepel::geom_label_repel(data = pop_centroid,
                                mapping = aes(geometry = geometry,
                                              label  = "Pop centre"),
                                nudge_y = nudge_y,
                                nudge_x = nudge_x, alpha = 0.8, seed = 1,
                                stat = "sf_coordinates") +
      # geom_sf(data = pop_centroid_adjusted,
      #         colour = "darkgreen",
      #         fill = "green",
      #         size = 5,
      #         shape = 21,
      #         alpha = 0.8) +
      # ggrepel::geom_label_repel(data = pop_centroid_adjusted,
      #                           mapping = aes(geometry = geometry,
      #                                         label  = "Pop centre adjusted"),
      #                           nudge_y = nudge_y,
      #                           nudge_x = nudge_x, alpha = 0.8, seed = 1,
    #                           stat = "sf_coordinates") +
    
    theme_void()
    
    base_osm_gg <- ggplot() +
      ggspatial::annotation_map_tile(type = "osm",
                                     zoomin = 0,
                                     cachedir = fs::path(ll_set_folder(), "ll_data")) +
      geom_sf(data = sf::st_as_sfc(desired_bbox),
              fill = NA,
              color = NA) +
      geom_sf(data = lau_sf,
              colour = "darkred",
              size = 2,
              fill = NA,
              alpha = 0.8) +
      geom_sf(data = centroid,
              colour = "darkred",
              fill = "coral",
              size = 5,
              shape = 21,
              alpha = 0.8) +
      ggrepel::geom_label_repel(data = centroid,
                                mapping = aes(geometry = geometry,
                                              label  = "Centroid"),
                                nudge_y = nudge_y,
                                nudge_x = nudge_x, alpha = 0.8, seed = 1,
                                stat = "sf_coordinates") +
      geom_sf(data = pop_centroid,
              colour = "blue4",
              fill = "cornflowerblue",
              size = 5,
              shape = 21,
              alpha = 0.8) +
      ggrepel::geom_label_repel(data = pop_centroid,
                                mapping = aes(geometry = geometry,
                                              label  = "Pop centre"),
                                nudge_y = nudge_y,
                                nudge_x = nudge_x, alpha = 0.8, seed = 1,
                                stat = "sf_coordinates") +
      
      
      theme_void()
    
    
    (base_cartolight_gg +  base_osm_gg) +
      patchwork::plot_annotation(title = paste(lau_sf$GISCO_ID, lau_sf$LAU_NAME, sep = " - "), 
                                 subtitle = "Administrative boundaries, population grid, and centroid",
                                 caption = "Source: 
       © EuroGeographics for the administrative boundaries
       Population grid information: Eurostat, JRC
       Map tiles ©CARTO / ©OpenStreetMap
       Base map data by OpenStreetMap, under ODbL. ©OpenStreetMap contributors") 
  }
  
  
}

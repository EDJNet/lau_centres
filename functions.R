
## create functions to get countries in each of these datasets

get_pop_grid_countries <- function(pop_grid_year, db) {
  
  available_countries_pop_grid_table <- paste0("countries_pop_grid", "_", pop_grid_year)
  
  if (DBI::dbExistsTable(conn = db,
                         name = available_countries_pop_grid_table) == FALSE) {
    
    # DBI::dbRemoveTable(conn = db, name = available_countries_pop_grid_table)
    
    pop_grid_sf <- ll_get_population_grid(year = pop_grid_year,
                                          silent = TRUE)
    
    # for older grid datasets
    if (is.element("CNTR_CODE", colnames(pop_grid_sf))) {
      pop_grid_sf <- pop_grid_sf %>% 
        rename(CNTR_ID = CNTR_CODE)
    }
    
    pop_grid_countries <- pop_grid_sf %>% 
      sf::st_drop_geometry() %>% 
      dplyr::distinct(CNTR_ID) %>% 
      dplyr::filter(stringr::str_detect(string = CNTR_ID, pattern = "-", negate = TRUE)) %>% # for 2018 dataset
      dplyr::filter(stringr::str_detect(string = CNTR_ID, pattern = ":", negate = TRUE)) %>% # for 2006 dataset
      dplyr::distinct(CNTR_ID) %>% 
      dplyr::rename(country = CNTR_ID)
    
    DBI::dbWriteTable(db,
                      name = available_countries_pop_grid_table,
                      value = pop_grid_countries,
                      append = TRUE
    )
  } else {
    pop_grid_countries <- dplyr::tbl(src = db, available_countries_pop_grid_table) %>%
      tibble::as_tibble()
  }
  pop_grid_countries
}

get_lau_countries <- function(lau_year, db) {
  
  available_countries_lau_table <- paste0("countries_lau", "_", lau_year)
  
  if (DBI::dbExistsTable(conn = db,
                         name = available_countries_lau_table) == FALSE) {
    
    lau_countries <-  ll_get_lau_eu(silent = TRUE, year = lau_year) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::distinct(CNTR_CODE) %>% 
      dplyr::rename(country = CNTR_CODE)
    
    DBI::dbWriteTable(db,
                      name = available_countries_lau_table,
                      value = lau_countries,
                      append = TRUE
    )
  } else {
    lau_countries <- dplyr::tbl(src = db, available_countries_lau_table) %>%
      tibble::as_tibble()
  }
  lau_countries
}

get_gisco_id <- function(lau_year, db) {
  
  gisco_id_table <- paste0("gisco_id", "_", lau_year)
  
  if (DBI::dbExistsTable(conn = db,
                         name = gisco_id_table) == FALSE) {
    
    gisco_id_df <- ll_get_lau_eu(silent = TRUE, year = lau_year) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(country = CNTR_CODE, gisco_id = GISCO_ID) %>% 
      dplyr::arrange(gisco_id)
    
    DBI::dbWriteTable(db,
                      name = gisco_id_table,
                      value = gisco_id_df,
                      append = TRUE
    )
  } else {
    gisco_id_df <- dplyr::tbl(src = db, gisco_id_table) %>%
      tibble::as_tibble() %>% 
      dplyr::arrange(gisco_id)
  }
  
}

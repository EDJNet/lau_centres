library("dplyr")

combo_df <- fs::dir_ls("lau_nuts_area", type = "directory") %>% 
  fs::path_file() %>% 
  tibble::enframe(value = "folder_name", name = NULL) %>% 
  tidyr::separate(col = folder_name, into = c("lau_text","lau_year", "nuts_text", "nuts_year", "area_text")) %>% 
  dplyr::select(lau_year, nuts_year) %>% 
  dplyr::filter((lau_year > 2017 & nuts_year == 2016) | (lau_year == 2020 & nuts_year == 2021)) %>% 
  dplyr::mutate(pop_grid_year = 2018, power_centre = 2, adjusted = TRUE) %>% 
  dplyr::arrange(dplyr::desc(lau_year))
  
  
purrr::walk(
  .x = 1:nrow(combo_df),
  .f = function(i) {
    current_combo_df <- combo_df %>% dplyr::slice(i)
    
    adjusted_text <- ifelse(current_combo_df$adjusted,
                            "adjusted_intersection",
                            "full_intersection")
    
    current_filename <- paste0(paste("lau",
                                     current_combo_df$lau_year,
                                     "nuts", 
                                     current_combo_df$nuts_year,
                                     "pop",
                                     current_combo_df$pop_grid_year,
                                     "p",
                                     current_combo_df$power_centre,
                                     adjusted_text,
                                     sep = "_"),
                               ".html")
    
    current_html <- fs::path("_lau_centres_html", current_filename)
    
    if (fs::file_exists(current_html)==FALSE) {
      rmarkdown::render(input = "_process_lau_centres.Rmd",
                        params = list(
                          lau_year = current_combo_df$lau_year,
                          nuts_year = current_combo_df$nuts_year,
                          pop_grid_year = current_combo_df$pop_grid_year,
                          power_centre = current_combo_df$power_centre,
                          adjusted = current_combo_df$adjusted
                        ),
                        output_file = current_filename,
                        output_dir = "_lau_centres_html")
    }
  })

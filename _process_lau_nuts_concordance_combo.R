library("dplyr")

combo_df <- fs::dir_ls("lau_nuts_area", type = "directory") %>% 
  fs::path_file() %>% 
  tibble::enframe(value = "folder_name", name = NULL) %>% 
  tidyr::separate(col = folder_name, into = c("lau_text","lau_year", "nuts_text", "nuts_year", "area_text")) %>% 
  dplyr::select(lau_year, nuts_year) %>% 
  dplyr::filter(lau_year > 2017, nuts_year == 2016)
  
  
purrr::walk(
  .x = 1:nrow(combo_df),
  .f = function(i) {
    current_combo_df <- combo_df %>% dplyr::slice(i)
    
    current_filename <- paste0(paste("lau",
                                     current_combo_df$lau_year,
                                     "nuts",
                                     current_combo_df$nuts_year,
                                     sep = "_"),
                               ".html")
    
    current_html <- fs::path("_lau_nuts_concordance_combo_html", current_filename)
    
    if (fs::file_exists(current_html)==FALSE) {
      rmarkdown::render(input = "_process_lau_nuts_concordance_combo.Rmd",
                        params = list(
                          lau_year = current_combo_df$lau_year,
                          nuts_year = current_combo_df$nuts_year
                        ),
                        output_file = current_filename,
                        output_dir = "_lau_nuts_concordance_combo_html")
    }
  })

library("dplyr")
combo_df <- tibble::tribble(~lau_year, ~nuts_year,
                            2020, 2021,
                            2019, 2021,
                            2018, 2021,
                            2017, 2021,
                            2016, 2021,
                            2020, 2016,
                            2019, 2016,
                            2018, 2016,
                            2017, 2016,
                            2016, 2016)

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
    
    current_html <- fs::path("_lau_nuts_area_html", current_filename)
    
    if (fs::file_exists(current_html)==FALSE) {
      rmarkdown::render(input = "_process_lau_nuts_area.Rmd",
                        params = list(
                          lau_year = current_combo_df$lau_year,
                          nuts_year = current_combo_df$nuts_year
                        ),
                        output_file = current_filename,
                        output_dir = "_lau_nuts_area_html")
    }
  })

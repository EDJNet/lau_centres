library("piggyback")


pb_new_release(repo = "EDJNet/lau_centres",
               tag = "lau_2020_nuts_2021_pop_grid_2018")

pb_upload(fs::path("lau_centres", "lau_2020_nuts_2021_pop_2018_p_2_adjusted_intersection.csv"), 
          repo = "EDJNet/lau_centres", 
          tag = "lau_2020_nuts_2021_pop_grid_2018")

pb_new_release(repo = "EDJNet/lau_centres",
               tag = "lau_2020_nuts_2016_pop_grid_2018")

pb_upload(fs::path("lau_centres", "lau_2020_nuts_2016_pop_2018_p_2_adjusted_intersection.csv"), 
          repo = "EDJNet/lau_centres", 
          tag = "lau_2020_nuts_2016_pop_grid_2018")

pb_new_release(repo = "EDJNet/lau_centres",
               tag = "lau_2019_nuts_2016_pop_grid_2018")

pb_upload(fs::path("lau_centres", "lau_2019_nuts_2016_pop_2018_p_2_adjusted_intersection.csv"), 
          repo = "EDJNet/lau_centres", 
          tag = "lau_2019_nuts_2016_pop_grid_2018")

pb_new_release(repo = "EDJNet/lau_centres",
               tag = "lau_2018_nuts_2016_pop_grid_2018")

pb_upload(fs::path("lau_centres", "lau_2018_nuts_2016_pop_2018_p_2_adjusted_intersection.csv"), 
          repo = "EDJNet/lau_centres", 
          tag = "lau_2018_nuts_2016_pop_grid_2018")



#pb_download_url(file = "lau_2020_nuts_2021_pop_2018_p_2_adjusted_intersection.csv", repo = "EDJNet/lau_centres", tag = "lau_2020_nuts_2021_pop_grid_2018")

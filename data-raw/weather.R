## code to prepare `DATASET` dataset goes here
require(dplyr)
weather <- readr::read_csv(
  file = "data-raw/stn_a.csv"
) %>%
  dplyr::mutate(
    station = "wakanda",
    .before = everything()
  ) %>%
  tibble::as_tibble()
usethis::use_data(weather, overwrite = TRUE)

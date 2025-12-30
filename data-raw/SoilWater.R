## code to prepare `DATASET` dataset goes here
require(dplyr)
SoilWater <- readr::read_csv(
  file = "data-raw/soil_water.csv"
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate_at(.vars = c("cra", "crb"), .funs = round, digits = 6)
usethis::use_data(SoilWater, overwrite = TRUE)

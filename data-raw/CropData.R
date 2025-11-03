## code to prepare `CropData` dataset goes here
require(dplyr)

CropData <- readr::read_tsv(
  file = "data-raw/CropData.tsv",
  locale = readr::locale(encoding = "latin1")
  #quote = "none"
) %>%
  tibble::as_tibble() %>%
  mutate(
    fmt = .get_sprintf_format(fmt)
  )

usethis::use_data(CropData, overwrite = TRUE)

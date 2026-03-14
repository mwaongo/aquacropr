## code to prepare `ManData` dataset goes here
require(dplyr)

ManData <- readr::read_tsv(
  file = "data-raw/ManData.tsv",
  locale = readr::locale(encoding = "latin1")
) %>%
  tibble::as_tibble() %>%
  mutate(
    fmt = .get_sprintf_format(fmt)
  )

usethis::use_data(ManData, overwrite = TRUE)

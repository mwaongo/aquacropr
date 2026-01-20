## code to prepare `SWOData` dataset goes here

# Soil Water Content Default Values
# Wilting point (WP) and electrical conductivity (ECe) for standard soil textures

SWOData <- data.frame(
  texture = c(
    "sand",
    "loamy sand",
    "sandy loam",
    "loam",
    "silt loam",
    "silt",
    "sandy clay loam",
    "clay loam",
    "silty clay loam",
    "sandy clay",
    "silty clay",
    "clay",
    "impermeable"
  ),
  wp = c(6, 8, 10, 15, 13, 9, 20, 23, 23, 27, 32, 39, 0.1),
  ece = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  stringsAsFactors = FALSE
)

# Add row names for easy access
rownames(SWOData) <- NULL

# Save to data/
usethis::use_data(SWOData, overwrite = TRUE)

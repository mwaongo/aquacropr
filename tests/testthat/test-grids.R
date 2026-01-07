library(testthat)
library(sf)

test_that("st_grid creates square polygons from sf points", {
  points <- st_as_sf(data.frame(x = c(1, 3, 5), y = c(1, 2, 1)),
    coords = c("x", "y"), crs = 32630
  )

  result <- st_grid(points, cellsize = 2, square = TRUE)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3)
  expect_equal(as.character(st_geometry_type(result, by_geometry = FALSE)), "POLYGON")
})

test_that("st_grid coerces data frame to sf", {
  df <- data.frame(lon = c(1, 2), lat = c(1, 2))

  result <- st_grid(df, cellsize = 1, coords = c("lon", "lat"), crs = 4326)

  expect_s3_class(result, "sf")
  expect_equal(st_crs(result)$input, "EPSG:4326")
})

test_that("st_grid creates correctly sized and centered squares", {
  point <- st_as_sf(data.frame(x = 10, y = 20),
    coords = c("x", "y"), crs = 32630
  )

  result <- st_grid(point, cellsize = 4)
  bbox <- st_bbox(result)

  expect_equal(bbox["xmax"] - bbox["xmin"], 4)
  expect_equal(bbox["xmin"], 8) # 10 - 4/2
  expect_equal(bbox["xmax"], 12) # 10 + 4/2
})

test_that("st_grid errors on non-point geometries", {
  line <- st_sf(geometry = st_sfc(
    st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    crs = 32630
  ))

  expect_error(st_grid(line), "x must contain POINT or MULTIPOINT geometries")
})

test_that("st_grid errors on invalid inputs", {
  expect_error(
    st_grid(list(x = 1, y = 1)),
    "x must be an sf object or a data frame"
  )

  df <- data.frame(a = 1, b = 2)
  expect_error(
    st_grid(df, coords = c("x", "y")),
    "Coordinate columns .* not found"
  )
})

test_that("st_grid errors for hexagonal grid", {
  points <- st_as_sf(data.frame(x = 1, y = 1),
    coords = c("x", "y"), crs = 32630
  )

  expect_error(
    st_grid(points, square = FALSE),
    "Hexagonal grid not yet implemented"
  )
})

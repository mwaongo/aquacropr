st_grid <- function(x, cellsize = 1, square = TRUE, coords = c("x", "y"), crs = NULL) {
  # Input validation and coercion
  if (!inherits(x, "sf")) {
    if (is.data.frame(x)) {
      # Check if coords columns exist
      if (!all(coords %in% names(x))) {
        stop(
          "Coordinate columns '", paste(coords, collapse = "', '"),
          "' not found in data frame"
        )
      }
      # Coerce to sf
      x <- st_as_sf(x, coords = coords, crs = crs)
    } else {
      stop("x must be an sf object or a data frame")
    }
  }

  # Check geometry type
  geom_type <- as.character(st_geometry_type(x, by_geometry = FALSE))
  if (!geom_type %in% c("POINT", "MULTIPOINT")) {
    stop("x must contain POINT or MULTIPOINT geometries")
  }

  # Create grids
  if (square) {
    # Create squares
    grids <- lapply(1:nrow(x), function(i) {
      coords <- st_coordinates(x$geometry[i])
      x_coord <- coords[1]
      y_coord <- coords[2]

      st_polygon(list(matrix(c(
        x_coord - cellsize / 2, y_coord - cellsize / 2,
        x_coord + cellsize / 2, y_coord - cellsize / 2,
        x_coord + cellsize / 2, y_coord + cellsize / 2,
        x_coord - cellsize / 2, y_coord + cellsize / 2,
        x_coord - cellsize / 2, y_coord - cellsize / 2
      ), ncol = 2, byrow = TRUE)))
    })
  } else {
    # Hexagonal grid not yet implemented
    stop("Hexagonal grid not yet implemented. Use square = TRUE")
  }

  # Return sf object
  st_sf(geometry = st_sfc(grids, crs = st_crs(x)))
}

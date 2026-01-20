#' Create Grid from Points
#'
#' Creates a regular grid (square or hexagonal) centered on point geometries.
#'
#' @param x An sf object with POINT or MULTIPOINT geometries, or a data frame
#'   with coordinate columns.
#' @param cellsize Numeric. Size of grid cells (in units of the CRS).
#' @param square Logical. If TRUE, creates square grids. If FALSE, creates
#'   hexagonal grids (not yet implemented).
#' @param coords Character vector of length 2. Names of coordinate columns
#'   if x is a data frame. Default is c("x", "y").
#' @param crs Coordinate reference system. Used when coercing data frame to sf.
#' @param keep_attributes Logical. If TRUE (default), preserves all attribute
#'   columns from the input. If FALSE, returns only the grid geometry.
#'
#' @return An sf object with POLYGON geometries representing grid cells.
#'   If keep_attributes = TRUE, includes all original columns from x.
#'
#' @details
#' If x is a data frame, it will be converted to an sf object using the
#' specified coordinate columns and CRS. The function creates square grid
#' cells centered on each point geometry.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # From sf object
#' points <- st_as_sf(data.frame(x = 1:5, y = 1:5), coords = c("x", "y"))
#' grid <- st_grid(points, cellsize = 0.5)
#'
#' # From data frame
#' df <- data.frame(lon = c(-5, 0, 5), lat = c(10, 15, 20), name = c("A", "B", "C"))
#' grid <- st_grid(df, cellsize = 1, coords = c("lon", "lat"), crs = 4326)
#'
#' # Without attributes
#' grid_only <- st_grid(df,
#'   cellsize = 1, coords = c("lon", "lat"),
#'   crs = 4326, keep_attributes = FALSE
#' )
#' }
#'
#' @importFrom sf st_as_sf st_geometry_type st_coordinates st_polygon st_sf st_sfc st_crs st_drop_geometry
#' @export
st_grid <- function(x,
                    cellsize = 1,
                    square = TRUE,
                    coords = c("x", "y"),
                    crs = NULL,
                    keep_attributes = TRUE) {
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
      x <- sf::st_as_sf(x, coords = coords, crs = crs)
    } else {
      stop("x must be an sf object or a data frame")
    }
  }

  # Check geometry type
  geom_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  if (!geom_type %in% c("POINT", "MULTIPOINT")) {
    stop("x must contain POINT or MULTIPOINT geometries")
  }

  # Create grids
  if (square) {
    # Create squares
    grids <- lapply(1:nrow(x), function(i) {
      coords <- sf::st_coordinates(x$geometry[i])
      x_coord <- coords[1]
      y_coord <- coords[2]
      sf::st_polygon(list(matrix(c(
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

  # Build result
  if (keep_attributes) {
    # Extract attribute data (drop geometry)
    attributes_df <- sf::st_drop_geometry(x)

    # Return sf object with original attributes + new grid geometry
    result <- sf::st_sf(
      attributes_df,
      geometry = sf::st_sfc(grids, crs = sf::st_crs(x))
    )
  } else {
    # Return only grid geometry
    result <- sf::st_sf(
      geometry = sf::st_sfc(grids, crs = sf::st_crs(x))
    )
  }

  return(result)
}

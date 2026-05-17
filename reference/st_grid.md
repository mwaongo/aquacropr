# Create Grid from Points

Creates a regular grid (square or hexagonal) centered on point
geometries.

## Usage

``` r
st_grid(
  x,
  cellsize = 1,
  square = TRUE,
  coords = c("x", "y"),
  crs = NULL,
  keep_attributes = TRUE
)
```

## Arguments

- x:

  An sf object with POINT or MULTIPOINT geometries, or a data frame with
  coordinate columns.

- cellsize:

  Numeric. Size of grid cells (in units of the CRS).

- square:

  Logical. If TRUE, creates square grids. If FALSE, creates hexagonal
  grids (not yet implemented).

- coords:

  Character vector of length 2. Names of coordinate columns if x is a
  data frame. Default is c("x", "y").

- crs:

  Coordinate reference system. Used when coercing data frame to sf.

- keep_attributes:

  Logical. If TRUE (default), preserves all attribute columns from the
  input. If FALSE, returns only the grid geometry.

## Value

An sf object with POLYGON geometries representing grid cells. If
keep_attributes = TRUE, includes all original columns from x.

## Details

If x is a data frame, it will be converted to an sf object using the
specified coordinate columns and CRS. The function creates square grid
cells centered on each point geometry.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# From sf object
points <- st_as_sf(data.frame(x = 1:5, y = 1:5), coords = c("x", "y"))
grid <- st_grid(points, cellsize = 0.5)

# From data frame
df <- data.frame(lon = c(-5, 0, 5), lat = c(10, 15, 20), name = c("A", "B", "C"))
grid <- st_grid(df, cellsize = 1, coords = c("lon", "lat"), crs = 4326)

# Without attributes
grid_only <- st_grid(df,
  cellsize = 1, coords = c("lon", "lat"),
  crs = 4326, keep_attributes = FALSE
)
} # }
```

# Write Fixed-Width Format File

Write data to a fixed-width format (FWF) file where each column has a
specified character width. This function creates properly aligned text
files commonly used for data exchange with legacy systems and specific
file format requirements like AquaCrop climate files.

## Usage

``` r
write_fwf(
  x,
  file,
  width,
  justify = "l",
  replace_na = "NA",
  eol = NULL,
  append = TRUE
)
```

## Arguments

- x:

  Data frame to write to file. All columns will be converted to
  character format. Factor columns are automatically converted to
  character before writing.

- file:

  Character string specifying the output file path. Can be a relative or
  absolute path.

- width:

  Numeric value or vector specifying the character width for each
  column:

  - Single value: All columns use the same width (e.g., `width = 10`)

  - Vector: Each column uses its corresponding width (e.g.,
    `width = c(10, 15, 8)`)

  If a single value is provided for a multi-column data frame, it is
  recycled for all columns.

- justify:

  Character string specifying text alignment within each column width:

  - `"l"`: Left-align all columns

  - `"r"`: Right-align all columns

  - Multi-character string: Align each column individually (e.g.,
    `"lrl"` for left, right, left alignment of three columns)

  Default = `"l"`. If a single character is provided for a multi-column
  data frame, it is recycled for all columns.

- replace_na:

  Character string to use in place of NA values. Default = `"NA"`

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected.

  - `"windows"`: Windows-style line endings (\r\n)

  - `"unix"`, `"linux"`, `"macOS"`: Unix-style line endings (\n)

  Default = `"windows"`

- append:

  Logical. If `TRUE`, append to existing file. If `FALSE`, overwrite
  existing file. Default = `TRUE`

## Value

Invisibly returns `NULL`. The function is called for its side effect of
writing data to a file.

## Details

This function provides a lightweight solution for writing fixed-width
format files without additional dependencies beyond base R and readr. It
is particularly useful for:

- Creating AquaCrop climate data files (.PLU, .ETo, .Tnx)

- Generating data files for legacy systems

- Producing human-readable aligned text output

### Column Alignment:

Text is aligned within the specified width using `sprintf` formatting:

- Left-aligned (`"l"`): Text starts at the left edge, padded on the
  right

- Right-aligned (`"r"`): Text ends at the right edge, padded on the left

### Data Conversion:

- Factor columns are automatically converted to character

- NA values are replaced with the string specified in `replace_na`

- All data is formatted as character strings with specified widths

### File Writing:

- By default, data is appended to existing files (`append = TRUE`)

- Use `append = FALSE` to overwrite existing files

- Column names are not included in the output

## See also

[`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
for functions that use `write_fwf` to create AquaCrop climate files

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
df <- data.frame(
  year = c(2020, 2021, 2022),
  rainfall = c(850.5, 920.3, 780.1),
  temp = c(25.2, 26.1, 24.8)
)

# Write with uniform width, left-aligned
write_fwf(
  x = df,
  file = "output.txt",
  width = 10,
  justify = "l",
  append = FALSE
)

# Write with different widths per column, right-aligned
write_fwf(
  x = df,
  file = "output.txt",
  width = c(6, 10, 10),
  justify = "r",
  append = FALSE
)

# Write with mixed alignment (left, right, right)
write_fwf(
  x = df,
  file = "output.txt",
  width = c(6, 10, 10),
  justify = "lrr",
  append = FALSE
)

# Append to existing file
write_fwf(
  x = df[1:2, ],
  file = "output.txt",
  width = 10,
  justify = "r",
  append = TRUE
)

# Handle NA values with custom replacement
df_na <- data.frame(
  year = c(2020, 2021, NA),
  value = c(100, NA, 150)
)

write_fwf(
  x = df_na,
  file = "output.txt",
  width = 10,
  justify = "r",
  replace_na = "-9999",
  append = FALSE
)

# Unix-style line endings
write_fwf(
  x = df,
  file = "output.txt",
  width = 10,
  justify = "r",
  eol = "unix",
  append = FALSE
)
} # }
```

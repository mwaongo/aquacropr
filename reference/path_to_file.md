# Get access to CO2 Files Bundled in aquacropr

Access CO2 concentration data files bundled with aquacropr. The package
includes historical CO2 data and climate scenario projections in the
`inst/extdata` directory, formatted for AquaCrop v7.0 (August 2022).
These files are automatically used by
[`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
when generating climate files.

## Usage

``` r
path_to_file(path = NULL)
```

## Arguments

- path:

  Character string specifying the name of the CO2 file (with .CO2
  extension) in the `inst/extdata` directory. If `NULL` (default),
  returns a list of all available CO2 files.

## Value

- If `path = NULL`: A character vector listing all available CO2 files

- If `path` is specified: The full file system path to the requested CO2
  file

## Details

### CO2 Data Files Included:

**Historical Observations:**

- **MaunaLoa.CO2**: Atmospheric CO2 from Mauna Loa Observatory (observed
  data)

- **GlobalAverage.CO2**: Global average atmospheric CO2 concentrations

**IPCC Scenarios:**

- **IPCC-BERN_A1B.CO2**: IPCC BERN model A1B scenario

**RCP Scenarios** (Representative Concentration Pathways - used in IPCC
AR5):

- **RCP2-6.CO2**: RCP 2.6 - Strong mitigation (radiative forcing peaks
  at ~3 W/m² then declines)

- **RCP4-5.CO2**: RCP 4.5 - Moderate mitigation (stabilizes at ~4.5
  W/m²)

- **RCP6-0.CO2**: RCP 6.0 - Intermediate pathway (stabilizes at ~6 W/m²)

- **RCP8-5.CO2**: RCP 8.5 - High emissions (rises to ~8.5 W/m² by 2100)

**SSP Scenarios** (Shared Socioeconomic Pathways - used in IPCC AR6):

- **SSP1_1.9.CO2**: SSP1-1.9 - Very low emissions (~1.9 W/m²)

- **SSP1_2.6.CO2**: SSP1-2.6 - Low emissions (~2.6 W/m²)

- **SSP2_4.5.CO2**: SSP2-4.5 - Moderate emissions (~4.5 W/m²)

- **SSP3_7.0.CO2**: SSP3-7.0 - High emissions (~7.0 W/m²)

- **SSP5_8.5.CO2**: SSP5-8.5 - Very high emissions (~8.5 W/m²)

### Usage Notes:

You typically don't need to call this function directly. The
[`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
function automatically selects and copies the appropriate CO2 file based
on the `scenario` parameter. However, this function is useful for:

- Listing available CO2 scenarios

- Reading CO2 data for analysis

- Manually copying CO2 files to custom locations

The function uses
[`system.file()`](https://rdrr.io/r/base/system.file.html) to locate
files within the installed package, ensuring paths work across different
operating systems and installation locations.

## See also

Other utility functions:
[`calculate_plant_density()`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md),
[`is_leap_year()`](https://mwaongo.github.io/aquacropr/reference/is_leap_year.md),
[`round_to()`](https://mwaongo.github.io/aquacropr/reference/round_to.md),
[`salinity_to_ece()`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md),
[`to_aquacrop_day()`](https://mwaongo.github.io/aquacropr/reference/to_aquacrop_day.md)

## Examples

``` r
# List all available CO2 files
path_to_file()
#>  [1] "AUTHORS.md"                    "AggregationResults.SIM"       
#>  [3] "DailyResultsFullList.SIM"      "GlobalAverage.CO2"            
#>  [5] "IPCC-BERN_A1B.CO2"             "LICENSE"                      
#>  [7] "MaunaLoa.CO2"                  "ParticularResultsFullList.SIM"
#>  [9] "RCP2-6.CO2"                    "RCP4-5.CO2"                   
#> [11] "RCP6-0.CO2"                    "RCP8-5.CO2"                   
#> [13] "SSP1_1.9.CO2"                  "SSP1_2.6.CO2"                 
#> [15] "SSP2_4.5.CO2"                  "SSP3_7.0.CO2"                 
#> [17] "SSP5_8.5.CO2"                 

# Get path to historical Mauna Loa data (used by default in write_cli)
co2_hist <- path_to_file("MaunaLoa.CO2")

# Get paths to climate scenario files
co2_rcp45 <- path_to_file("RCP4-5.CO2")
co2_ssp126 <- path_to_file("SSP1_2.6.CO2")

# Read CO2 data for analysis
if (FALSE) { # \dontrun{
# Read historical CO2 concentrations
hist_data <- readLines(path_to_file("MaunaLoa.CO2"))

# Compare different scenarios
rcp26_data <- readLines(path_to_file("RCP2-6.CO2"))
rcp85_data <- readLines(path_to_file("RCP8-5.CO2"))

# Note: Typically you use write_cli() instead, which handles this automatically:
write_cli(stn = "Wakanda", scenario = "rcp45") # Automatically uses RCP4-5.CO2
write_cli(stn = "Wakanda", scenario = "hist") # Automatically uses MaunaLoa.CO2
} # }
```

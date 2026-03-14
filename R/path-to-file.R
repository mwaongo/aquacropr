#' Get access to CO2 Files Bundled in aquacropr
#'
#' @description
#' Access CO2 concentration data files bundled with aquacropr. The package includes
#' historical CO2 data and climate scenario projections in the `inst/extdata` directory,
#' formatted for AquaCrop v7.0 (August 2022). These files are automatically used by
#' \code{\link{write_cli}} when generating climate files.
#'
#' @param path Character string specifying the name of the CO2 file (with .CO2 extension)
#'   in the `inst/extdata` directory. If `NULL` (default), returns a list of all
#'   available CO2 files.
#'
#' @return
#' \itemize{
#'   \item If `path = NULL`: A character vector listing all available CO2 files
#'   \item If `path` is specified: The full file system path to the requested CO2 file
#' }
#'
#' @details
#' ## CO2 Data Files Included:
#'
#' **Historical Observations:**
#' \itemize{
#'   \item \strong{MaunaLoa.CO2}: Atmospheric CO2 from Mauna Loa Observatory (observed data)
#'   \item \strong{GlobalAverage.CO2}: Global average atmospheric CO2 concentrations
#' }
#'
#' **IPCC Scenarios:**
#' \itemize{
#'   \item \strong{IPCC-BERN_A1B.CO2}: IPCC BERN model A1B scenario
#' }
#'
#' **RCP Scenarios** (Representative Concentration Pathways - used in IPCC AR5):
#' \itemize{
#'   \item \strong{RCP2-6.CO2}: RCP 2.6 - Strong mitigation (radiative forcing peaks at ~3 W/m² then declines)
#'   \item \strong{RCP4-5.CO2}: RCP 4.5 - Moderate mitigation (stabilizes at ~4.5 W/m²)
#'   \item \strong{RCP6-0.CO2}: RCP 6.0 - Intermediate pathway (stabilizes at ~6 W/m²)
#'   \item \strong{RCP8-5.CO2}: RCP 8.5 - High emissions (rises to ~8.5 W/m² by 2100)
#' }
#'
#' **SSP Scenarios** (Shared Socioeconomic Pathways - used in IPCC AR6):
#' \itemize{
#'   \item \strong{SSP1_1.9.CO2}: SSP1-1.9 - Very low emissions (~1.9 W/m²)
#'   \item \strong{SSP1_2.6.CO2}: SSP1-2.6 - Low emissions (~2.6 W/m²)
#'   \item \strong{SSP2_4.5.CO2}: SSP2-4.5 - Moderate emissions (~4.5 W/m²)
#'   \item \strong{SSP3_7.0.CO2}: SSP3-7.0 - High emissions (~7.0 W/m²)
#'   \item \strong{SSP5_8.5.CO2}: SSP5-8.5 - Very high emissions (~8.5 W/m²)
#' }
#'
#' ## Usage Notes:
#'
#' You typically don't need to call this function directly. The \code{\link{write_cli}}
#' function automatically selects and copies the appropriate CO2 file based on the
#' \code{scenario} parameter. However, this function is useful for:
#' \itemize{
#'   \item Listing available CO2 scenarios
#'   \item Reading CO2 data for analysis
#'   \item Manually copying CO2 files to custom locations
#' }
#'
#' The function uses \code{system.file()} to locate files within the installed package,
#' ensuring paths work across different operating systems and installation locations.
#'
#' @examples
#' # List all available CO2 files
#' path_to_file()
#'
#' # Get path to historical Mauna Loa data (used by default in write_cli)
#' co2_hist <- path_to_file("MaunaLoa.CO2")
#'
#' # Get paths to climate scenario files
#' co2_rcp45 <- path_to_file("RCP4-5.CO2")
#' co2_ssp126 <- path_to_file("SSP1_2.6.CO2")
#'
#' # Read CO2 data for analysis
#' \dontrun{
#' # Read historical CO2 concentrations
#' hist_data <- readLines(path_to_file("MaunaLoa.CO2"))
#'
#' # Compare different scenarios
#' rcp26_data <- readLines(path_to_file("RCP2-6.CO2"))
#' rcp85_data <- readLines(path_to_file("RCP8-5.CO2"))
#'
#' # Note: Typically you use write_cli() instead, which handles this automatically:
#' write_cli(stn = "Wakanda", scenario = "rcp45") # Automatically uses RCP4-5.CO2
#' write_cli(stn = "Wakanda", scenario = "hist") # Automatically uses MaunaLoa.CO2
#' }
#' @family utility functions
#' @export

path_to_file <- function(path = NULL) {
  if (is.null(path)) {
    # List all files in extdata directory
    dir(system.file("extdata", package = "aquacropr"))
  } else {
    # Validate input
    if (!is.character(path) || length(path) != 1) {
      stop(
        "path must be a single character string.",
        "\nReceived: ", class(path)[1], " of length ", length(path)
      )
    }

    # Get full path to the specified file
    file_path <- system.file(
      "extdata",
      path,
      package = "aquacropr",
      mustWork = TRUE
    )

    return(file_path)
  }
}

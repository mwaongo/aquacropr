#' Write AquaCrop Management File
#' @description
#' Write an AquaCrop v7.0 (August 2022) management (.MAN) file containing field and crop management parameters.
#' This file specifies management practices such as mulch cover, soil fertility, weeds, and harvesting practices
#' that will be applied during the AquaCrop simulation.
#'
#' @param path Directory path where the output .MAN file will be written. Default = "man/"
#' @param management_name Name identifier for the management scenario (used in output filename). Default = "generic-management"
#' @param eol End-of-line character style for the output file. Options: "windows", "linux", "unix", or "macOS". Default = "windows"
#' @param params Named list of management parameter values to override defaults from ManData.
#'   Parameter names should be var_02 through var_21 (e.g., `list(var_02 = 7.0, var_04 = 50)`).
#'   Unspecified parameters use default values from ManData. See details for complete parameter list and valid ranges.
#'
#' @details
#' ManData contains 20 management parameters (var_02 to var_21) with the following valid ranges:
#' \itemize{
#'   \item var_02: AquaCrop version (typically 7.0)
#'   \item var_03: Ground surface covered by mulches (0-100 %)
#'   \item var_04: Mulch effect on soil evaporation reduction (0-100 %)
#'   \item var_05: Soil fertility stress degree (0-100 %)
#'   \item var_06: Height of soil bunds (0+ m)
#'   \item var_07: Surface runoff not affected by practices (0-100 %)
#'   \item var_08: Surface runoff completely prevented (0-100 %)
#'   \item var_09: Relative weed cover at canopy closure (0-100 %)
#'   \item var_10: Increase of relative weed cover in mid-season (0+ %)
#'   \item var_11: Shape factor of canopy cover expansion in weed field
#'   \item var_12: Replacement by weeds of self-thinned cover (0-100 %)
#'   \item var_13: Multiple cuttings flag (0 = not considered, 1 = considered)
#'   \item var_14: Canopy cover after cutting (0-100 %)
#'   \item var_15: Increase of Canopy Growth Coefficient after cutting (0+ %)
#'   \item var_16: First day of cutting window (1+ days)
#'   \item var_17: Number of days in cutting window (-9 = total cycle)
#'   \item var_18: Timing of multiple cuttings
#'   \item var_19: Time criterion for cuttings
#'   \item var_20: Final harvest at maturity flag (0 = not considered, 1 = considered)
#'   \item var_21: Start of growing cycle in cuttings list (1+ or -9)
#' }
#'
#' @return
#' Invisibly returns the full path to the created .MAN file. Creates a colon-delimited .MAN file
#' in the specified directory with the format: <<management_name>>.MAN containing management
#' parameters and descriptions as required by AquaCrop v7.0.
#'
#' @examples
#' \dontrun{
#' # Write default management file
#' write_man(path = "management/", management_name = "default-management")
#'
#' # Write custom management with modified parameters
#' write_man(
#'   path = "management/",
#'   management_name = "irrigated-maize",
#'   params = list(var_03 = 20, var_04 = 60, var_05 = 30)
#' )
#' }
#'
#' @export

write_man <- function(
    path = "man/",
    management_name = "generic-management",
    eol = "windows",
    params = list()) {
  # Handle NULL params
  if (is.null(params)) {
    params <- list()
  }

  # Add only the missing critical parameters, just to construct header
  if (!"var_03" %in% names(params)) {
    params$var_03 <- 0
  }

  if (!"var_05" %in% names(params)) {
    params$var_05 <- 42
  }

  # Extract values for header (always available now)
  fertilization_rate <- params$var_05
  mulching_rate <- params$var_03

  # Validate fertilization_rate and mulching_rate
  if (
    !is.numeric(fertilization_rate) ||
      fertilization_rate < 0 ||
      fertilization_rate > 100
  ) {
    stop(
      "fertilization_rate must be numeric between 0 and 100. Received: ",
      fertilization_rate,
      "\nNote: Represents soil fertility stress (%) - var_05"
    )
  }

  # Ensure trailing slash on path

  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  fs::dir_create(path, recurse = TRUE)

  # Load ManData
  utils::data("ManData", envir = environment())
  valid_names <- ManData$name

  # Validate parameter names
  invalid_names <- setdiff(names(params), valid_names)
  if (length(invalid_names) > 0) {
    stop(
      "Invalid parameter names: ",
      paste(invalid_names, collapse = ", "),
      "\nValid names are: ",
      paste(valid_names, collapse = ", ")
    )
  }

  # Parameter-specific validation ranges
  param_ranges <- list(
    var_03 = c(0, 100), # mulch cover %
    var_04 = c(0, 100), # mulch effect %
    var_05 = c(0, 100), # soil fertility %
    var_06 = c(0, Inf), # bund height m
    var_07 = c(0, 100), # runoff not affected %
    var_08 = c(0, 100), # runoff prevented %
    var_09 = c(0, 100), # weed cover %
    var_10 = c(0, Inf), # weed increase %
    var_12 = c(0, 100), # weed replacement %
    var_13 = c(0, 1), # multiple cuttings flag
    var_14 = c(0, 100), # canopy cover after cutting %
    var_15 = c(0, Inf), # CGC increase %
    var_16 = c(1, Inf), # first day of window
    var_20 = c(0, 1) # final harvest flag
  )

  # Validate parameter ranges
  for (param_name in names(params)) {
    if (param_name %in% names(param_ranges)) {
      range_limits <- param_ranges[[param_name]]
      value <- params[[param_name]]

      if (value < range_limits[1] || value > range_limits[2]) {
        stop(
          "Parameter '",
          param_name,
          "' value (",
          value,
          ") is outside valid range [",
          range_limits[1],
          ", ",
          range_limits[2],
          "]"
        )
      }
    }
  }

  # Process parameters
  value <- params %>%
    unlist() %>%
    as.vector() # %>%
  # round_to(to = 5 ) that's good but not general, think later how to take user need

  description <- ManData %>%
    dplyr::filter((name %in% names(params))) %>%
    dplyr::pull(description)

  fmt <- ManData %>%
    dplyr::filter((name %in% names(params))) %>%
    dplyr::pull(fmt)

  width <- ManData %>%
    dplyr::filter((name %in% names(params))) %>%
    dplyr::pull(width)

  new_vars <- tibble::tibble(
    name = names(params),
    value = value,
    description = description,
    fmt = fmt,
    width = width
  )

  # Combine with defaults and prepare output
  man_info <- ManData %>%
    dplyr::filter(!(name %in% names(params))) %>%
    dplyr::bind_rows(new_vars) %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(
      value = .format_string2(
        string = value,
        fmt = fmt,
        width = width
      )
    ) %>%
    dplyr::select(value, description)

  # Get line ending and header
  sep <- .get_eol(eol = eol)

  header <- .get_man_header(
    man = management_name,
    fertilizer = fertilization_rate,
    mulch = mulching_rate,
    eol = eol
  ) %>%
    glue::glue("\n", .sep = sep)

  # Write file (FIX: was .CRO, should be .MAN)
  output_file <- paste0(path, management_name, ".MAN")

  readr::write_file(
    x = header,
    file = output_file
  )

  readr::write_delim(
    x = man_info,
    file = output_file,
    col_names = FALSE,
    delim = ":",
    eol = sep,
    append = TRUE,
    quote = "none"
  )

  # Return invisibly with file path
  invisible(output_file)
}

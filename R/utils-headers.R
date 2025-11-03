
#' Get Climate File Header
#'
#' @description
#' Internal function to generate headers for AquaCrop climate files (.PLU, .ETo, .Tnx).
#'
#' @param var_name Character string specifying variable type: "rain", "et0", or "temperature". Default = NULL
#' @param stn Station name or identifier. Default = "station"
#' @param syear Start year of the data period. Default = 1991
#' @param eyear End year of the data period. Default = 2020
#' @param eol End-of-line character style. Options: "windows", "unix", "linux", or "macOS". Default = "windows"
#' @param record_type Type of temporal aggregation. Options: 1 = daily, 2 = 10-daily, 3 = monthly. Default = 1
#' @param first_day First day of record. For 10-daily: 1, 11, or 21; For monthly: 1. Default = 1
#' @param first_month First month of record (1-12). Default = 1
#'
#' @return Character string containing the formatted header for AquaCrop climate files
#'
#' @keywords internal
#' @noRd
.get_header <- function(
    var_name = NULL,
    stn = NULL,
    syear = NULL,
    eyear = NULL,
    eol = "windows",
    record_type = 1,
    first_day = 1,
    first_month = 1
) {

  # Apply safe defaults
  if (is.null(stn)) stn <- "station"
  if (is.null(syear)) syear <- 1991
  if (is.null(eyear)) eyear <- 2020
  if (is.null(var_name)) var_name <- "temperature"
  # Validate var_name
  valid_vars <- c("rain", "et0", "temperature")
  if (!is.null(var_name) && !var_name %in% valid_vars) {
    stop(
      "Invalid var_name: '", var_name, "'\n",
      "Valid options are: ", paste(valid_vars, collapse = ", ")
    )
  }

  # Validate record_type
  if (!record_type %in% c(1, 2, 3)) {
    stop(
      "Invalid record_type: ", record_type, "\n",
      "Valid options are: 1 (daily), 2 (10-daily), 3 (monthly)"
    )
  }

  # Validate first_day based on record_type
  if (record_type == 2 && !first_day %in% c(1, 11, 21)) {
    stop(
      "For 10-daily records (record_type = 2), first_day must be 1, 11, or 21\n",
      "Received: ", first_day
    )
  }

  if (record_type == 3 && first_day != 1) {
    warning("For monthly records (record_type = 3), first_day is typically 1. Using: ", first_day)
  }

  # Validate first_month
  if (!first_month %in% 1:12) {
    stop(
      "Invalid first_month: ", first_month, "\n",
      "Must be between 1 and 12"
    )
  }

  # Format station name
  stn_formatted <- snakecase::to_any_case(
    stn,
    case = "snake",
    sep_out = "_"
  )

  # Build headline 1 based on variable type
  data_type <- dplyr::case_when(
    var_name == "rain" ~ "rainfall",
    var_name == "et0" ~ "ETo",
    TRUE ~ "temperature"
  )

  headline_1 <- paste0(
    stn_formatted,
    " : Daily ", data_type, " data (1 Jan ", syear,
    " - 31 December ", eyear, ")"
  )

  # Build customizable headlines
  headline_2 <- sprintf("%6d  : Daily records (1=daily, 2=10-daily and 3=monthly data)", record_type)
  headline_3 <- sprintf("%6d  : First day of record (1, 11 or 21 for 10-day or 1 for months)", first_day)
  headline_4 <- sprintf("%6d  : First month of record", first_month)
  headline_5 <- paste0("  ", syear, "  : First year of record (1901 if not linked to a specific year)")
  headline_6 <- " "

  # Build headline 7 based on variable type
  headline_7 <- dplyr::case_when(
    var_name == "rain" ~ "  Total rain (mm)",
    var_name == "et0" ~ "  Average ETo (mm/day)",
    TRUE ~ "  Tmin (C)   Tmax (C)"
  )

  headline_8 <- "======================="

  # Get end-of-line separator
  sep <- .get_eol(eol = eol)

  # Combine all headlines
  header <- glue::glue(
    headline_1,
    headline_2,
    headline_3,
    headline_4,
    headline_5,
    headline_6,
    headline_7,
    headline_8,
    "\n",
    .sep = sep
  )

  return(header)
}


#' Get Soil File Header
#'
#' @description
#' Internal function to generate headers for AquaCrop soil files (.SOL).
#'
#' @param cn Numeric. Curve Number
#' @param rew Numeric. Readily evaporable water from top layer (mm)
#' @param nsoils Integer. Number of soil horizons
#' @param eol Character. End-of-line style. Default = "windows"
#'
#' @return Character string containing the formatted header for soil files
#' @keywords internal
#' @noRd
.get_soil_header <- function(cn = cn, rew = rew, nsoils = nsoils, eol = "windows") {
  headline_1 <- "default"
  headline_2 <- paste0(
    format("7.0", width = 28, justify = "centre"),
    ": AquaCrop Version (August 2022)"
  )
  headline_3 <- paste0(
    format(paste(cn), width = 28, justify = "centre"),
    ": CN (Curve Number)"
  )
  headline_4 <- paste0(
    format(paste(rew), width = 28, justify = "centre"),
    ": Readily evaporable water from top layer (mm)"
  )
  headline_5 <- paste0(
    format(paste(nsoils), width = 28, justify = "centre"),
    ": number of soil horizons"
  )
  headline_6 <- paste0(
    format("-9", width = 28, justify = "centre"),
    ": variable no longer applicable"
  )
  headline_7 <- paste0(
    "  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description"
  )
  headline_8 <- paste0(
    "  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------"
  )
  # end of line handling
  sep <- .get_eol(eol = eol)
  header <- glue::glue(
    headline_1,
    headline_2,
    headline_3,
    headline_4,
    headline_5,
    headline_6,
    headline_7,
    headline_8,
    "\n",
    .sep = sep
  )
  return(header)
}


#' Get Crop File Header
#'
#' @description
#' Internal helper function that generates the header line for AquaCrop crop (.CRO) files.
#'
#' @param crop Character string specifying the crop name or identifier. Default = "generic-crop-name"
#' @param owner Character string specifying the owner or source. Default = "Experimental"
#'
#' @return Character string containing the formatted header line
#' @keywords internal
#' @noRd
.get_crop_header <- function(crop = "generic-crop-name", owner = "Experimental") {
  line_1 <- glue::glue("Crop : ", crop, " , Owner: ", owner)
  return(line_1)
}


#' Generate Management File Header
#'
#' @description
#' Create a descriptive header for AquaCrop management (.MAN) files based on
#' fertility and mulching levels.
#'
#' @param man Character string naming the management scenario
#' @param fertilizer Numeric value (0-100) representing soil fertility level as percentage
#' @param mulch Numeric value (0-100) representing mulch coverage as percentage
#' @param eol Character string for end-of-line style. Default = "windows"
#'
#' @return Character string with formatted header
#' @keywords internal
#' @noRd
.get_man_header <- function(
    man = "generic_management",
    fertilizer,
    mulch,
    eol = "windows"
) {
  # Validate fertilizer input
  if (!is.numeric(fertilizer)) {
    stop(
      "fertilizer must be numeric. Received: ", class(fertilizer),
      "\nNote: var_05 represents soil fertility stress (%) - valid range 0-100"
    )
  }
  if (fertilizer < 0 || fertilizer > 100) {
    stop(
      "fertilizer value (", fertilizer, ") is outside valid range [0, 100]",
      "\nCheck var_05: Degree of soil fertility stress (%)"
    )
  }

  # Validate mulch input
  if (!is.numeric(mulch)) {
    stop(
      "mulch must be numeric. Received: ", class(mulch),
      "\nNote: var_03 represents ground surface covered by mulches (%) - valid range 0-100"
    )
  }
  if (mulch < 0 || mulch > 100) {
    stop(
      "mulch value (", mulch, ") is outside valid range [0, 100]",
      "\nCheck var_03: Percentage of ground surface covered by mulches (%)"
    )
  }

  # Classify fertility level
  fertility <- dplyr::case_when(
    fertilizer >= 20 & fertilizer <= 34 ~ "Very poor",
    fertilizer >= 35 & fertilizer <= 44 ~ "Poor",
    fertilizer >= 45 & fertilizer <= 55 ~ "About half",
    fertilizer >= 56 & fertilizer <= 75 ~ "Moderate",
    fertilizer >= 76 & fertilizer <= 99 ~ "Near optimal",
    fertilizer == 100 ~ "Non-limiting",
    .default = NA_character_
  )

  # Check for invalid fertility classification
  if (is.na(fertility)) {
    stop(
      "fertilizer value (", fertilizer, ") does not map to a valid fertility level",
      "\nValid ranges: 20-34 (Very poor), 35-44 (Poor), 45-55 (About half),",
      "\n56-75 (Moderate), 76-99 (Near optimal), 100 (Non-limiting)",
      "\nCheck var_05: Degree of soil fertility stress (%)"
    )
  }

  # Classify mulching level
  mulching <- dplyr::case_when(
    mulch == 0 ~ "None",
    mulch >= 1 & mulch <= 40 ~ "Sparse",
    mulch >= 41 & mulch <= 59 ~ "About half",
    mulch >= 60 & mulch <= 99 ~ "Significant",
    mulch == 100 ~ "Complete",
    .default = NA_character_
  )

  # Check for invalid mulching classification
  if (is.na(mulching)) {
    stop(
      "mulch value (", mulch, ") does not map to a valid mulching level",
      "\nValid ranges: 0 (None), 1-40 (Sparse), 41-59 (About half),",
      "\n60-99 (Significant), 100 (Complete)",
      "\nCheck var_03: Percentage of ground surface covered by mulches (%)"
    )
  }

  # Generate header line
  line_1 <- glue::glue(
    fertility, " fertility and ", mulching, " mulching coverage"
  )

  return(line_1)
}


#' Generate Soil Initial Conditions File Header
#'
#' @description
#' Internal helper function to create the header section of AquaCrop .SW0 files
#' (soil initial water content files).
#'
#' @param texture Character vector of soil texture names for each layer
#' @param nsoils Integer. Number of soil layers
#' @param initial_water Character or numeric. Initial water level
#' @param initial_cc Numeric. Initial canopy cover (%). Default = -9.00
#' @param initial_biomass Numeric. Initial biomass (ton/ha). Default = 0.000
#' @param initial_root_depth Numeric. Initial rooting depth (m). Default = -9.00
#' @param bund_water Numeric. Water stored between bunds (mm). Default = 0.0
#' @param bund_ec Numeric. EC of water between bunds (dS/m). Default = 0.00
#' @param eol Character. End-of-line style. Default = "windows"
#'
#' @return Character string containing the formatted header
#' @keywords internal
#' @noRd
.get_soil_init_header <- function(
    texture,
    nsoils,
    initial_water = "WP",
    initial_cc = -9.00,
    initial_biomass = 0.000,
    initial_root_depth = -9.00,
    bund_water = 0.0,
    bund_ec = 0.00,
    eol = "windows"
) {

  # Load SWOData for texture validation
  utils::data("SWOData", envir = environment())

  # Validate textures
  texture_norm <- tolower(trimws(texture))
  invalid_textures <- setdiff(texture_norm, tolower(SWOData$texture))

  if (length(invalid_textures) > 0) {
    stop(
      "Invalid texture(s): ", paste(unique(invalid_textures), collapse = ", "),
      "\nValid textures are:\n  ",
      paste(SWOData$texture, collapse = ", ")
    )
  }

  # Validate nsoils matches texture length
  if (length(texture) != nsoils) {
    warning(
      "Number of textures (", length(texture), ") does not match ",
      "number of soil layers (", nsoils, ").",
      "\nUsing nsoils = ", nsoils
    )
  }

  # Determine water content description
  if (is.character(initial_water)) {
    water_desc <- dplyr::case_when(
      tolower(initial_water) %in% c("wp", "wilting_point") ~ "at wilting point",
      tolower(initial_water) %in% c("fc", "field_capacity") ~ "at field capacity",
      tolower(initial_water) %in% c("sat", "saturation") ~ "at saturation",
      TRUE ~ "at specified water content"
    )
  } else if (is.numeric(initial_water)) {
    water_desc <- paste0("at ", sprintf("%.1f", initial_water), "% water content")
  } else {
    water_desc <- "with specified water content"
  }

  # Format headline 1 with texture and water content description
  if (length(texture) == 1) {
    headline_1 <- paste0(texture[1], " soil ", water_desc)
  } else {
    if (nsoils > 1) {
      if (is.character(initial_water)) {
        if (tolower(initial_water) %in% c("wp", "wilting_point")) {
          specific_desc <- "at respective wilting points"
        } else if (tolower(initial_water) %in% c("fc", "field_capacity")) {
          specific_desc <- "at respective field capacities"
        } else if (tolower(initial_water) %in% c("sat", "saturation")) {
          specific_desc <- "at respective saturation levels"
        } else {
          specific_desc <- "with layer-specific water content"
        }
      } else {
        specific_desc <- water_desc
      }

      headline_1 <- paste(
        paste(texture[1:(length(texture) - 1)], collapse = ", "),
        "and",
        texture[length(texture)],
        "soils",
        specific_desc
      )
    } else {
      headline_1 <- paste0(texture[1], " soil ", water_desc)
    }
  }

  # Line 2: AquaCrop version
  headline_2 <- paste0(
    format("7.0", width = 10, justify = "centre"),
    ": AquaCrop Version (August 2022)"
  )

  # Line 3: Initial canopy cover
  headline_3 <- paste0(
    format(sprintf("%.2f", initial_cc), width = 10, justify = "centre"),
    ": initial canopy cover that can be reached without water stress will be used as default"
  )

  # Line 4: Initial biomass
  headline_4 <- paste0(
    format(sprintf("%.3f", initial_biomass), width = 10, justify = "centre"),
    ": biomass (ton/ha) produced before the start of the simulation period"
  )

  # Line 5: Initial rooting depth
  headline_5 <- paste0(
    format(sprintf("%.2f", initial_root_depth), width = 10, justify = "centre"),
    ": initial effective rooting depth that can be reached without water stress will be used as default"
  )

  # Line 6: Water between bunds
  headline_6 <- paste0(
    format(sprintf("%.1f", bund_water), width = 10, justify = "centre"),
    ": water layer (mm) stored between soil bunds (if present)"
  )

  # Line 7: EC of water between bunds
  headline_7 <- paste0(
    format(sprintf("%.2f", bund_ec), width = 10, justify = "centre"),
    ": electrical conductivity (dS/m) of water layer stored between soil bunds (if present)"
  )

  # Line 8: Water content specification flag
  headline_8 <- paste0(
    format("0", width = 10, justify = "centre"),
    ": soil water content specified for specific layers"
  )

  # Line 9: Number of layers
  headline_9 <- paste0(
    format(as.character(nsoils), width = 10, justify = "centre"),
    ": number of layers considered"
  )

  # Line 10: Empty line
  headline_10 <- ""

  # Line 11: Column headers
  headline_11 <- "Thickness layer (m)     Water content (vol%)     ECe(dS/m)"

  # Line 12: Separator line
  headline_12 <- "=============================================================="

  # Get line ending
  sep <- .get_eol(eol = eol)

  # Build complete header
  header <- glue::glue(
    headline_1,
    headline_2,
    headline_3,
    headline_4,
    headline_5,
    headline_6,
    headline_7,
    headline_8,
    headline_9,
    headline_10,
    headline_11,
    headline_12,
    "\n",
    .sep = sep
  )

  return(header)
}


#' Get PRM Header Lines
#'
#' @description
#' Write the first two header lines to a .PRM file.
#'
#' @param path Character. Directory path
#' @param crop Character. Crop name
#' @param station_name Character. Station identifier
#' @param eol Character. End-of-line style. Default = "windows"
#'
#' @return NULL (writes to file)
#' @keywords internal
#' @noRd
.write_prm_header <- function(path = path, crop = crop, station_name, eol = "windows") {
  sep <- .get_eol(eol = eol)
  line1 <- paste0(.format_string2(crop, "%s", 16), "", sep)
  line2 <- paste0(.format_string2(7, "%.1f", 16), ":AquaCrop Version (August 2022)", sep)

  output_file <- fs::path(path, paste0(station_name, ".PRM"))
  readr::write_file(
    x = line1,
    file = output_file
  )
  readr::write_file(
    x = line2,
    file = output_file,
    append = TRUE
  )
}

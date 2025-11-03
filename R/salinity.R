#' Convert Salinity Level to ECe Value
#'
#' @description
#' Convert qualitative salinity descriptions to quantitative electrical conductivity
#' (ECe) values in dS/m. Uses standard salinity classification ranges.
#'
#' @param salinity Character string or numeric. Options:
#'   \itemize{
#'     \item \strong{"none"} or \strong{"non-saline"}: 0 dS/m
#'     \item \strong{"very slight"}: 1 dS/m
#'     \item \strong{"slight"} or \strong{"slightly saline"}: 3 dS/m
#'     \item \strong{"moderate"} or \strong{"moderately saline"}: 6 dS/m
#'     \item \strong{"strong"} or \strong{"strongly saline"}: 12 dS/m
#'     \item \strong{"very strong"} or \strong{"very strongly saline"}: 20 dS/m
#'     \item \strong{Numeric value}: Used directly (0-100 dS/m)
#'   }
#'
#' @return Numeric value representing electrical conductivity in dS/m
#'
#' @details
#' Standard salinity classification:
#' \itemize{
#'   \item 0-2 dS/m: Non-saline (most crops unaffected)
#'   \item 2-4 dS/m: Slightly saline (sensitive crops affected)
#'   \item 4-8 dS/m: Moderately saline (many crops affected)
#'   \item 8-16 dS/m: Strongly saline (only tolerant crops grow)
#'   \item >16 dS/m: Very strongly saline (few crops tolerate)
#' }
#'
#' @examples
#' salinity_to_ece("none")            # 0
#' salinity_to_ece("slightly saline") # 3
#' salinity_to_ece("moderate")        # 6
#' salinity_to_ece(5.5)               # 5.5
#'
#' @export
salinity_to_ece <- function(salinity) {
  if (is.numeric(salinity)) {
    if (salinity < 0 || salinity > 100) {
      stop(
        "ECe value must be between 0 and 100 dS/m. Received: ", salinity
      )
    }
    return(salinity)
  }

  if (!is.character(salinity)) {
    stop(
      "salinity must be character or numeric. Received: ", class(salinity)[1]
    )
  }

  # Normalize: lowercase and trim
  sal_norm <- tolower(trimws(salinity))

  # Map to ECe values (using midpoint of each range)
  ece <- dplyr::case_when(
    sal_norm %in% c("none", "non-saline", "non saline") ~ 0,
    sal_norm %in% c("very slight", "very slightly saline") ~ 1,
    sal_norm %in% c("slight", "slightly saline", "slightly") ~ 3,
    sal_norm %in% c("moderate", "moderately saline", "moderately") ~ 6,
    sal_norm %in% c("strong", "strongly saline", "strongly", "high") ~ 12,
    sal_norm %in% c("very strong", "very strongly saline", "very high") ~ 20,
    TRUE ~ NA_real_
  )

  if (is.na(ece)) {
    stop(
      "Invalid salinity level: '", salinity, "'",
      "\nValid options: 'none', 'very slight', 'slight', 'moderate', 'strong', 'very strong'",
      "\nOr provide numeric ECe value (0-100 dS/m)"
    )
  }

  return(ece)
}


#' Get Salinity Description from ECe Value
#'
#' @description
#' Convert electrical conductivity (ECe) values to qualitative salinity descriptions.
#'
#' @param ece Numeric. Electrical conductivity in dS/m
#'
#' @return Character string describing the salinity level
#'
#' @examples
#' ece_to_salinity(0)    # "Non-saline"
#' ece_to_salinity(3.5)  # "Slightly saline"
#' ece_to_salinity(10)   # "Strongly saline"
#'
#' @export
ece_to_salinity <- function(ece) {
  if (!is.numeric(ece)) {
    stop("ece must be numeric. Received: ", class(ece)[1])
  }

  if (ece < 0) {
    stop("ECe cannot be negative. Received: ", ece)
  }

  # Use if-else instead of case_when to avoid evaluation issues
  if (ece <= 2) {
    return("Non-saline")
  } else if (ece <= 4) {
    return("Slightly saline")
  } else if (ece <= 8) {
    return("Moderately saline")
  } else if (ece <= 16) {
    return("Strongly saline")
  } else {
    return("Very strongly saline")
  }
}

#' Write the AquaCrop Project List (ListProjects.txt)
#'
#' @description
#' Scans a project directory (`LIST/` by default) for `.PRM` files and writes
#' their names, one per line, to `ListProjects.txt` in the same directory.
#' AquaCrop standalone reads this file to know which projects to run.
#'
#' The file is rewritten from scratch on every call, so it always mirrors the
#' `.PRM` files currently present in `path`.
#'
#' @param path Directory holding the `.PRM` files. Default: `"LIST/"`
#' @param eol End-of-line character style. Options: "windows", "linux", or
#'   "macos". If `NULL` (default), eol is auto-detected.
#'
#' @return Invisibly returns the path to `ListProjects.txt`, or `NULL` when
#'   `path` does not exist.
#'
#' @keywords internal
#' @noRd
.write_list_projects <- function(path = "LIST/", eol = NULL) {
  stopifnot(is.character(path) && length(path) == 1)

  if (!dir.exists(path)) return(invisible(NULL))

  sep <- .get_eol(eol)

  projects <- basename(fs::dir_ls(path, glob = "*.PRM", type = "file"))

  output_file <- file.path(path, "ListProjects.txt")

  readr::write_file(
    x    = if (length(projects) == 0L) "" else paste0(paste(projects, collapse = sep), sep),
    file = output_file
  )

  invisible(output_file)
}

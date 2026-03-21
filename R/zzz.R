#' Package Attach Message
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  msg <- cli::format_inline(
    "{.pkg aquacropr} v{version}, R interface to FAO AquaCrop v7.x\n",
    "Run {.code init_aquacrop()} to initialize your project."
  )
  packageStartupMessage(msg)
}

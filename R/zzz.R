#' Package Attach Message
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  msg <- cli::format_inline(
    "{.pkg aquacropr} v{version}\n",
    "Wrapper and Tools to streamline the use of FAO Crop Water Model AquaCrop (v7.0+)\n",
    "\n",
    "First time setup: use {.code init_aquacrop()} to create aquacropr project structure."
  )

  packageStartupMessage(msg)
}

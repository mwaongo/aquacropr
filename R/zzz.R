#' Package Attach Message
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  msg <- cli::format_inline(
    "{.pkg aquacropr} v{version}\n",
    "R interface to AquaCrop v7 and higher, the FAO crop water productivity model.\n",
    "Streamlines reading, writing, and managing AquaCrop input/output files\n",
    "for batch simulations, climate scenario analysis, and crop yield assessment.\n",
    "\n",
    "First time setup: use {.code init_aquacrop()} to initialize your project."
  )
  packageStartupMessage(msg)
}

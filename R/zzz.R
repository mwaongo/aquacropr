#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # récupère le nom/vers. sans dépendre de .pkg_name() si absent
  pkg <- if (exists(".pkg_name", mode = "function")) .pkg_name() else pkgname
  ver <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "?.?")

  # mise en forme avec cli, puis émission via packageStartupMessage()
  msg <- cli::format_inline(
    "{.pkg {pkg}} {ver}\nUse {.code init_aquacrop()} to create a new project structure."
  )
  packageStartupMessage(msg)
}

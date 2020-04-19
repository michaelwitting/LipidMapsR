#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  base_url <- "https://www.lipidmaps.org/rest"
  assign("BASE_URL", base_url, envir = asNamespace(pkgname))
}

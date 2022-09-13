#' Start the occupation coding API.
#'
#' @param start Whether to immediately start the api. (Defaults to TRUE)
#' @param file Path to the `plumber.R` file describing the API.
#'   Defaults to `plumber/api/plumber.R` within the installed package.
#'   Refer to this file to understand how the API is implemented.
#'
#' @return A Plumber router
#' @export
#'
#' @examples
#' \dontrun{
#' api(start = TRUE)
#'
#' router <- api(start = FALSE)
#' plumber::pr_run(router)
#' }
api <- function(start = TRUE, file = system.file("plumber", "api", "plumber.R", package = "occupationMeasurement")) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Starting the API server requires the R package 'plumber', please install it via install.packages(\"plumber\").")
  }

  require_dependencies()

  router <- plumber::pr(file = file)

  if (start) {
    plumber::pr_run(router)
  }

  return(invisible(router))
}

#' Start the occupation coding API.
#'
#' @param start Whether to immediately start the api. (Defaults to TRUE)
#' @param file Path to the `plumber.R` file describing the API.
#'   Defaults to `plumber/api/plumber.R` within the installed package.
#'   Refer to this file to understand how the API is implemented.
#' @param allow_origin Domain from which to allow cross origin requests (CORS).
#'   If the API is running on a different domain / server than the application
#'   using it, the website's root has to be provided here e.g.
#'   "https://occupationMeasurement.github.io". For more information see the
#'   [plumber security page](https://www.rplumber.io/articles/security.html#cross-origin-resource-sharing-cors),
#'   and [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin).
#'   Defaults to NULL to not set any header at all.
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
api <- function(start = TRUE, file = system.file("plumber", "api", "plumber.R", package = "occupationMeasurement"), allow_origin = NULL) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Starting the API server requires the R package 'plumber', please install it via install.packages(\"plumber\").")
  }

  require_dependencies()

  router <- plumber::pr(file = file)

  # Set CORS headers by adding a filter
  # Comparing with NULL and "" here to ignore default function and ENV values
  if (!is.null(allow_origin) && allow_origin != "") {
    stopifnot(is.character(allow_origin))

    message("Allowing cross origin requests (CORS) from: ", allow_origin)

    router |>
      plumber::pr_filter("cors", function(res) {
        res$setHeader("Access-Control-Allow-Origin", allow_origin)
        plumber::forward()
      })
  }

  if (start) {
    plumber::pr_run(router)
  }

  return(invisible(router))
}

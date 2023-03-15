#' Start the occupation coding API.
#' @param start Whether to immediately start the api. (Defaults to TRUE)
#' @param file Path to the `plumber.R` file describing the API.
#'   Defaults to `plumber/api/plumber.R` within the installed package.
#'   Refer to this file to understand how the API is implemented.
#' @param log_to_console Whether to requests should be logged in the console.
#'   Defaults to TRUE.
#' @param log_to_file Whether to requests should be logged in a file.
#'   Defaults to TRUE.
#'   Note: The file format used here is a CSV file for easier analysis.
#' @param log_filepath The path to a CSV file in which to save the structured
#'   logs.
#' @param require_identifier Whether an identifier has to be added to api
#'   requests in order to match / identify requests afterwards.
#'   Defaults to FALSE.
#' @param allow_origin Domain from which to allow cross origin requests (CORS).
#'   If the API is running on a different domain / server than the application
#'   using it, the website's root has to be provided here e.g.
#'   "https://occupationMeasurement.github.io". For more information see the
#'   [plumber security page](https://www.rplumber.io/articles/security.html#cross-origin-resource-sharing-cors),
#'   and [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin).
#'   Defaults to NULL to not set any header at all.
#' @return A Plumber router
#' @export
#' @seealso `vignette("api")`
#' @examples
#' # Get the plumber router
#' router <- api(
#'   start = FALSE,
#'   # If this is TRUE, the log directory will immediately be created
#'   log_to_file = FALSE
#' )
#' if (interactive()){
#'   # Start the router
#'   plumber::pr_run(router)
#' }
#'
#' if (interactive()){
#'   # Immediately start the API
#'   api(start = TRUE)
#' }
api <- function(start = TRUE,
                file = system.file("plumber", "api", "plumber.R", package = "occupationMeasurement"),
                log_to_console = TRUE,
                log_to_file = TRUE,
                log_filepath = file.path("output", "log_api.csv"),
                require_identifier = FALSE,
                allow_origin = NULL) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Starting the API server requires the R package 'plumber', please install it via install.packages(\"plumber\").")
  }

  require_dependencies()

  router <- plumber::pr(file = file)

  # Use full filepath as plumber will change working directory
  if (log_to_file) {
    # Make sure log directory exists
    log_filepath |>
      dirname() |>
      dir.create(showWarnings = FALSE, recursive = TRUE)

    # Construct the absolute path, by splitting and then recombining
    # This is necessary as normalizePath only works with existing destinations
    log_filepath_full <- file.path(
      log_filepath |>
        dirname() |>
        normalizePath(),
      log_filepath |>
        basename()
    )

    cat("Logging to file:", log_filepath_full, "\n")
  }

  # Add request logging
  router |>
    # Implement logging via preroute / postroute to capture response timing
    # and possibly output
    plumber::pr_hooks(list(
      preroute = function(data) {
        data$timestamp_preroute <- Sys.time()
      },
      postserialize = function(data, req, res, value) {
        ts_postserialize <- Sys.time()
        ts_preroute <- data$timestamp_preroute
        execution_time <- difftime(ts_postserialize, ts_preroute)

        # Simple logging to the console
        if (log_to_console) {
          cat(
            as.character(ts_preroute), "|",
            req$REQUEST_METHOD, "|",
            req$PATH_INFO, "|",
            req$QUERY_STRING, "|",
            req$HTTP_USER_AGENT, "@",
            req$REMOTE_ADDR, "|",
            execution_time,
            "\n"
          )
        }

        # Structured (& extended) logging to a CSV file
        if (log_to_file) {
          # Structured logging to a CSV file
          data.table(
            ts_preroute = ts_preroute,
            ts_postserialize = ts_postserialize,
            execution_time = execution_time,
            req_request_method = req$REQUEST_METHOD,
            req_path_info = req$PATH_INFO,
            req_query_string = req$QUERY_STRING,
            req_http_user_agent = req$HTTP_USER_AGENT,
            req_remote_addr = req$REMOTE_ADDR,
            res_status = res$status
            # Note: The returned response could be tracked via res$body,
            # but this includes HTML / JS data as well so is quite messy
          ) |>
            fwrite(
              log_filepath_full,
              append = file.exists(log_filepath_full)
            )
        }

        # Passing value along
        return(value)
      }
    ))

  if (require_identifier) {
    matching_prefix <- "/v1/"

    router |>
      # Check for the presence of an "identifier" parameter
      plumber::pr_filter("require_identifier", function(req, res) {
        identifier <- req$argsQuery$identifier

        # Return an error if no identifier has been provided
        if (
          startsWith(req$PATH_INFO, matching_prefix) &&
          (is.null(identifier) || is.na(identifier) || identifier == "")
        ) {
          res$status <- 400 # Bad Request
          return(list(error = "Missing query argument: identifier"))
        } else {
          plumber::forward()
        }
      }) |>
      # Update Open API spec to include "identifier" as a parameter
      plumber::pr_set_api_spec(function(spec) {
        for (pathname in names(spec$paths)) {
          path <- spec$paths[[pathname]]

          if (startsWith(pathname, matching_prefix) && !is.null(path$get)) {
            # Add new parameter "identifier" to spec
            spec$paths[[pathname]]$get$parameters <- path$get$parameters |>
              # Double nested list to append a list itself and not just add
              # its entries
              append(list(list(
                name = "identifier",
                description = "An identifier, typically tied to a responend, to allow matching different API requests with each other.",
                `in` = "query",
                required = TRUE,
                schema = list(
                  type = "string",
                  format = NULL,
                  default = NA
                )
              )))
          }
        }

        return(spec)
      })
  }

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

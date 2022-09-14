#' Preprocess a string, removing special characters and handling abbreviations.
#'
#' Replace some common characters / character sequences
#' (e.g., Ä, Ü, "DIPL.-ING.") with their uppercase equivalents and removes
#' punctuation, empty spaces and the word "Diplom".
#'
#' \link{charToRaw} or \link{stringi::stri_escape_unicode} helps to find UTF-8 characters.
#'
#' @encoding UTF-8
#'
#' @param verbatim The character vector to process.
#' @param lang The language the text is in.
#'   Currently only German is supported.
#'   Defaults to "de" (German).
#'
#' @return The same character vector after processing
#' @export
#'
#' @examples
#' preprocess_string(c(
#'   "Verkauf von B\u00fcchern, Schreibwaren",
#'   "Fach\u00e4rztin f\u00fcr Kinder- und Jugendmedizin im \u00f6ffentlichen Gesundheitswesen",
#'   "Industriemechaniker",
#'   "Dipl.-Ing. - Agrarwirtschaft (Landwirtschaft)"
#' ))
preprocess_string <- function(verbatim, lang = "de") {
  if (!is.character(verbatim)) {
    stop("Character Input required for function stringPreprocessing")
  }
  if (lang != "de") {
    stop("This function was programmed for German characters and German occupational titles")
  }
  verbatim <- toupper(verbatim)

  verbatim <- gsub(intToUtf8(0xc4), "AE", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xd6), "OE", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xdc), "UE", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xdf), "SS", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xc2), "A", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xc9), "E", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xca), "E", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xcd), "I", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xce), "I", verbatim, fixed = TRUE)
  verbatim <- gsub(intToUtf8(0xd4), "O", verbatim, fixed = TRUE)

  verbatim <- gsub("/", " ", verbatim, fixed = TRUE)
  verbatim <- gsub("+", " ", verbatim, fixed = TRUE)
  verbatim <- gsub("-", " ", verbatim, fixed = TRUE)
  verbatim <- gsub(")", " ", verbatim, fixed = TRUE)
  verbatim <- gsub("(", " ", verbatim, fixed = TRUE)

  verbatim <- gsub("\u20ac", "EURO", verbatim, fixed = TRUE) # no solution with intToUtf8
  verbatim <- gsub("\u0080", "EURO", verbatim, fixed = TRUE) # there must be a solution with intToUtf8

  verbatim <- gsub("DIPL.-ING.", "DIPLOMINGENIEUR", verbatim, fixed = TRUE) # this abbreviation is often used in the coding index
  verbatim <- gsub("ING.", "INGENIEUR", verbatim, fixed = TRUE) # also sometimes used
  verbatim <- gsub("DIPL.", "DIPLOM", verbatim, fixed = TRUE) # this abbreviation is often used in the coding index
  verbatim <- gsub("DIPLOM", "DIPLOM ", verbatim, fixed = TRUE) # better to think of DIPLOM as a separate word

  verbatim <- tm::removePunctuation(verbatim)
  verbatim <- stringr::str_trim(verbatim)

  verbatim <- gsub(" {2,}", " ", verbatim) # in case we have at least two double spaces, replace it with a single one

  verbatim <- gsub("DIPLOM ", "", verbatim)
  verbatim <- gsub("DIPL ", "", verbatim)

  return(verbatim)
}

# Check for and load all required packages
# Calling this upon initializing the package will lead to a slightly longer
# first load, but will lead to faster generation of suggestions
require_dependencies <- function(dependencies = c(
                                   "data.table",
                                   "shiny",
                                   "stringdist",
                                   "stringr",
                                   "text2vec",
                                   "tm"
                                 )) {
  # Call requireNamespace for each package / dependency
  lapply(dependencies, requireNamespace)
}

# Generate app_settings based on environment variables
create_app_settings_from_env <- function(verbose = FALSE) {
  if (verbose) message("Loading settings for occupationMeasurement app")

  # Get a list of possible parameters
  # (They should all be in upper case)
  possible_params <- create_app_settings |>
    args() |>
    as.list() |>
    names() |>
    toupper()
  possible_params <- possible_params[
    !(
      # Remove parameters starting with "."
      startsWith(".", possible_params) |
        # Remove empty parameters
        possible_params == ""
    )
  ]

  # Iterate over all possible parameters and extract them if present
  app_settings_params_from_env <- list()
  for (param in possible_params) {
    value <- Sys.getenv(param)

    # Try and convert to a number
    value_num <- suppressWarnings(as.numeric(value))

    # Set value if it exists
    if (value != "") {
      # Do a crude conversion from strings to different types
      if (value == "TRUE") {
        value <- TRUE
      } else if (value == "FALSE") {
        value <- FALSE
      } else if (!is.na(value_num)) {
        value <- value_num
      }

      app_settings_params_from_env[[param]] <- value
    }
  }

  # Convert parameters back to lowercase:
  names(app_settings_params_from_env) <- names(app_settings_params_from_env) |>
    tolower()

  if (verbose) {
    paste(
      "Detected variables:",
      app_settings_params_from_env |> print() |> capture.output() |> paste(collapse = "\n"),
      "List of supported environment variables:",
      possible_params |> print() |> capture.output() |> paste(collapse = "\n"),
      "Check ?occupationMeasurement::create_app_settings for explanations.",
      sep = "\n"
    ) |>
      message()
  }

  # Actually generate the app_settings taking into account defaults etc.
  app_settings <- do.call(create_app_settings, app_settings_params_from_env)

  return(app_settings)
}

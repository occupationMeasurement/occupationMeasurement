# TODO: Document datasets

#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"isco_08_en"

#' Load AuxCO from a directory of CSV files
#'
#' This function loads the Auxiliary Classification of Occupations (AuxCO) by
#' reading CSVs from the specified directory, while loading e.g. ids in the
#' correct format. Data is loaded into a named list matching the format
#' expected by other functions in this package.
#'
#' This package also includes an already loaded version of the [auxco], which
#' can be used straight away *without* calling this function.
#'
#' @param dir The path to the directory which holds the CSVs.
#' @param add_explanations Whether explanations should be added to some of the
#'   harder to understand task descriptions. Defaults to TRUE.
#'
#' @return A list with multiple data.tables.
#' @export
#' @seealso auxco
load_auxco <- function(dir, add_explanations = TRUE) {
  # Small convenience function to not re-specify parameters
  read_auxco_csv <- function(filename, ...) {
    fread(
      file.path(dir, filename),
      keepLeadingZeros = TRUE,
      encoding = "UTF-8",
      ...
    )
  }

  # Generate the auxco object
  auxco <- list(
    categories = read_auxco_csv(
      "auxco_categories.csv",
      colClasses = c(
        "auxco_id" = "character",
        "default_kldb_id" = "character",
        "default_isco_id" = "character"
      )
    ),
    distinctions = read_auxco_csv("auxco_distinctions.csv",
      colClasses = c(
        "auxco_id" = "character",
        "similar_auxco_id" = "character",
        "default_kldb_id" = "character"
      )
    ),
    followup_questions = read_auxco_csv("auxco_followup_questions.csv",
      colClasses = c(
        "auxco_id" = "character",
        "answer_kldb_id" = "character",
        "answer_isco_id" = "character"
      )
    ),
    mapping_from_kldb = read_auxco_csv("auxco_mapping_from_kldb.csv",
      colClasses = c(
        "kldb_id" = "character",
        "auxco_id" = "character"
      )
    ),
    mapping_from_isco = read_auxco_csv("auxco_mapping_from_isco.csv",
      colClasses = c(
        "isco_id" = "character",
        "auxco_id" = "character"
      )
    )
  )

  if(add_explanations) {
    # Add additional explanations for extra_instructions interviews
    # Was ist Ausführung von Hilfsarbeiten?
    auxco$categories[
      grepl("Ausf\u00fchrung von Hilfsarbeiten", task),
      task_description := paste(
        "Hilfsarbeiten sind einfache Aufgaben,",
        "f\u00fcr die keine abgeschlossene Berufsausbildung erforderlich ist,",
        task_description
      )
    ]
    # Was ist Führungsaufgaben mit Personalverantwortung?
    auxco$categories[
      grepl("F\u00fchrungsaufgaben mit Personalverantwortung", task),
      task_description := paste(
        "F\u00fchrungsaufgaben mit Personalverantwortung bedeutet,",
        "dass untergeordnete Arbeitskr\u00e4fte beaufsichtigt und angeleitet werden.",
        "Weitere Aufgaben sind",
        task_description
      )
    ]
  }

  return(load_auxco)
}

load_kldb_raw <- function() {
  terms_of_use <- "
    (c) Statistik der Bundesagentur f\u00fcr Arbeit
    Sie k\u00f6nnen Informationen speichern, (auch auszugsweise) mit Quellenangabe
    weitergeben, vervielf\u00e4ltigen und verbreiten. Die Inhalte d\u00fcrfen nicht
    ver\u00e4ndert oder verf\u00e4lscht werden. Eigene Berechnungen sind erlaubt, jedoch
    als solche kenntlich zu machen. Im Falle einer Zug\u00e4nglichmachung im
    Internet soll dies in Form einer Verlinkung auf die Homepage der Statistik
    der Bundesagentur f\u00fcr Arbeit erfolgen. Die Nutzung der Inhalte f\u00fcr
    gewerbliche Zwecke, ausgenommen Presse, Rundfunk und Fernsehen und
    wissenschaftliche Publikationen, bedarf der Genehmigung durch die Statistik
    der Bundesagentur f\u00fcr Arbeit.
  "

  # Create cache dir if it doesn't exist yet
  cache_path <- file.path("cache")
  dir.create(cache_path, showWarnings = FALSE)

  kldb_archive_path <- file.path(cache_path, "kldb_2010_archive.zip")
  if (!file.exists(kldb_archive_path)) {
    print(paste(
      "Using a modified version of the KldB 2010.",
      "Please mind the terms of use of the original KldB dataset (German):",
      terms_of_use,
      sep = "\n"
    ))

    # Download the kldb file (which is a zip archive)
    url <- "https://www.klassifikationsserver.de/klassService/jsp/variant/downloadexport?type=EXPORT_CSV_VARIANT&variant=kldb2010&language=DE"
    utils::download.file(url, destfile = kldb_archive_path, mode = "wb")
  }

  # Get the CSV filename
  # (R cannot extract the file directly due to special characters in the name)
  filename_in_zip <- utils::unzip(zipfile = kldb_archive_path, list = TRUE)[1, "Name"]

  # Unzip the file in-place and read its' contents
  # (fread does not support reading from this kind of stream)
  kldb_df <- utils::read.csv2(
    unz(kldb_archive_path, filename_in_zip),
    skip = 8,
    sep = ";",
    encoding = "UTF-8",
    check.names = FALSE
  )

  return(as.data.table(kldb_df))
}

#' Clean & Load KldB 2010 dataset.
#'
#' Use load_kldb_raw() to load the whole dataset.
#'
#' @return A cleaned / slimmed version of the KldB 2010.
#' @export
load_kldb <- function() {
  # Column names used in data.table (for R CMD CHECK)
  level <- title <- label <- kldb_id <- NULL

  kldb_data <- load_kldb_raw()

  kldb_new_names <- c(
    # old name => new name
    "Schl\u00fcssel KldB 2010" = "kldb_id",
    "Ebene" = "level",
    "Titel" = "title",
    "Allgemeine Bemerkungen" = "description",
    "Ausschl\u00fcsse" = "excludes"
  )

  setnames(
    kldb_data,
    old = names(kldb_new_names),
    new = kldb_new_names
  )

  # Only keep the new kldb columns
  # If you want to look at the whole dataset, use load_kldb_raw()
  kldb_data <- kldb_data[, kldb_new_names, with = FALSE]

  # Generate Clean level-4 Job Titles
  kldb_data[
    level == 4 & grepl("Berufe", title),
    label := gsub(
      "Berufe in der |Berufe im Bereich |Berufe im |Berufe in |Berufe f\u00fcr ",
      "",
      title
    )
  ]
  kldb_data[
    level == 4 & grepl("^[[:lower:]]", label),
    label := gsub(
      "technischen Laboratorium", "technisches Laboratorium",
      label,
      perl = TRUE
    )
  ]
  kldb_data[
    level == 4 & grepl("^[[:lower:]]", label),
    label := gsub("^([[:lower:]-]{1,})(n )", "\\1 ", label, perl = TRUE)
  ]
  kldb_data[
    label == "technische Eisenbahnbetrieb",
    label := "technischer Eisenbahnbetrieb"
  ]
  kldb_data[
    label == "technische Luftverkehrsbetrieb",
    label := "technischer Luftverkehrsbetrieb"
  ]
  kldb_data[
    label == "technische Schiffsverkehrsbetrieb",
    label := "technischer Schiffsverkehrsbetrieb"
  ]
  kldb_data[
    label == "technische Betrieb des Eisenbahn-, Luft- und Schiffsverkehrs (sonstige spezifische T\u00e4tigkeitsangabe)",
    label := "technischer Betrieb des Eisenbahn-, Luft- und Schiffsverkehrs (sonstige spezifische T\u00e4tigkeitsangabe)"
  ]
  kldb_data[
    label == "visuelle Marketing",
    label := "visuelles Marketing"
  ]
  kldb_data[
    title == "Verwaltende Berufe im Sozial- und Gesundheitswesen",
    label := "Verwaltung im Sozial- und Gesundheitswesen"
  ]
  kldb_data[
    label == "kaufm\u00e4nnischen und technischen Betriebswirtschaft (ohne Spezialisierung)",
    label := "kaufm\u00e4nnische und technische Betriebswirtschaft (ohne Spezialisierung)"
  ]
  kldb_data[
    label == "\u00f6ffentlichen Verwaltung (ohne Spezialisierung)",
    label := "\u00d6ffentliche Verwaltung (ohne Spezialisierung)"
  ]
  kldb_data[
    label == "\u00f6ffentlichen Verwaltung (sonstige spezifische T\u00e4tigkeitsangabe)",
    label := "\u00d6ffentliche Verwaltung (sonstige spezifische T\u00e4tigkeitsangabe)"
  ]
  kldb_data[
    label == "operations-/medizintechnischen Assistenz",
    label := "operations-/medizintechnische Assistenz"
  ]
  kldb_data[
    label == "nicht klinischen Psychologie",
    label := "nicht klinische Psychologie"
  ]
  kldb_data[
    label == "nicht \u00e4rztlichen Psychotherapie",
    label := "nicht \u00e4rztliche Psychotherapie"
  ]
  kldb_data[
    label == "nicht \u00e4rztlichen Therapie und Heilkunde (sonstige spezifische T\u00e4tigkeitsangabe)",
    label := "nicht \u00e4rztliche Therapie und Heilkunde (sonstige spezifische T\u00e4tigkeitsangabe)"
  ]
  # Uppercase the first letter
  kldb_data[
    level == 4,
    label := gsub("^([[:lower:]])", "\\U\\1", label, perl = TRUE)
  ]
  kldb_data[
    level == 4 & is.na(label),
    label := title
  ]
  kldb_data[
    level == 4,
    label := gsub(" \\(sonstige spezifische T\u00e4tigkeitsangabe\\)", "", label)
  ]
  # Handle titles for Leitungsfunktion
  kldb_data[
    level == 4 & substr(kldb_id, 4, 4) == 9,
    label := paste(
      gsub(
        "Aufsichts- und F\u00fchrungskr\u00e4fte - |Aufsichtskr\u00e4fte - |F\u00fchrungskr\u00e4fte - ",
        "",
        label
      ),
      "(F\u00fchrungskraft)"
    )
  ]

  # Convert kldb_id to character for overall consistency, joins etc.
  kldb_data[, kldb_id := as.character(kldb_id)]

  # Only export the standard set of columns
  # Note: Column "excludes" is currently still used, but can hopefully be
  # dropped in the future or be handled in a more generic usecase
  # Note: Using two separate columns, label & title here.
  # We might want to only use one going forward,
  # but both are needed atm. to support previous code
  kldb_data <- kldb_data[
    ,
    c("kldb_id", "level", "label", "description", "excludes", "title")
  ]

  return(kldb_data)
}

kldb_10 <- load_kldb()
kldb_10_lvl_3 <- kldb_10[level == 3]

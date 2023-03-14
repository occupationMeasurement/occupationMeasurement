#' Categories of the The International Standard Classification of Occupations - ISCO-08
#'
#' Categories from the International Standard Classification of Occupations - ISCO-08. ISCO-08 is a hierarchical classification, consisting of 10 (1-digit) major groups, 43 (2-digit) sub-major groups, 130 (3-digit) minor groups, and 436 (4-digit) unit groups, all of them included in this data set.
#'
#' Source: https://esco.ec.europa.eu
#' This service uses the ESCO classification of the European Commission. The descriptions used here are taken from the ESCO classification (v1.1, Occupations pillar) of the European Commission, which is based on ISCO-08.
#'
#' More information on the ISCO-08: https://isco-ilo.netlify.app/en/isco-08/, https://www.ilo.org/public/english/bureau/stat/isco/isco08/
#'
#' @format A data frame with 619 rows and 3 variables:
#' \describe{
#'   \item{\code{code}}{character. Unique ISCO-08 identifier / code.}
#'   \item{\code{label}}{character. Short label / title for the category.}
#'   \item{\code{description}}{character. Detailed description of the category.}
#' }
"isco_08_en"

#' Pretrained ML models to be used with the package.
#'
#' @seealso [algo_similarity_based_reasoning()], [train_similarity_based_reasoning()], https://github.com/malsch/occupationCoding
#' @format A nested list with pretrained machine learning models:
#' \describe{
#'   \item{\code{similarity_based_reasoning}}{list. Contains pretrained models to be used with [algo_similarity_based_reasoning()].}
#'   \item{\code{similarity_based_reasoning$wordwise}}{list. Contains the pretrained model to be used for providing suggestions using full wordwise matching.}
#'   \item{\code{similarity_based_reasoning$substring}}{list. Contains the pretrained model to be used for providing suggestions using substring matching.}
#' }
#'
#' This training data always predicts a 5-digit code from the 2010 German Classification of Occupations, with some exceptions: -0004 stands for 'Not precise enough/uncodable', -0006 stands for 'Multiple Jobs', -0012 stands for 'Blue-collar workers', -0019 stands for 'Volunteer/Social Service', and -0030 stands for 'Student assistant'.
#'
#' @source
#' Data from the following surveys were pooled:
#'
#' Antoni, M., Drasch, K., Kleinert, C., Matthes, B., Ruland, M. and Trahms, A. (2010): Arbeiten und Lernen im Wandel * Teil 1: Überblick über die Studie, FDZ-Methodenreport 05/2010, Forschungsdatenzentrum der Bundesagentur für Arbeit im Institut für Arbeitsmarkt- und Berufsforschung, Nuremberg.
#'
#' Rohrbach-Schmidt, D., Hall, A. (2013): BIBB/BAuA Employment Survey 2012, BIBB-FDZ Data and Methodological Reports Nr. 1/2013. Version 4.1, Federal Institute for Vocational Education and Training (Research Data Centre), Bonn.
#'
#' Lange, C., Finger, J., Allen, J., Born, S., Hoebel, J., Kuhnert, R., Müters, S., Thelen, J., Schmich, P., Varga, M., von der Lippe, E., Wetzstein, M., Ziese, T. (2017): Implementation of the European Health Interview Survey (EHIS) into the German Health Update (GEDA), Archives of Public Health, 75, 1–14.
#'
#' Hoffmann, R., Lange, M., Butschalowsky, H., Houben, R., Schmich, P., Allen, J., Kuhnert, R., Schaffrath Rosario, A., Gößwald, A. (2018): KiGGS Wave 2 Cross-Sectional Study—Participant Acquisition, Response Rates and Representativeness, Journal of Health Monitoring, 3, 78–91. (only wave 2)
#'
#' Trappmann, M., Beste, J., Bethmann, A., Müller, G. (2013): The PASS Panel Survey after Six Waves, Journal for Labour Market Research, 46, 275–281. (only wave 10)
#'
#' Job titles were taken from the following publication:
#'
#' Bundesagentur für Arbeit (2019). Gesamtberufsliste der Bundesagentur für Arbeit. Stand: 03.01.2019. https://download-portal.arbeitsagentur.de/files/.
#'
#' Basically, leaving some data anonymization steps aside, we count for each job title from the Gesamtberufsliste (and some additional titles/texts) how many responses from all surveys are similar to this job title, separately for each coded category. Similarity is calculated in two ways, implying that we obtain two different counts: SubstringSimilarity refers to situations where the job title from the Gesamtberufsliste is a substring of the verbal answer; WordwiseSimilarity refers to situations where a word from the verbal answer is identical to a job title from the Gesamtberufsliste, except that one character from this word is allowed to change (Levenshtein distance). These counts are available as two separate files in the data-raw/training-data/ directory of this package. The algorithm to create these counts is available inside an R-package at https://github.com/malsch/occupationCoding, along with further documentation.
#'
#' [train_similarity_based_reasoning()] is then used to train the ML models. See data-raw/pretrained_models.R for the raw counts and further details.
"pretrained_models"

#' German Auxiliary Classification of Occupations (AuxCO)
#'
#' Berufs-Hilfsklassifikation mit Tätigkeitsbeschreibungen.
#'
#' @seealso
#' https://github.com/occupationMeasurement/auxiliary-classification, [load_auxco()]
#'
#' @references
#' Schierholz, Malte; Brenner, Lorraine; Cohausz, Lea; Damminger, Lisa; Fast, Lisa; Hörig, Ann-Kathrin; Huber, Anna-Lena; Ludwig, Theresa; Petry, Annabell; Tschischka, Laura (2018): Vorstellung einer Hilfsklassifikation mit Tätigkeitsbeschreibungen für Zwecke der Berufskodierung. (IAB-Discussion Paper, 2018), Nürnberg, 45 S. https://www.iab.de/183/section.aspx/Publikation/k180509301
#'
#' @format A list with data.tables:
#' \describe{
#'   \item{\code{categories}}{data.table. Main list of AuxCO categories including their descriptions etc.}
#'   \item{\code{distinctions}}{data.table. List of highly similar AuxCO categories that one may want to present to disambiguate between them.}
#'   \item{\code{followup_questions}}{data.table. Follow-up questions to specify final codings based on AuxCO categories. Includes the questions' answer options as well as information on how to encode more complex occupations which depend on multiple answers.}
#'   \item{\code{mapping_from_isco}}{data.table. Mapping from ISCO-08 categories to AuxCO categories.}
#'   \item{\code{mapping_from_kldb}}{data.table. Mapping from KldB 2010 categories to AuxCO categories.}
#' }
"auxco"

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
#' @return A list with multiple data.tables.
#' @export
#' @examples
#' # This function expects the CSV files from
#' # https://github.com/occupationMeasurement/auxiliary-classification/releases/
#' # to be there.
#' \dontrun{
#' load_auxco("path/to/auxco/")
#' }
#' @seealso https://github.com/occupationMeasurement/auxiliary-classification, [auxco]
load_auxco <- function(dir, add_explanations = TRUE) {
  # Column names used in data.table (for R CMD CHECK)
  task <- task_description <- NULL

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

  if (add_explanations) {
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

  return(auxco)
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
    check.names = FALSE,
    colClasses = c("character", rep(NA, 11))
  )

  return(as.data.table(kldb_df))
}

#' Clean & Load KldB 2010 dataset.
#'
#' Use load_kldb_raw() to load the whole dataset.
#'
#' Source: https://www.klassifikationsserver.de/klassService/index.jsp?variant=kldb2010
#'
#' More information on the KldB 2010: https://statistik.arbeitsagentur.de/DE/Navigation/Grundlagen/Klassifikationen/Klassifikation-der-Berufe/KldB2010-Fassung2020/KldB2010-Fassung2020-Nav.html The KldB 2010 has been revised in 2020. These changes have not been implemented here yet.
#'
#' @return A cleaned / slimmed version of the KldB 2010.
#' @export
#' @examples
#' load_kldb()
load_kldb <- function() {
  # Column names used in data.table (for R CMD CHECK)
  level <- title <- label <- kldb_id <- NULL

  kldb_data <- load_kldb_raw()

  kldb_new_names <- c(
    "kldb_id",
    "level",
    "title",
    "description",
    "excludes"
  )
  setnames(
    kldb_data,
    # Using two separate vectors here, because Windows cannot handle
    # special characters in names
    old = c(
      "Schl\u00fcssel KldB 2010",
      "Ebene",
      "Titel",
      "Allgemeine Bemerkungen",
      "Ausschl\u00fcsse"
    ),
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

  # Add kldb level 4 labels to KldB level 5
  kldb_lvl4 <- kldb_data[level == 4, ]
  kldb_lvl4_labels <- kldb_lvl4$label
  names(kldb_lvl4_labels) <- kldb_lvl4$kldb_id
  kldb_data[
    level == 5,
    label := kldb_lvl4_labels[substr(kldb_id, 1, 4)]
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

#' Load a standard dataset, while supporting overriding by the user.
#'
#' @param dataset_name Name of the dataset. Currently supported values are
#'   "auxco-1.2.x", "kldb-2010", "isco-08"
#' @param user_provided_data List of datasets provided by the user.
#'
#' @return The requested dataset.
#' @keywords internal
get_data <- function(dataset_name, user_provided_data = list()) {
  # Allow the user to overwrite data
  if (dataset_name %in% user_provided_data) {
    return(user_provided_data[[dataset_name]])
  }

  # Provide default data elsewise
  if (dataset_name == "auxco-1.2.x") {
    return(occupationMeasurement::auxco)
  } else if (dataset_name == "kldb-2010") {
    return(load_kldb())
  } else if (dataset_name == "isco-08") {
    return(occupationMeasurement::isco_08_en)
  }
}

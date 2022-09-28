# Training data of probabilities used for suggestions.

library(data.table)

# Load string preprocessing function
source("R/helper_functions.R")
# Load function to train similarity_based_reasoning model
source("R/model_training.R")

data_directory <- file.path("data-raw")

# Set a seed for exactly reproducible models
set.seed(4666) # random.org 1 - 10000

#' Prepares the Gesamtberufsliste der BA to be used with this package
#'
#' This functions loads the \code{Gesamtberufsliste_der_BA_Januar_2019.xlsx}.
#' It corrects some errors in the Gesamtberufsliste, may remove a few entries,
#' renames some column names, calculates an indicator of job title ambiguity,
#' and, most importantly, it removes parentheses and text in between.
#'
#' With \code{count.categories = TRUE} an indicator of job title ambiguity is
#' calculated. See the source code for examples what exactly is being
#' calculated. This indicator is only needed if one wishes to change the default
#' behaviour in \code{\link{predictWithCodingIndex}} by setting
#' \code{max.count.categories}. This parameter allows to exclude ambiguous job
#' titles (those with large values in count_categories) from the coding index.
#'
#' @references
#' Gesamtberufsliste_der_BA_Januar_2019.xlsx. Bundesagentur für Arbeit (2019): Gesamtberufsliste der Bundesagentur für Arbeit (Datenstand: 3.1.2019). Quelle: DKZ-Download-Portal: ein Angebot der Bundesagentur für Arbeit. Stand: (01/2019)
#'
#' @param path_to_file path to downloaded file
#' @param count.categories (default: \code{FALSE}). The default sets the column \code{count_categories} to 0, avoiding lengthy computations. With \code{count.categories = TRUE} an indicator of job title ambiguity is calculated. See below.
#'
#' @seealso
#' \code{\link{coding_index_excerpt}}, \code{\link{predictWithCodingIndex}}, \code{\link{trainSimilarityBasedReasoning}}
#'
#' @return a data.table with columns Berufsbenennungen, bezMale, and bezFemale, Code, and count_categories.
#' @import data.table
#' @export
prepare_coding_index_ba <- function(path_to_file, count.categories = FALSE) {
  gesamtberufsliste <- data.table(readxl::read_excel(path_to_file, sheet = 2, skip = 4, col_names = c("DKZ-ID", "Codenummer", "Zustand", "Bezeichnung_neutral", "Bezeichnung_maennlich", "Bezeichnung_weiblich")))

  # correct spelling errors
  gesamtberufsliste[Bezeichnung_neutral == "Fellverarbeiterhelfer/n", Bezeichnung_neutral := "Fellverarbeiterhelfer/in"]
  gesamtberufsliste[Bezeichnung_neutral == "Facharzt/-\u00e4rztin - Innere Medizin u. H\u00e4matolog. u. Onkologie" & Bezeichnung_maennlich == "f", Bezeichnung_maennlich := "Facharzt f\u00fcr Innere Medizin und H\u00e4matologie und Onkologie"]

  # some male/female job titles are missing -> insert them
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Ballerina/Ballerino", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Ballerino", "Ballerina")]
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Deichvogt/Deichv\u00f6gtin", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Deichvogt", "Deichv\u00f6gtin")]
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Laufschlosser/-in", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Laufschlosser", "Laufschlosserin")]
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & Bezeichnung_neutral == "Pr\u00fcfwart/-wartin", c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list("Pr\u00fcfwart", "Pr\u00fcfwartin")]
  # if there is no '/' in the neutral title, we can simply insert that neutral title (although it may be just the male or the female title)
  gesamtberufsliste[is.na(Bezeichnung_maennlich) & is.na(Bezeichnung_weiblich) & !grepl("/", Bezeichnung_neutral), c("Bezeichnung_maennlich", "Bezeichnung_weiblich") := list(Bezeichnung_neutral, Bezeichnung_neutral)]

  # if we still have titles without male/female text -> remove them
  # gesamtberufsliste[(is.na(Bezeichnung_maennlich)  | nchar(Bezeichnung_maennlich) < 3) & (is.na(Bezeichnung_weiblich)  | nchar(Bezeichnung_weiblich) < 3)]
  gesamtberufsliste <- gesamtberufsliste[!((is.na(Bezeichnung_maennlich) | nchar(Bezeichnung_maennlich) < 3) & (is.na(Bezeichnung_weiblich) | nchar(Bezeichnung_weiblich) < 3))]

  ## remove text in parenthesis. Examples: Abgleicher/in (Elektro) and Abgleicher/in (Nachrichtentechnik) -> Abgleicher/in
  # gesamtberufsliste[,Bezeichnung_neutral_ohne := gsub(" \\(.*\\)", "", Bezeichnung_neutral)]
  gesamtberufsliste[, bezMale := gsub(" \\(.*\\)", "", Bezeichnung_maennlich)]
  gesamtberufsliste[, bezFemale := gsub(" \\(.*\\)", "", Bezeichnung_weiblich)]

  ## extract Code
  gesamtberufsliste[, Code := substr(Codenummer, 3, 7)]

  # remove superfluous columns (we could also delete Bezeichnung_neutral)
  gesamtberufsliste[, `DKZ-ID` := NULL]
  gesamtberufsliste[, Codenummer := NULL]
  gesamtberufsliste[, Bezeichnung_maennlich := NULL]
  gesamtberufsliste[, Bezeichnung_weiblich := NULL]
  # gesamtberufsliste[, Bezeichnung_neutral_ohne := NULL]
  gesamtberufsliste[, Zustand := NULL]

  if (count.categories) {
    #############
    # add another column `count_categories` (only used within function predictWithCodingIndex if max.count.categories < Inf)
    #############

    # Suppose we try coding a given job title and search the coding for all entries that have this job title as a substring: How many different codes would the coding index suggest?
    # An example for the following:
    # gesamtberufsliste[grepl("Augenoptiker", bezMale, fixed = TRUE) | grepl("Augenoptiker", bezFemale, fixed = TRUE) | grepl("Augenoptikerin", bezMale, fixed = TRUE) | grepl("Augenoptikerin", bezFemale, fixed = TRUE), .N, by = Code]
    # -> unique codings are only possible if a job title (in male or female form) is not a substring of another job title (in male or female form), e.g. Augenoptiker/in has multiple codes because Augenoptiker/in (staatlich gepr\u00fcft).......82523, Augenoptikermeister/in...82593 and Augenoptiktechniker/in..82523 have different codes

    # standarize texts to get more matchtes
    gesamtberufsliste[, bezMaleL := tolower(bezMale)]
    gesamtberufsliste[, bezMaleL := gsub("-", "", bezMaleL)] # e.g. koch/k\u00f6chin should also match Chefkoch/-k\u00f6chin
    gesamtberufsliste[, bezFemaleL := tolower(bezFemale)]
    gesamtberufsliste[, bezFemaleL := gsub("-", "", bezFemaleL)] # e.g. koch/k\u00f6chin should also match Chefkoch/-k\u00f6chin

    # helper function -> with some simple job titles we observe many different categories -> how many
    getNumCategories <- function(male.title, female.title) {
      gesamtberufsliste[grepl(male.title, bezMaleL, fixed = TRUE) | grepl(male.title, bezFemaleL, fixed = TRUE) | grepl(female.title, bezMaleL, fixed = TRUE) | grepl(female.title, bezFemaleL, fixed = TRUE), .N, by = Code][, .N]
    }

    # an example for this -> with some simple job titles we observe many different categories -> how many
    getNumCategories2 <- function(male.title, female.title) {
      gesamtberufsliste[grepl(male.title, bezMaleL, fixed = TRUE) | grepl(male.title, bezFemaleL, fixed = TRUE) | grepl(female.title, bezMaleL, fixed = TRUE) | grepl(female.title, bezFemaleL, fixed = TRUE)]
    }
    # getNumCategories2("koch", "k\u00f6chin")
    # getNumCategories2("koch", "k\u00f6chin")[,.N, by = Code]

    # Suppose we try coding a given job title and search the coding for all entries that have this job title as a substring: How many different codes would the coding index suggest?
    gesamtberufsliste[, count_categories := mapply(getNumCategories, bezMaleL, bezFemaleL)]

    # clean up
    gesamtberufsliste[, bezFemaleL := NULL]
    gesamtberufsliste[, bezMaleL := NULL]
  } else {
    gesamtberufsliste[, count_categories := 0L]
  }

  ################
  # clean up
  # some job titles do not have a male/female form. To avoid that we need to deal with empty cells ("-"), insert the existing form instead.
  # This is a cheap hack that works because we will delete duplicate entries in one of the next steps.
  gesamtberufsliste[bezFemale == "-" | is.na(bezFemale), bezFemale := bezMale]
  gesamtberufsliste[bezMale == "-" | is.na(bezMale), bezMale := bezFemale]

  setnames(gesamtberufsliste, "Bezeichnung_neutral", "Berufsbenennungen")
  gesamtberufsliste
}

# Load anonymized training data from previous research
load(file.path(data_directory, "training-data", "surveyCountsSubstringSimilarity.RData"))
load(file.path(data_directory, "training-data", "surveyCountsWordwiseSimilarity.RData"))

# Load & prepare the German coding index
coding_index_w_codes <- prepare_coding_index_ba(
  file.path(data_directory, "Gesamtberufsliste_der_BA_Januar_2019.xlsx"),
  count.categories = FALSE
)

# Fit the models
model_wordwise <- train_similarity_based_reasoning(
  anonymized_data = surveyCountsWordwiseSimilarity,
  num_allowed_codes = 1291,
  coding_index_w_codes = coding_index_w_codes,
  preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
  dist_type = "wordwise",
  dist_control = list(method = "osa", weight = c(d = 1, i = 1, s = 1, t = 1)),
  threshold = c(max = NA, use = 1),
  simulation_control = list(n.draws = 250, check_normality = FALSE)
)
if (digest::digest(model_wordwise$prediction.datasets) != "4f37b6c3e23055774a4b9d7137ea2ab4") {
  warning("Running the function 'train_similarity_based_reasoning' several times will never give the exact same output. Don't worry, the tiny differences in 'model_wordwise' don't matter in practice.")
}

model_substring <- train_similarity_based_reasoning(
  anonymized_data = surveyCountsSubstringSimilarity,
  num_allowed_codes = 1291,
  coding_index_w_codes = coding_index_w_codes,
  preprocessing = list(stopwords = NULL, stemming = NULL, strPreprocessing = TRUE, removePunct = FALSE),
  dist_type = "substring",
  dist_control = NA,
  threshold = NA,
  simulation_control = list(n.draws = 250, check_normality = FALSE)
)
if (digest::digest(model_substring$prediction.datasets) == "84e4dcf533b4a553155110bcf82e8749") {
  warning("Running the function 'train_similarity_based_reasoning' several times will never give the exact same output. Don't worry, the tiny differences in 'model_substring' don't matter in practice.")
}

# Combine model output
pretrained_models <- list(
  similarity_based_reasoning = list(
    wordwise = model_wordwise$prediction.datasets,
    substring = model_substring$prediction.datasets
  )
)

usethis::use_data(pretrained_models, overwrite = TRUE)

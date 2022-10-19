# Load AuxCo data, this is the main dataset used to generate suggestions
# Run the following code to create four .csv-Files from auxiliary classification
# CSVs are generated via this code:
# https://github.com/malsch/occupationCodingAuxco/blob/master/generate_CSVs.R

library(data.table)
source("R/data.R")

auxco_directory <- file.path("data-raw", "auxco")
auxco <- load_auxco(
  dir = auxco_directory,
  add_explanations = TRUE
)

usethis::use_data(auxco, overwrite = TRUE)

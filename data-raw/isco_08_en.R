# Clean & Load the ISCO 08 dataset

# We're using the dataset of ISCO categories bundled with the ESCO v1.1.

# Terms of Use:
# Specific Conditions applying for the ESCO Service
# In accordance with the Commission Decision of 12 December 2011 on the reuse of Commission documents (2011/833/EU), the ESCO classification can be downloaded, used, reproduced and reused for any purpose and by any interested party free of charge. It may be linked with existing taxonomies or classifications for supplementing and mapping purposes. Any use is subject to the following conditions:
# 1) The use of ESCO shall be acknowledged by publishing the statement below: - For services, tools and applications integrating totally or partially ESCO: "This service uses the ESCO classification of the European Commission." - For other documents such as studies, analysis or reports making use of ESCO: "This publication uses the ESCO classification of the European Commission.:
# 2) Any modified or adapted version of ESCO must be clearly indicated as such. Please, be aware that it cannot be guaranteed that the information provided on the ESCO website or in the downloaded files is accurate, up-to-date or complete. The same applies to the quality of translated terms within the classification. The Commission shall not be liable for any consequence stemming from the use, reuse or deployment of the ESCO
# 3) The user agrees that the information submitted in the form below may be used for purposes indicated in the section 2-a.

library(data.table)

isco_raw <- fread(
  file.path("data-raw", "ESCO dataset - v1.1.0 - classification - en - csv", "ISCOGroups_en.csv"),
  encoding = "UTF-8"
)
isco_raw$level <- nchar(isco_raw$code)

setnames(
  isco_raw,
  old = c(
    "preferredLabel"
  ),
  new = c(
    "label"
  )
)

isco_08_en <- isco_raw[, c("code", "label", "description")]

usethis::use_data(isco_08_en, overwrite = TRUE)

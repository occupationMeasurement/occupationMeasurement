library(testthat)
library(occupationMeasurement)

# Limit number of threads used by data.table
# More info here: https://github.com/Rdatatable/data.table/issues/5658
data.table::setDTthreads(1)

test_check("occupationMeasurement")

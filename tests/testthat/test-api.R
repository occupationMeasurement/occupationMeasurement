# Setup by starting APIs
api_root <- "http://localhost"
port <- 14700

log_file <- withr::local_tempfile()

# Determine how the package was installed, because devtools::test()
# and R CMD CHECK use slightly different ways of installation
# This is necessary, because we manually need to load the package to make it
# available in the background process of callr
package_info <- sessioninfo::package_info("occupationMeasurement")
package_info <- package_info[package_info$package == "occupationMeasurement", ]

# We need to manually check for R CMD CHECK, because loadedpath
# will correspond to different things based on how the package was loaded.
# For R CMD CHECK it will correspond to the built package which just needs
# to be laoded, for devtools::test it will correspond to the package source
installed_via_check <- package_info$path != "" && package_info$source != "load_all()"

if (installed_via_check) {
  # Re-use the installation from R CMD CHECK and directly load
  # via library()
  temporary_library_path <- package_info$loadedpath |>
    dirname()
} else {
  # Locally install the package again from source, when running
  # via devtools::test()
  local_package(
    pkg = package_info$loadedpath
  )
}

api_process <- callr::r_bg(
  function() {
    if (installed_via_check) {
      library(occupationMeasurement, lib.loc = temporary_library_path)
    } else {
      library(occupationMeasurement)
    }

    api(
      start = FALSE,
      log_filepath = log_file,
      allow_origin = "https://occupationMeasurement.github.io"
    ) |>
      plumber::pr_run(port = port)
  },
  # Pass environment
  package = TRUE
)

withr::defer({
  api_process$kill()
})

# Give plumber time to start the API
Sys.sleep(2)

test_that("API started successfully", {
  if (!api_process$is_alive()) {
    # Get output if the API crashed
    print(api_process$get_result())
  }

  expect_true(api_process$is_alive())
})

test_that("endpoint '/' works", {
  # Send API request
  r <- httr::GET(api_root, port = port, path = "/")

  # Check response
  expect_equal(r$status_code, 200)
})

test_that("endpoint '/v1/suggestions' works (with suggestions)", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/suggestions",
    query = list(
      text = "Friseur"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
  # Check CORS header
  expect_equal(r$headers$`access-control-allow-origin`, "https://occupationMeasurement.github.io")
})

test_that("endpoint '/v1/suggestions' works (w/o suggestions)", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/suggestions",
    query = list(
      text = "A piece of text that does not tell you anything"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})

test_that("endpoint '/v1/next_followup_question' works", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/next_followup_question",
    query = list(
      suggestion_id = "7078"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})

test_that("endpoint '/v1/next_followup_question' works (when coding is finished)", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/next_followup_question",
    query = list(
      suggestion_id = "7078",
      followup_question_id = "Q7078_1",
      followup_answer_id = "1"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})

test_that("endpoint '/v1/next_followup_question' works with multiple followup questions (incl. early end)", {
  # Get first followup question
  first <- httr::GET(
    api_root,
    port = port,
    path = "/v1/next_followup_question",
    query = list(
      suggestion_id = "5078"
    )
  )
  # Check response
  expect_equal(first$status_code, 200)
  expect_snapshot_value(httr::content(first, encoding = "UTF-8"))

  # Send the answer to the first followup question *with* early end
  second_early_end <- httr::GET(
    api_root,
    port = port,
    path = "/v1/next_followup_question",
    query = list(
      suggestion_id = "5078",
      followup_question_id = "Q5078_1",
      followup_answer_id = "1"
    )
  )
  # Check response
  expect_equal(second_early_end$status_code, 200)
  second_early_end_content <- httr::content(second_early_end, encoding = "UTF-8")
  expect_snapshot_value(second_early_end_content)
  expect_true(second_early_end_content$coding_is_finished)

  # Send the answer to the first followup question without early end
  second <- httr::GET(
    api_root,
    port = port,
    path = "/v1/next_followup_question",
    query = list(
      suggestion_id = "5078",
      followup_question_id = "Q5078_1",
      followup_answer_id = "3"
    )
  )
  # Check response
  expect_equal(second$status_code, 200)
  expect_snapshot_value(httr::content(second, encoding = "UTF-8"))

  # Send third and final API request when coding is finished (optional)
  third <- httr::GET(
    api_root,
    port = port,
    path = "/v1/next_followup_question",
    query = list(
      suggestion_id = "5078",
      followup_question_id = "Q5078_2",
      followup_answer_id = "2"
    )
  )
  # Check response
  expect_equal(third$status_code, 200)
  expect_snapshot_value(httr::content(third, encoding = "UTF-8"))
})

test_that("endpoint '/v1/final_codes' works (without followup answers)", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/final_codes",
    query = list(
      suggestion_id = "7078"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})


test_that("endpoint '/v1/final_codes' works (with followup answers)", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/final_codes",
    query = list(
      suggestion_id = "7078",
      followup_answers = list(
        1
      )
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})


test_that("endpoint '/v1/final_codes' works (with partial followup answers)", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/final_codes",
    query = list(
      suggestion_id = "1836",
      followup_answers = list(
        4
      )
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})


test_that("endpoint '/v1/final_codes' works (with standardized followup answers)", {
  # approximate matching: skill level
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/final_codes",
    query = list(
      suggestion_id = "1706",
      isco_skill_level = "isco_skill_level_1"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))

  # approximate matching: isco_manager
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/final_codes",
    query = list(
      suggestion_id = "1783",
      isco_supervisor_manager = "isco_manager"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
})

test_that("API logging is working", {
  # Check whether the log file exists
  expect_true(file.exists(log_file))

  log <- log_file |>
    read.csv()

  # Check whether CSV fields are unchanged
  expect_snapshot_value(log |> colnames(), style = "deparse")
})

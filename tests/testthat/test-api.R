# Setup by starting APIs
api_root <- "http://localhost"
port <- 8000

log_file <- withr::local_tempfile()

api_process <- callr::r_bg(
  function() {
    occupationMeasurement::api(
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

test_that("endpoint '/v1/followup_questions' works", {
  # Send API request
  r <- httr::GET(
    api_root,
    port = port,
    path = "/v1/followup_questions",
    query = list(
      suggestion_id = "7078"
    )
  )

  # Check response
  expect_equal(r$status_code, 200)
  expect_snapshot_value(httr::content(r, encoding = "UTF-8"))
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

test_that("API logging is working", {
  # Check whether the log file exists
  expect_true(file.exists(log_file))

  log <- log_file |>
    read.csv()

  # Check whether CSV fields are unchanged
  expect_snapshot_value(log |> colnames(), style = "deparse")
})


testthat::test_that("app_settings are generated from env vars", {
  withr::local_envvar(
    # Check strings
    SUGGESTION_TYPE = "kldb-2010",
    # And booleans (re conversion)
    REQUIRE_RESPONDENT_ID = TRUE
  )

  expect_equal(
    occupationMeasurement:::create_app_settings_from_env(verbose = FALSE),
    create_app_settings(
      save_to_file = TRUE,
      suggestion_type = "kldb-2010",
      require_respondent_id = TRUE
    )
  )
})

testthat::test_that("create_app_settings parameters are robust to case changes", {
  # Because we convert param names between upper and lower case in
  # the function create_app_settings_from_env()

  possible_params <- create_app_settings |>
    args() |>
    as.list() |>
    names()

  expect_equal(
    possible_params,
    possible_params |>
      toupper() |>
      tolower()
  )
})

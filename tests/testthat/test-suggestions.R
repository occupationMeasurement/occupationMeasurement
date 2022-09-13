test_that("suggestions are generated correctly for 'Koch' in default format (auxco)", {
  expect_snapshot_value(style = "json2", tolerance = .001, as.data.frame(get_job_suggestions("Koch")))
})

test_that("suggestions are generated correctly for 'Koch' in kldb format", {
  expect_snapshot_value(style = "json2", tolerance = .001, as.data.frame(get_job_suggestions("Koch", suggestion_type = "kldb")))
})

test_that("similarity based reasoning works as expected", {
  expect_snapshot_value(style = "json2", tolerance = .001, as.data.frame(algo_similarity_based_reasoning("KOCH", suggestion_type = "auxco")))
})

test_that("suggestions and final codes are correctly generated for 'Soldat' (leading 0 in ISCO)", {
  # Check suggestions
  expect_snapshot_value(style = "json2", tolerance = .001, as.data.frame(get_job_suggestions("Soldat")))

  # Check final codes for default case
  expect_snapshot_value(style = "json2", tolerance = .001, get_final_codes("9999", followup_answers = list()))
  # and when supplying a followup answer
  expect_snapshot_value(style = "json2", tolerance = .001, get_final_codes("9999", followup_answers = list(Q9999_1 = 3)))
})

test_that("final codes are correctly generated answers which depend on multiple followup questions", {
  # Electronics engineers
  expect_equal(
    get_final_codes(
      "1733",
      followup_answers = list(
        Q1733_1 = 1,
        Q1733_2 = 2
      )
    ),
    list(
      isco_08 = "3113",
      kldb_10 = "26303"
    )
  )

  # Electronics engineering technicians
  expect_equal(
    get_final_codes(
      "1733",
      followup_answers = list(
        Q1733_1 = 2,
        Q1733_2 = 2
      )
    ),
    list(
      isco_08 = "3114",
      kldb_10 = "26303"
    )
  )
})

test_that("final codes are irrespective of the question order", {
  expect_equal(
    get_final_codes(
      "1710",
      followup_answers = list(
        Q1710_2 = 3,
        Q1710_1 = 2
      )
    ),
    list(
      isco_08 = "6123",
      kldb_10 = "11293"
    )
  )

  # Electronics engineering technicians (dependend on both follow up question answers)
  expect_equal(
    get_final_codes(
      "1733",
      followup_answers = list(
        Q1733_2 = 2,
        Q1733_1 = 1
      )
    ),
    list(
      isco_08 = "3113",
      kldb_10 = "26303"
    )
  )
})

test_that("final_codes throws an error when used improperly", {
  # Electronics engineering technicians
  expect_error(
    get_final_codes(
      "1733",
      followup_answers = list(
        1,
        2
      )
    )
  )
})

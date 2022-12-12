test_that("Performance evaluation works as expected", {
  message <- capture.output(
    evaluation_result <- evaluate_performance(
      test_data = data.frame(
        freetext_answer = c(
          "Koch",
          "Donaudampschifffahrtskapitän",
          "Arbeiter"
        ),
        kldb_code = c(
          "29302",
          "52423",
          "72213"
        )
      ),
      freetext_colname = "freetext_answer",
      code_colname = "kldb_code",
      code_format = "kldb-2010"
    )
  )

  # Check whether both parts of the evaluation result are unchanged
  expect_snapshot_value(evaluation_result$scores, tolerance = 0.001)
  expect_snapshot_value(evaluation_result$data, style = "json2")
  # Note: We could also test against a snapshot of the message,
  # but it seems overkill since we capture all the underlying metrics
})

test_that("Performance evaluation works with KldB-2010 Suggestions", {
  message <- capture.output(
    evaluation_result <- evaluate_performance(
      test_data = data.frame(
        freetext_answer = c(
          "Koch",
          "Donaudampschifffahrtskapitän",
          "Arbeiter"
        ),
        kldb_code = c(
          "29302",
          "52423",
          "72213"
        )
      ),
      freetext_colname = "freetext_answer",
      code_colname = "kldb_code",
      code_format = "kldb-2010",
      app_settings = create_app_settings(
        suggestion_type = "kldb-2010",
        .validate = FALSE
      )
    )
  )

  # Check whether both parts of the evaluation result are unchanged
  expect_snapshot_value(evaluation_result$scores, tolerance = 0.001)
  expect_snapshot_value(evaluation_result$data, style = "json2")
  # Note: We could also test against a snapshot of the message,
  # but it seems overkill since we capture all the underlying metrics
})

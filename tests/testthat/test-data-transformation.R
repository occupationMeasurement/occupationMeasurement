testthat::test_that("data is transformed correctly", {
  questionnaire_data <- list(
    welcome = list(
      page_id = "welcome",
      respondent_id = NA,
      session_id = "NA_1660052665_9",
      status = "new",
      start = "2022-08-09 15:44:25",
      questions = list(),
      end = "2022-08-09 15:44:26"
    ),
    freetext_1 = list(
      page_id = "freetext_1",
      respondent_id = NA,
      session_id = "NA_1660052665_9",
      status = "new",
      start = "2022-08-09 15:44:26",
      questions = list(
        default = list(
          question_text = "Welche berufliche T\u00e4tigkeit \u00fcben Sie derzeit haupts\u00e4chlich aus?",
          response_text = "koch"
        ),
        no_answer = list(
          question_text = "Welche berufliche T\u00e4tigkeit \u00fcben Sie derzeit haupts\u00e4chlich aus?",
          response_id = FALSE
        )
      ),
      end = "2022-08-09 15:44:28"
    ),
    select_suggestion = list(
      page_id = "select_suggestion",
      respondent_id = NA,
      session_id = "NA_1660052665_9",
      status = "new",
      start = "2022-08-09 15:44:28",
      questions = list(
        default = list(
          question_text = "Welche der folgenden Beschreibungen trifft am ehesten f\u00fcr Ihren Beruf zu? Wenn mehrere Beschreibungen zutreffen,
          denken Sie bitte an diejenige T\u00e4tigkeit,
          die Sie haupts\u00e4chlich aus\u00fcben.",
          response_id = "9079"
        ),
        text_none_selected = list(
          response_id = ""
        )
      ),
      end = "2022-08-09 15:44:30"
    ),
    final = list(
      page_id = "final",
      respondent_id = NA,
      session_id = "NA_1660052665_9",
      status = "old",
      start = "2022-08-09 15:44:30",
      questions = list()
    )
  )

  expected_output <- data.table(
    respondent_id = NA,
    session_id = "NA_1660052665_9",
    P_welcome_Q_NA_R_id = NA_character_,
    P_welcome_Q_NA_R_text = NA_character_,
    P_freetext_1_Q_default_R_id = NA_character_,
    P_freetext_1_Q_default_R_text = "koch",
    P_freetext_1_Q_no_answer_R_id = "FALSE",
    P_freetext_1_Q_no_answer_R_text = NA_character_,
    P_select_suggestion_Q_default_R_id = "9079",
    P_select_suggestion_Q_default_R_text = NA_character_,
    P_select_suggestion_Q_text_none_selected_R_id = "",
    P_select_suggestion_Q_text_none_selected_R_text = NA_character_,
    P_final_Q_NA_R_id = NA_character_,
    P_final_Q_NA_R_text = NA_character_,
    key = c("respondent_id", "session_id")
  )

  testthat::expect_equal(
    extract_questions_wide(
      questionnaire_data = questionnaire_data
    ),
    expected_output
  )
})

testthat::test_that("extract_questions_wide is robust", {
  testthat::expect_equal(
    extract_questions_wide(questionnaire_data = NULL),
    data.table()
  )

  testthat::expect_equal(
    extract_questions_wide(questionnaire_data = list()),
    data.table()
  )

  testthat::expect_equal(
    extract_questions_wide(
      questionnaire_data =  list(
        welcome = list(
          page_id = "test",
          respondent_id = "test_123",
          session_id = "NA_1660052665_9",
          status = "new",
          start = "2022-08-09 15:44:25",
          questions = list(),
          end = "2022-08-09 15:44:26"
        )
      )
    ),
    data.table(
      respondent_id = "test_123",
      session_id = "NA_1660052665_9",
      P_test_Q_NA_R_id = NA_character_,
      P_test_Q_NA_R_text = NA_character_,
      key = c("respondent_id", "session_id")
    )
  )
})

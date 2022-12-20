library(shinytest2)

# Use edition 2 api of testthat
# Only using it locally here due to shinytest2 having issues with e3
local_edition(2)

# Init code to be able to call "checkForChange()"
setup_wait_for_change <- function(app) {
  # Start counting changes to MainContent
  app$run_js("
    var observeDOM = (function(){
      var MutationObserver = window.MutationObserver || window.WebKitMutationObserver;

      return function( obj, callback ){
        if( !obj || obj.nodeType !== 1 ) return;

        if( MutationObserver ){
          // define a new observer
          var mutationObserver = new MutationObserver(callback)

          // have the observer observe foo for changes in children
          mutationObserver.observe( obj, { childList:true, subtree:true })
          return mutationObserver
        }

        // browser support fallback
        else if( window.addEventListener ){
          obj.addEventListener('DOMNodeInserted', callback, false)
          obj.addEventListener('DOMNodeRemoved', callback, false)
        }
      }
    })();

    window.numberOfChanges = 0;
    observeDOM(document.getElementById('MainAction'), function () {
      console.log('Change detected');
      window.numberOfChanges++;
    })

    window.checkForChange = function (minNumberOfChanges = 1) {
      if (window.numberOfChanges >= minNumberOfChanges) {
        if (window.numberOfChanges > minNumberOfChanges) {
          console.warn('Note: Higher Number of Changes than expected', window.numberOfChanges);
        }
        window.numberOfChanges = 0;
        return true;
      } else {
        return false;
      }
    }
  ")
}

debug_test_case <- TRUE
debug_note <- function(...) {
  if (debug_test_case) {
    paste(
      ...,
      sep = "\n"
    ) |>
      cat()
  }
}
is_ci <- !is.na(as.logical(Sys.getenv("CI"))) && !as.logical(Sys.getenv("CI"))

test_that("E2E: test case Koch", {
  skip_on_ci()

  response_dir <- withr::local_tempdir()

  app_settings <- create_app_settings(
    require_respondent_id = TRUE,
    save_to_file = TRUE,
    response_output_dir = response_dir
  )

  app <- AppDriver$new(
    app_dir = app(
      questionnaire = questionnaire_demo(
        show_feedback_page = FALSE
      ),
      app_settings = app_settings
    ),
    name = "test_1_koch",
    height = 741,
    width = 1139
  )

  wait_time <- 1

  # To Debug in Chrome
  if (debug_test_case && is_ci) {
    app$view()
  }

  # Go to example URL
  app$run_js("window.location.href = '/?respondent_id=test123&tense=present&extra_instructions=on&study_id=Test'")
  Sys.sleep(wait_time)

  setup_wait_for_change(app)

  # Navigate beyond welcome page
  debug_note("Welcome")
  app$click(selector = "#nextButton")
  app$wait_for_js("checkForChange()")

  # Answer the freetext question
  debug_note("Freetext (1)")
  app$run_js("document.querySelector('input').value = 'Techniker/in - Informations- und Kommunikationselektronik'")
  app$run_js("
    var element = document.querySelector('input');
    if ('createEvent' in document) {
      var evt = document.createEvent('HTMLEvents');
      evt.initEvent('change', false, true);
      element.dispatchEvent(evt);
    } else {
      element.fireEvent('onchange');
    }
  ")
  Sys.sleep(0.5)
  app$click(selector = "#nextButton")
  app$wait_for_js("document.querySelector('table')?.childNodes.length === 5")
  app$run_js("window.numberOfChanges = 0;") # reset counter

  # Select a suggestion
  debug_note("Select Suggestion (1)")
  app$click(selector = "#question1 > div > div:nth-child(5) > label > input[type=radio]")
  app$click(selector = "#nextButton")
  app$wait_for_js("checkForChange()")

  # Follow up question
  debug_note("Follow-Up (1)")
  app$click(selector = "div.radio:nth-child(1) > label:nth-child(1)")
  app$click(selector = "#nextButton")
  app$wait_for_js("document.querySelector('table')?.childNodes.length === 5")
  app$run_js("window.numberOfChanges = 0;") # reset counter

  # Go back

  # Back to follow-up
  debug_note("Back - 1")
  app$click(selector = "#previousButton")
  app$wait_for_js("checkForChange()")
  # Back to suggestion selection
  debug_note("Back - 2")
  app$click(selector = "#previousButton")
  app$wait_for_js("document.querySelector('table')?.childNodes.length === 5")
  app$run_js("window.numberOfChanges = 0;") # reset counter
  # Back to free-text entry
  debug_note("Back - 3")
  app$click(selector = "#previousButton")
  app$wait_for_js("checkForChange()")


  # Answer the freetext question
  debug_note("Freetext (2)")
  app$run_js("document.querySelector('input').value = 'Koch'")
  app$run_js("
    var element = document.querySelector('input');
    if ('createEvent' in document) {
      var evt = document.createEvent('HTMLEvents');
      evt.initEvent('change', false, true);
      element.dispatchEvent(evt);
    } else {
      element.fireEvent('onchange');
    }
  ")
  Sys.sleep(0.5)
  app$click(selector = "#nextButton")
  app$wait_for_js("document.querySelector('table')?.childNodes.length === 5")
  app$run_js("window.numberOfChanges = 0;") # reset counter

  # Check suggestions
  app$expect_text("body")

  # Select a suggestion
  debug_note("Select Suggestion (2)")
  app$click(selector = "#question1 > div > div:nth-child(1) > label > input[type=radio]")
  app$click(selector = "#nextButton")
  app$wait_for_js("checkForChange()")

  # Follow up question
  debug_note("Follow-Up (2)")
  app$click(selector = "div.radio:nth-child(1) > label:nth-child(1)")
  app$click(selector = "#nextButton")
  app$wait_for_js("document.querySelector('table')?.childNodes.length === 5")
  app$run_js("window.numberOfChanges = 0;") # reset counter

  # Check final output
  app$expect_text("body")

  # To Output Shiny Logs in the end
  # print(app$get_logs())

  overview_data <- get_responses(app_settings = app_settings)

  testthat::expect_equal(
    overview_data,
    data.table::data.table(
      session_id = overview_data$session_id,
      url_search = "?respondent_id=test123&tense=present&extra_instructions=on&study_id=Test",
      isco_08 = "9411",
      kldb_10 = "63312",
      respondent_id = "test123",
      P_welcome_Q_NA_R_id = "",
      P_welcome_Q_NA_R_text = "",
      P_freetext_1_Q_default_R_id = "",
      P_freetext_1_Q_default_R_text = "Koch",
      P_freetext_1_Q_no_answer_R_id = "no_answer_unchecked",
      P_freetext_1_Q_no_answer_R_text = "",
      P_select_suggestion_Q_default_R_id = "9079",
      P_select_suggestion_Q_default_R_text = "Zubereitung von Speisen in Restaurants, Hotels und sonstigen gastronomischen Einrichtungen",
      P_followup_1_Q_default_R_id = "1",
      P_followup_1_Q_default_R_text = "nur einfache Speisen zum Sofortverkauf in Fast-Food-Restaurants"
    )
  )
})

test_that("Followup questions are correctly skipped: ESE test case Textiltechniker)", {
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = app(
      app_settings = create_app_settings(
        # Don't write data from test to disk
        save_to_file = FALSE
      ),
      questionnaire = questionnaire_interviewer_administered(
        show_feedback_page = FALSE
      )
    ),
    name = "test_2_textiltechniker",
    height = 741,
    width = 1139
  ) |>
    expect_warning("saving to files has been disabled")

  wait_time <- 1

  # To Debug in Chrome
  # app$view()

  setup_wait_for_change(app)

  # Answer the freetext question
  app$run_js("document.querySelector('input').value = 'Textiltechniker'")
  app$run_js("
    var element = document.querySelector('input');
    if ('createEvent' in document) {
      var evt = document.createEvent('HTMLEvents');
      evt.initEvent('change', false, true);
      element.dispatchEvent(evt);
    } else {
      element.fireEvent('onchange');
    }
  ")
  Sys.sleep(0.5)
  app$click(selector = "#nextButton")
  app$wait_for_js("checkForChange()") # Waiting for two changes here due to renderTable

  app$run_js("document.querySelector('input').value = 'Arbeit mit Textilien'")
  app$run_js("
    var element = document.querySelector('input');
    if ('createEvent' in document) {
      var evt = document.createEvent('HTMLEvents');
      evt.initEvent('change', false, true);
      element.dispatchEvent(evt);
    } else {
      element.fireEvent('onchange');
    }
  ")
  Sys.sleep(0.5)
  app$click(selector = "#nextButton")

  app$wait_for_js("checkForChange()") # Waiting for two changes here due to renderTable

  # Check suggestions
  app$expect_text("body")

  # Select a suggestion
  app$click(selector = "#question1 > div > div:nth-child(1) > label > input[type=radio]")
  app$click(selector = "#nextButton")
  app$wait_for_js("checkForChange()")

  # Follow up question
  app$click(selector = "div.radio:nth-child(1) > label:nth-child(1)")
  app$click(selector = "#nextButton")
  app$wait_for_js("checkForChange()") # Waiting for two changes here due to renderTable

  # Check final output
  app$expect_text("body")

  # To Output Shiny Logs in the end
  # print(app$get_logs())
})

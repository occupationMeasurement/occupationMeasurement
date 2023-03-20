# Small helper functions to be used when creating questionnaire pages

#' Go to the next page
#'
#' Buttons to navigate between pages.
#' @param label What label the button should have.
#' @return shiny Action Button
#' @export
#' @seealso [new_page()]
#' @examples
#' very_simple_page <- new_page(
#'   page_id = "example",
#'   render = function(session, run_before_output, input, output, ...) {
#'     list(
#'       shiny::tags$h1("My test page"),
#'       button_previous(),
#'       button_next()
#'     )
#'   }
#' )
button_next <- function(label = "Weiter") {
  actionButton("nextButton", label)
}
#' @describeIn button_next Go to the previous page
#' @export
button_previous <- function(label = "Zur\u00fcck") {
  actionButton("previousButton", label)
}
mark_questionnaire_complete <- function() {
  tags$script("window.study_completed = true;")
}

#' Show a page with multiple radio button options where once can be picked.
#' @inheritParams page_freetext
#' @param list_of_choices A list of answering options.
#'   This can either be just a simple list of values or a named list with the
#'   names corresponding to what the user sees and the values corresponding to
#'   the actually saved values.
#'   e.g. with `list(One = 1, Two = 2, Three = 3)` people will see One, Two, ...
#'   and numbers 1, 2, ... will be saved under `response_id`.
#'   If you want to use more complex choice names than jsut strings (i.e. HTML),
#'    you can also use the choice_labels option for that.
#' @param choice_labels List or vector of only the choice names to be shown.
#'   This has to be matched by an equal-length vector in list_of_choices.
#' @param ... Other parametrs are passed on to `new_page()`
#' @return A page object.
#' @seealso [new_page()]
#' @export
#' @examples
#' one_page_questionnaire <- list(
#'   page_choose_one_option(
#'     "test_page_radio",
#'     question_text = "Hello there! Please pick your favorite number from the options below:",
#'     list_of_choices = list(One = 1, Two = 2, Three = 3)
#'   ),
#'   page_final()
#' )
#' if (interactive()) {
#'   app(questionnaire = one_page_questionnaire)
#' }
#'
page_choose_one_option <- function(page_id,
                                   question_text = "Please pick one of the following options",
                                   list_of_choices = list(One = 1, Two = 2, Three = 3),
                                   choice_labels = NULL,
                                   next_button = TRUE,
                                   previous_button = TRUE,
                                   run_before = NULL,
                                   run_after = NULL,
                                   ...) {
  # Potentially generate choice_labels from list_of_choices
  choice_values <- unlist(list_of_choices)
  names(choice_values) <- NULL

  if (is.null(choice_labels)) {
    if (!is.null(names(list_of_choices))) {
      choice_labels <- names(list_of_choices)
    } else {
      choice_labels <- choice_values
    }
  }

  new_page(
    page_id = page_id,
    run_before = function(session, page, ...) {
      if (is.function(question_text)) {
        # Handle dynamic question texts if a function is passed instead of a string
        actual_question_text <- question_text(session, page, ...)
      } else {
        actual_question_text <- question_text
      }
      set_item_data(
        session = session,
        page_id = page$page_id,
        question_text = actual_question_text
      )

      # Support a custom run_before
      if (!is.null(run_before)) {
        extra_run_before_output <- run_before(session, page, ...)
      } else {
        extra_run_before_output <- list()
      }

      utils::modifyList(
        list(
          question_text = actual_question_text
        ),
        extra_run_before_output
      )
    },
    render = function(session, page, run_before_output, ...) {
      list(
        shiny::tags$div(class = "question", run_before_output$question_text),
        radioButtons("radioButtonQuestion", NULL,
          width = "100%",
          choiceNames = choice_labels,
          choiceValues = choice_values,
          selected = get_item_data(session = session, page_id = page$page_id, key = "response_id", default = character())
        ),
        if (previous_button) button_previous(),
        if (next_button) button_next()
      )
    },
    run_after = function(session, page, input, ...) {
      response_id <- input[["radioButtonQuestion"]]
      set_item_data(
        session = session,
        page_id = page$page_id,
        response_id = as.character(response_id),
        response_text = choice_labels[choice_values == response_id]
      )

      # Support a custom run_after
      if (!is.null(run_after)) {
        run_after(session, page, input, ...)
      }
    },
    ...
  )
}

#' Show a page with a text field where free text can be entered.
#' @param page_id A unique string identifiying this page. Used to store data.
#' @param question_text The question / text to display.
#'   This can be either a string, which will simply be displayed or a function
#'   to dynamically determine the question_text.
#' @param is_interview Should the page show slightly different / additional
#'  instructions and answer options for an interview that is conducted by
#'  another person? Defaults to FALSE.
#' @param no_answer_checkbox Whether to provide a checkbox to denote that no
#'   answer has been provided.
#' @param next_button Whether to show the button to navigate to the next page?
#'   Defaults to TRUE.
#' @param previous_button Whether to show the button to navigate to the preivous page?
#'   Defaults to TRUE.
#' @param trigger_next_on_enter Whether the next button is triggered
#'   when one presses enter. Defaults to TRUE. There are known issues with IE11.
#' @param render_question_text Whether the question text should be displayed?
#'   Only set this to FALSE, if you wish to change the rendering of the
#'   question_text by e.g. using `render_before`.
#'   Defaults to TRUE.
#' @param run_before Similar to `run_before` in `new_page()`, passed explicitly
#'   here as this page adds some of its own code to `run_before`.
#' @param run_after Similar to `run_after` in `new_page()`, passed explicitly
#'   here as this page adds some of its own code to `run_after`.
#' @param ... Other parametrs are passed on to `new_page()`
#' @return A page object.
#' @seealso [new_page()]
#' @export
#' @examples
#' page_freetext(
#'   "test_page_freetext",
#'   question_text = "Hello there! Please fill in your name below:",
#'   no_answer_checkbox = TRUE
#' )
page_freetext <- function(page_id,
                          question_text = "Please enter your answer in the box below",
                          is_interview = FALSE,
                          no_answer_checkbox = TRUE,
                          next_button = TRUE,
                          previous_button = TRUE,
                          trigger_next_on_enter = TRUE,
                          render_question_text = TRUE,
                          run_before = NULL,
                          run_after = NULL,
                          ...) {
  new_page(
    page_id = page_id,
    run_before = function(session, page, ...) {
      if (is.function(question_text)) {
        # Handle dynamic question texts if a function is passed instead of a string
        actual_question_text <- question_text(session, page, ...)
      } else {
        actual_question_text <- question_text
      }
      set_item_data(
        session = session,
        page_id = page$page_id,
        question_text = actual_question_text
      )
      if (no_answer_checkbox) {
        set_item_data(
          session = session,
          page_id = page$page_id,
          item_id = "no_answer",
          question_text = actual_question_text
        )
      }

      # Support a custom run_before
      if (!is.null(run_before)) {
        extra_run_before_output <- run_before(session, page, ...)
      } else {
        extra_run_before_output <- list()
      }

      utils::modifyList(
        list(
          question_text = actual_question_text
        ),
        extra_run_before_output
      )
    },
    render = function(session, page, run_before_output, ...) {
      list(
        if (render_question_text) p(run_before_output$question_text),
        if (trigger_next_on_enter) {
          # Known issue with IE11: If one types fast and presses enter,
          # not the complete text gets used/is saved.
          tags$script(paste0('var $nextButton = $("#nextButton");
                      $("#', page$page_id, '_text").on("keyup", function(e) {
                        if(e.keyCode == 13){
                          $nextButton.click();
                        }
                      });'))
        },
        textInput(
          paste0(page$page_id, "_text"),
          NULL,
          value = get_item_data(session = session, page_id = page$page_id, key = "response_text"),
          width = "80%"
        ),
        br(),
        if (no_answer_checkbox) {
          checkboxInput(
            paste0(page$page_id, "_chk_no_answer"),
            label = if (is_interview) {
              p(class = "interviewer", "*** Keine Angabe")
            } else {
              p("Keine Angabe")
            },
            # Check whether it matches our recoded value of "no_answer_checked"
            value = get_item_data(
              session = session,
              page_id = page$page_id,
              item_id = "no_answer",
              key = "response_id",
              default = FALSE
            ) == "no_answer_checked",
            width = "100%"
          )
        },
        if (previous_button) button_previous(),
        if (next_button) button_next()
      )
    },
    run_after = function(session, page, input, ...) {
      set_item_data(
        session = session,
        page_id = page$page_id,
        response_text = if (is.null(input[[paste0(page$page_id, "_text")]])) "" else input[[paste0(page$page_id, "_text")]]
      )

      # Also record no-answer checkbox
      if (no_answer_checkbox) {
        no_answer_checked <- if (is.null(input[[paste0(page$page_id, "_chk_no_answer")]])) FALSE else input[[paste0(page$page_id, "_chk_no_answer")]]

        set_item_data(
          session = session,
          page_id = page$page_id,
          item_id = "no_answer",
          response_id = if (no_answer_checked) "no_answer_checked" else "no_answer_unchecked",
          response_text = if (no_answer_checked) "Keine Angabe" else ""
        )
      }

      # Support a custom run_after
      if (!is.null(run_after)) {
        run_after(session, page, input, ...)
      }
    },
    ...
  )
}

# The default questionnaire pages

#' Welcome Page
#'
#' Providing an introduction and greeting participants.
#'
#' @param title The heading with which to greet participants.
#' @param ... All additional parameters are passed to [new_page()]
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_welcome <- function(title = "Herzlich Willkommen zum Modul zur automatischen Berufskodierung!",
                         ...) {
  new_page(
    page_id = "welcome",
    render = function(...) {
      return(
        list(
          p(strong(title)),
          h5(""),
          button_next(label = "Start"),
          # Move to the top when next or previous button is clicked (this works for all pages, after being called once)
          tags$script('$("body").on("click", "#nextButton, #previousButton", function() {$(window).scrollTop(0) });')
        )
      )
    },
    ...
  )
}

#' The first freetext question to show.
#'
#' Here, the description of the job can be entered in an open freetext field.
#'
#' @param is_interview Should the page show slightly different / additional
#'  instructions and answer options for an interview that is conducted by
#'  another person? Defaults to FALSE.
#' @param ... All additional parameters are passed to [new_page()]
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_first_freetext <- function(is_interview = FALSE, ...) {
  page_freetext(
    page_id = "freetext_1",
    is_interview = is_interview,
    question_text = function(session, page, ...) {
      if (session$userData$session_settings$tense == "past") {
        return("Welche berufliche T\u00e4tigkeit haben Sie in Ihrem letzten Beruf haupts\u00e4chlich ausge\u00fcbt?")
      } else {
        return("Welche berufliche T\u00e4tigkeit \u00fcben Sie derzeit haupts\u00e4chlich aus?")
      }
    },
    previous_button = FALSE,
    render_question_text = FALSE,
    render_before = function(session, page, run_before_output, ...) {
      list(
        p(run_before_output$question_text),
        if (is_interview) p(class = "interviewer", "INT: Angaben des Befragten vollst\u00e4ndig eintragen. Bitte auf Rechtschreibung achten und ggf. buchstabieren lassen.")
      )
    },
    run_after = function(session, page, input, ...) {
      text <- get_question_data(session = session, page_id = page$page_id, key = "response_text")
      session$userData$user_info$text_for_suggestion <- text

      # Generate Job Suggestions
      # (in a configurable fashion)
      job_suggestion_parameters <- list(
        text = text,
        suggestion_type = session$userData$app_settings$suggestion_type,
        num_suggestions = session$userData$session_settings$num_suggestions
      )
      # Merge with parameters from app_settings if provided
      if (!is.null(session$userData$session_settings$get_job_suggestion_params)) {
        utils::modifyList(
          job_suggestion_parameters,
          session$userData$session_settings$get_job_suggestion_params
        )
      }
      automatic_suggestions <- do.call(get_job_suggestions, job_suggestion_parameters)
      session$userData$user_info$list_suggestions <- automatic_suggestions
    },
    ...
  )
}

#' An optional, second free text question if the first didn't yield suggestions.
#'
#' If the first freetext question didn't provide satisfactory results,
#' ask for more details and try again.
#'
#' @param combine_input_with_first Should input be combined with the
#'   previous question?
#' @inheritParams page_first_freetext
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_second_freetext <- function(combine_input_with_first = TRUE, is_interview = FALSE, ...) {
  page_freetext(
    page_id = "freetext_2",
    is_interview = is_interview,
    question_text = "Bitte beschreiben Sie mir diese berufliche T\u00e4tigkeit genau.",
    condition = function(session, page, ...) {
      # Show when there are no suggestions yet
      nrow(stats::na.omit(session$userData$user_info$list_suggestions)) == 0
    },
    run_after = function(session, page, input, ...) {
      text <- get_question_data(
        session = session,
        page_id = page$page_id,
        key = "response_text"
      )
      if (combine_input_with_first) {
        # Combine answer texts from first and second question
        text_from_first_question <- get_question_data(
          session = session,
          page_id = "freetext_1",
          key = "response_text",
          default = ""
        )
        text <- paste(text_from_first_question, text)
      }
      session$userData$user_info$text_for_suggestion <- text

      # Generate Job Suggestions
      # (in a configurable fashion)
      job_suggestion_parameters <- list(
        text = text,
        suggestion_type = session$userData$app_settings$suggestion_type,
        num_suggestions = session$userData$session_settings$num_suggestions
      )
      # Merge with parameters from app_settings if provided
      if (!is.null(session$userData$session_settings$get_job_suggestion_params)) {
        utils::modifyList(
          job_suggestion_parameters,
          session$userData$session_settings$get_job_suggestion_params
        )
      }
      automatic_suggestions <- do.call(get_job_suggestions, job_suggestion_parameters)
      session$userData$user_info$list_suggestions <- automatic_suggestions
    },
    ...
  )
}

#' Display the generated suggestions for the user to pick one.
#'
#' @inheritParams page_first_freetext
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_select_suggestion <- function(is_interview = FALSE, ...) {
  new_page(
    page_id = "select_suggestion",
    condition = function(session, page, ...) {
      return(
        nrow(stats::na.omit(session$userData$user_info$list_suggestions)) > 0 &
          # TODO: Support kldb here as well
          session$userData$app_settings$suggestion_type == "auxco-1.2.x"
      )
    },
    run_before = function(session, page, ...) {
      # Prepare texts to be shown
      if (session$userData$session_settings$tense == "past") {
        if (nrow(session$userData$user_info$list_suggestions) == 1) {
          question_text <- "Trifft die folgende Beschreibung auf Ihre letzte berufliche T\u00e4tigkeit zu oder haben Sie etwas anderes gemacht?" # z.B. bei Eingabe Kr\u00e4utersammler
        } else {
          question_text <- "Welche der folgenden Beschreibungen trifft am ehesten f\u00fcr Ihren letzten Beruf zu? Wenn mehrere Beschreibungen zutreffen, denken Sie bitte an diejenige T\u00e4tigkeit, die Sie haupts\u00e4chlich ausge\u00fcbt haben."
        }
        question_text_other <- "haben Sie etwas anderes gemacht?"
        transition_text <- "Wir versuchen nun, Ihren zuletzt ausge\u00fcbten Beruf genauer einzuordnen."
      } else {
        # alter Text: Wir versuchen nun ihren Beruf f\u00fcr statistische Zwecke genauer einzuordnen. Welche T\u00e4tigkeit f\u00fchren Sie in Ihrem Beruf haupts\u00e4chlich aus? # "Welche dieser T\u00e4tigkeiten \u00fcben Sie derzeit haupts\u00e4chlich aus?"
        if (nrow(session$userData$user_info$list_suggestions) == 1) {
          question_text <- "Trifft die folgende Beschreibung auf Ihre derzeitige berufliche T\u00e4tigkeit zu oder machen Sie etwas anderes?"
        } else {
          question_text <- "Welche der folgenden Beschreibungen trifft am ehesten f\u00fcr Ihren Beruf zu? Wenn mehrere Beschreibungen zutreffen, denken Sie bitte an diejenige T\u00e4tigkeit, die Sie haupts\u00e4chlich aus\u00fcben."
        }
        question_text_other <- "machen Sie etwas anderes?"
        transition_text <- "Wir versuchen nun, Ihren Beruf genauer einzuordnen."
      }

      # default: suggest categories descriptions from auxiliary classification
      if (session$userData$app_settings$suggestion_type == "auxco-1.2.x") {
        df_suggestions <- session$userData$user_info$list_suggestions

        suggestion_main_label_column <- "task"
        # Note: dropdown infromation is currently only shown for interviewers
        suggestion_dropdown_label_column <- "title"
        suggestion_dropdown_content_column <- "task_description"

        # Generate the html of the choices themselves
        if (session$userData$session_settings$extra_instructions == "off") {
          # Don't include extra interviewer information
          style_is_interview <- "display: none" # Interviewerhinweise ausblenden
          platzhalter <- ""

          suggestions_html <- lapply(c(1:nrow(df_suggestions)), function(i) { # access top five entries from df_suggestions
            tags$div(
              tags$div(
                p(tags$b(paste0(i, ". ", df_suggestions[i, suggestion_main_label_column, with = FALSE])))
              )
            )
          })
        } else {
          # Show extra information for the interviewer
          style_is_interview <- ""
          platzhalter <- "z.B. \u00fcbliche Aufgaben und T\u00e4tigkeiten, erforderliche Kenntnisse und Fertigkeiten"

          suggestions_html <- lapply(c(1:nrow(df_suggestions)), function(i) { # access top five entries from df_suggestions
            tags$div(
              tags$div(
                p(tags$b(paste0(i, ". ", df_suggestions[i, suggestion_main_label_column, with = FALSE]))),
                tagAppendAttributes(p(class = "read-on-demand", df_suggestions[i, suggestion_dropdown_label_column, with = FALSE], icon("angle-double-down")), `data-click-toggle` = paste0("toggle-pos-", i))
              ),
              tags$div(id = paste0("toggle-pos-", i), style = "display: none", em(class = "read-on-demand", df_suggestions[i, suggestion_dropdown_content_column, with = FALSE]))
            )
          })
        }


        # append question "Oder machen Sie etwas anderes?" and "*** keine Angabe"
        suggestions_html[[length(suggestions_html) + 1]] <- tags$div(
          p(tags$b(paste0("Oder, ", nrow(df_suggestions) + 1, "., ", question_text_other)))
        )
      }

      # TODO: Encode this somewhere else / don't handle this via suggestion_type
      # alternative: suggest *job titles* from auxiliary classification
      if (session$userData$app_settings$suggestion_type == "aux.labels") {
        df_suggestions <- session$userData$user_info$list_suggestions
        suggestions_html <- lapply(c(1:nrow(df_suggestions)), function(i) { # access top five entries from df_suggestions
          tags$div(
            tagAppendAttributes(p(df_suggestions[i, 4], icon("angle-double-down")), `data-click-toggle` = paste0("toggle-pos-", i)),
            tags$div(id = paste0("toggle-pos-", i), style = "display: none", em(df_suggestions[i, 5]))
          )
        })
        # append question "Oder machen Sie etwas anderes?"
        suggestions_html[[length(suggestions_html) + 1]] <- tags$div(
          p(paste("Oder", question_text_other)),
          textInput("text_none_selected", label = "Bitte beschreiben Sie mir diese T\u00e4tigkeit genau.", placeholder = "z.B. \u00fcbliche Aufgaben und T\u00e4tigkeiten, erforderliche Kenntnisse und Fertigkeiten", width = "800px"),
          br()
        )
      }

      # Add no-answer option
      suggestions_html[[length(suggestions_html) + 1]] <- if (is_interview) {
        tags$div(p(class = "interviewer", "*** Keine Angabe"))
      } else {
        tags$div(p("Keine Angabe"))
      }

      set_question_data(
        session = session,
        page_id = page$page_id,
        question_text = question_text
      )

      return(list(
        transition_text = transition_text,
        style_is_interview = style_is_interview,
        suggestions_html = suggestions_html,
        df_suggestions = df_suggestions
      ))
    },
    render = function(session, page, run_before_output, ...) {
      # Column names used in data.table (for R CMD CHECK)
      auxco_id <- NULL

      list(
        div(
          class = "question",
          div(
            class = if (is_interview) c("secondary", "interviewer") else "secondary",
            div(p("Vorschl\u00e4ge beruhen auf der Eingabe:")),
            div(
              style = "display:inline-block; width:600px",
              textInput(
                "previous-input",
                label = NULL,
                value = session$userData$user_info$text_for_suggestion,
                width = "100%"
              )
            )
          ),
          p(run_before_output$transition_text),
          p(get_question_data(session = session, page_id = page$page_id, key = "question_text")),
          if (is_interview) {
            list(
              p(class = "interviewer", style = run_before_output$style_is_interview, "INT: Gefragt ist diejenige T\u00e4tigkeit, die am meisten Arbeitszeit beansprucht."),
              p(class = "interviewer", style = run_before_output$style_is_interview, "INT: Auslassen von v\u00f6llig unpassenden Vorschl\u00e4gen ist erlaubt.")
            )
          }
        ),
        radioButtons("question1", NULL,
          width = "100%",
          choiceNames = run_before_output$suggestions_html,
          choiceValues = as.list(c(run_before_output$df_suggestions[, auxco_id], "95", "99")),
          selected = get_question_data(session = session, page_id = page$page_id, key = "response_id", default = character(0))
        ),
        br(),
        button_previous(),
        button_next(),
        # # JavaScript Code to immitate button clicking (id = 'nextButton') if a user presses enter in text field (id = 'text_none_selected'), adapted from https://github.com/daattali/advanced-shiny/blob/master/proxy-click/app.R
        # tags$script('var $proxy = $("#nextButton");
        #             $("#text_none_selected").on("keyup", function(e) {
        #             if(e.keyCode == 13){
        #             $proxy.click();
        #             }}); ')
        #
        # JavaScript code to toggle long descriptons ($el refers to the job titles to click on, $proxy to the job descriptions)
        tags$script(
          paste(
            "var $els = $('[data-click-toggle]');",
            "$.each(",
            "  $els,",
            "  function(idx, el) {",
            "    var $el = $(el);",
            "    var $proxy = $('#' + $el.data('clickToggle'));",
            "    $el.on('click', function() {",
            "      // check after completion if the description is shown",
            "      $proxy.slideToggle(300, function() {Shiny.onInputChange('toggleLongDesc', $el.data('clickToggle') + '-' + $proxy.is(':visible'));});",
            "    });",
            "  }",
            ");",
            sep = "\n"
          )
        ),
        br(),
        br()
      )
    },
    run_after = function(session, page, input, ...) {
      shown_suggestions <- rbind(session$userData$user_info$list_suggestions,
        data.frame(id = 95, score = 0, task = paste0("Oder etwas anderes?"), stringsAsFactors = FALSE),
        data.frame(id = 99, score = 0, task = paste0("*** Keine Angabe"), stringsAsFactors = FALSE),
        fill = TRUE
      )

      if (is.null(input$question1)) {
        # Nothing has been selected
        # TODO: We might want to enforce a selection here?
        set_question_data(
          session = session,
          page_id = page$page_id,
          response_id = "EMPTY"
        )
      } else {
        # At least some option has been selected
        set_question_data(
          session = session,
          page_id = page$page_id,
          response_id = input$question1
        )
        if (input$question1 != "95" & input$question1 != "99") {
          # A proper suggestion has been selected
          # TODO: check whether we want to handle this
        }
      }

      # Check for potential clarifying followup questions and add them to the user_data if there are any
      session$userData$followup_questions <- get_followup_questions(
        suggestion_id = get_question_data(session = session, page_id = page$page_id, key = "response_id"),
        tense = session$userData$session_settings$tense
      )

      # save list of suggestions
      shown_suggestions$session_id <- session$userData$user_info$session_id
      shown_suggestions$start <- get_page_data(session = session, page_id = page$page_id, key = "start")
      save_data("occupations_suggested", shown_suggestions, session = session)
    },
    ...
  )
}

#' An additional freetext page to show when no suggestion has been selected.
#'
#' @inheritParams page_first_freetext
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_none_selected_freetext <- function(is_interview = FALSE) {
  page_freetext(
    page_id = "none_selected_freetext",
    question_text = if (is_interview) {
      "Bitte beschreiben Sie mir diese T\u00e4tigkeit genau."
    } else {
      "Bitte beschreiben Sie diese T\u00e4tigkeit genau."
    },
    # Only show this page when none of the suggestions has been picked
    condition = function(session, page, ...) {
      selected_suggestion_id <- get_question_data(session = session, page_id = "select_suggestion", key = "response_id")
      return(selected_suggestion_id %in% c(
        # Nothing ticked at all
        "EMPTY",
        # Picked that they do sth else
        "95",
        # Picked "no response"
        "99"
      ))
    }
  )
}

#' Show potential followup questions to the user.
#'
#' To disambiguate between similar occupations. Depending on the suggestion,
#' multiple followup questions can be shown.
#'
#' @param index The index of the followup question (1-based).
#'   To show the first followup question (if there are any) use
#'   page_followup(index = 1), to show a potential second followup question use
#'   page_followup(index = 2).
#'   For example [questionnaire_web_survey()] uses
#'   `..., page_followup(index = 1), page_followup(index = 2), ...`
#' @inheritParams page_first_freetext
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_followup <- function(index, is_interview = FALSE, ...) { # 1 based because R (sigh)
  new_page(
    page_id = paste0("followup_", index),
    condition = function(session, page, ...) {
      # Column names used in data.table (for R CMD CHECK)
      answer_id <- NULL

      # Is there a question available? If not skip immediately
      if (length(session$userData$followup_questions) < index) {
        return(FALSE)
      }

      # Check whether we can skip this question because one of the previous
      # questions has already been answered
      if (index > 1) {
        # Iterate over previous question indices
        for (previous_index in (index - 1):1) {
          # Retrieve the selected previous answer
          previous_question <- session$userData$followup_questions[[previous_index]]
          previous_answer_id <- get_question_data(
            session = session,
            page_id = paste0("followup_", previous_index),
            key = "response_id"
          )
          previous_answer <- previous_question$answers[answer_id == previous_answer_id]

          # Skip further answers if the previous answer is marked as finished
          if (!is.null(previous_answer) && previous_answer$coding_is_finished) {
            return(FALSE)
          }
        }
      }

      # Is this type of followup question enabled?
      !(session$userData$followup_questions[[index]]$type %in% session$userData$app_settings$skip_followup_types)
    },
    run_before = function(session, page, ...) {
      session$userData$active_followup_question <- session$userData$followup_questions[[index]]
      question <- session$userData$active_followup_question

      set_question_data(
        session = session,
        page_id = page$page_id,
        question_text = paste0(question$question_text, " (", question$id, ")")
      )

      question <- session$userData$active_followup_question
      answer_options_html <- lapply(question$answers$answer_text, function(txt) {
        if (txt %in% c("Ja", "Nein")) {
          tags$div(p(class = "read-on-demand", tags$b(txt)))
        } else {
          tags$div(p(tags$b(txt)))
        }
      })
      answer_options_values <- as.list(question$answers$answer_id)

      if (is_interview) {
        answer_options_html <- append(answer_options_html, list(tags$div(p(class = "interviewer", "*** Verweigert"))))
        answer_options_values <- append(answer_options_values, "97")
        answer_options_html <- append(answer_options_html, list(tags$div(p(class = "interviewer", "*** Wei\u00df nicht"))))
        answer_options_values <- append(answer_options_values, "98")
        answer_options_html <- append(answer_options_html, list(tags$div(p(class = "interviewer", "*** Nicht sinnvoll beantwortbar"))))
        answer_options_values <- append(answer_options_values, "90")
      }

      return(list(
        question = question,
        answer_options_html = answer_options_html,
        answer_options_values = answer_options_values
      ))
    },
    render = function(session, page, run_before_output, ...) {
      list(
        h4(run_before_output$question$question_text),
        br(),
        radioButtons("question.follow.quest", NULL,
          width = "100%",
          choiceNames = run_before_output$answer_options_html,
          choiceValues = run_before_output$answer_options_values,
          selected = character(0)
        ),
        button_previous(),
        button_next()
        # # JavaScript Code to immitate button clicking (id = 'nextButton') if a user presses enter in text field (id = 'text_none_selected'), adapted from https://github.com/daattali/advanced-shiny/blob/master/proxy-click/app.R
        # tags$script('var $proxy = $("#nextButton");
        #           $("#text_none_selected").on("keyup", function(e) {
        #           if(e.keyCode == 13){
        #           $proxy.click();
        #           }});')
      )
    },
    run_after = function(session, page, input, ...) {
      question <- session$userData$active_followup_question

      selected <- if (is.null(input$question.follow.quest)) NA_integer_ else as.integer(input$question.follow.quest) # setze alles auf NA wenn nichts ausgew\u00e4hlt wurde
      selected_suggestion <- question$answers[selected, ]

      set_question_data(
        session = session,
        page_id = page$page_id,
        response_id = selected,
        response_text = selected_suggestion$answer_text
      )

      # Clean up (just for good measure)
      session$userData$active_followup_question <- NULL
    },
    ...
  )
}

#' Page showing the user's results
#'
#' This page is only shown if the query parameter `show_results` is present.
#'
#' This page saves data in results_overview and marks the questionnaire as
#' complete.
#'
#' @param ... All additional parameters are passed to [new_page()]
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_results <- function(...) {
  new_page(
    page_id = "results",
    condition = function(session, page, ...) {
      !is.null(session$userData$user_info$query$show_results)
    },
    run_before = function(session, page, ...) {
      # Column names used in data.table (for R CMD CHECK)
      auxco_id <- NULL

      save_results_overview(session)

      res <- data.frame(
        user_id = session$userData$user_info$id,
        session_id = session$userData$user_info$session_id,
        url_query = session$userData$user_info$url_search
      )

      # 1. Freitextantwort speichern
      res$berufTaetigkeitText <- get_question_data(session = session, page_id = "freetext_1", key = "response_text")
      res$dictionaryCodedTitle <- get_question_data(session = session, page_id = "freetext_1", key = "dictionaryCodedTitle")

      # Save output from 2nd freetext question
      res$berufTaetigkeitText2 <- get_question_data(session = session, page_id = "freetext_2", key = "response_text")

      # Match answers IDs with data
      suggestions <- session$userData$user_info$list_suggestions

      # Check whether ther are suggestions
      has_suggestions <- nrow(stats::na.omit(session$userData$user_info$list_suggestions)) > 0
      if (has_suggestions) {
        selected_suggestion_id <- get_question_data(session = session, page_id = "select_suggestion", key = "response_id")
        selected_suggestion <- suggestions[auxco_id == selected_suggestion_id]
      }

      # And whether one has been picked
      if (nrow(selected_suggestion) > 0) {
        # Auswahl aus Hilfsklassifikation speichern, falls Folgefragen beantwortet wurden, werden KldB und ISCO nachfolgend geupdated
        res$auswahlHilfsklassifikation <- selected_suggestion$auxco_id
        res$auswahlHilfsklassifikationText <- selected_suggestion$task
        res$auxco_id <- selected_suggestion$auxco_id

        # Get raw answers to follow up questions
        res$followUp1Question <- get_question_data(session = session, page_id = "followup_1", key = "question_text", default = NA_character_)
        res$followUp1Answer <- get_question_data(session = session, page_id = "followup_1", key = "response_text", default = NA_character_)
        res$followUp2Question <- get_question_data(session = session, page_id = "followup_2", key = "question_text", default = NA_character_)
        res$followUp2Answer <- get_question_data(session = session, page_id = "followup_2", key = "response_text", default = NA_character_)

        # Retrieve final kldb / isco codes
        followup_1_id <- get_question_data(session = session, page_id = "followup_1", key = "response_id")
        followup_2_id <- get_question_data(session = session, page_id = "followup_2", key = "response_id")

        # Create a named list of followup_answers
        followup_questions <- get_followup_questions(selected_suggestion$auxco_id)
        followup_answers <- list(
          followup_1_id,
          followup_2_id
        )
        names(followup_answers) <- sapply(
          followup_questions,
          function(x) x$question_id
        )

        final_codes <- get_final_codes(
          suggestion_id = selected_suggestion_id,
          followup_answers = followup_answers,
          code_type = c("isco_08", "kldb_10")
        )
        res$kldb <- final_codes$kldb_10
        res$isco <- final_codes$isco_08
      }

      return(list(
        res = res
      ))
    },
    render = function(session, page, run_before_output, ...) {
      res <- run_before_output$res
      kldb_10 <- get_data("kldb-2010")

      list(
        h2("Ergebnis\u00fcberblick"),
        br(),
        h4("Ergebnis erste Freitextfrage:"),
        p(res$berufTaetigkeitText),
        h4("Ergebnis Auswahl Hilfsklassifikation:"),
        p(res$auswahlHilfsklassifikationText),
        h4("Ergebnis zweite Freitextfrage (nur falls der Algorithmus nicht direkt Vorschl\u00e4ge gemacht hat):"),
        p(res$berufTaetigkeitText2),
        h4("Ergebnis erste Folgefrage:"),
        p(res$followUp1Answer),
        h4("Ergebnis zweite Folgefrage:"),
        p(res$followUp2Answer),
        br(),
        h4("Ergebnis Auswahl KldB"),
        renderTable(
          {
            validate(
              need(
                !is.na(res$kldb) | res$kldb == "EMPTY",
                "Wir konnten die von Ihnen eingegebene Berufsbeschreibung nicht mit Kldb abgleichen"
              ),
              need(
                res$auxco_id != "EMPTY" |
                  (is.na(res$followUp1Question) | (!is.na(res$followUp1Question) & !is.na(res$followUp1Answer))) |
                  (is.na(res$followUp2Question) | (!is.na(res$followUp2Question) & !is.na(res$followUp2Answer))),
                "Bitte w\u00e4hlen Sie eine Antwort auf der vorherigen Seite"
              )
            )
            tabKldb <- data.frame(
              Kategorie = c("Kldb Code", "Kldb Titel", "Kldb Inhalt"),
              Ergebnis = c(res$kldb, kldb_10$title[res$kldb == kldb_10$kldb_id], kldb_10$description[res$kldb == kldb_10$kldb_id])
            )
            tabKldb
          },
          striped = TRUE,
          spacing = "l",
          width = "100%",
          align = "l",
          colnames = TRUE
        ),
        h4("Ergebnis Auswahl ISCO"),
        renderTable(
          {
            validate(
              need(
                !is.na(res$kldb) | res$kldb == "EMPTY",
                "Wir konnten die von Ihnen eingegebene Berufsbeschreibung nicht mit ISCO abgleichen"
              ),
              need(
                res$auxco_id != "EMPTY" |
                  (is.na(res$followUp1Question) | (!is.na(res$followUp1Question) & !is.na(res$followUp1Answer))) |
                  (is.na(res$followUp2Question) | (!is.na(res$followUp2Question) & !is.na(res$followUp2Answer))),
                "Bitte w\u00e4hlen Sie eine Antwort auf der vorherigen Seite"
              )
            )
            tabisco <- data.frame(
              Kategorie = c("ISCO Code", "ISCO Titel"),
              Ergebnis = c(res$isco, occupationMeasurement::isco_08_en$label[res$isco == occupationMeasurement::isco_08_en$code])
            )
            tabisco
          },
          striped = TRUE,
          spacing = "l",
          width = "100%",
          align = "l",
          colnames = TRUE
        ),
        button_previous(),
        button_next(),
        mark_questionnaire_complete(),
        ## here
        h4(tags$a("Neustart", href = paste0("/telephone-demo/?followup_types=aufsicht;spezialisierung;sonstige&tense=present&extra_instructions=on&id=0&study_id=Test&show_results")))
      )
    },
    ...
  )
}

#' A final page, showing instructions to close the window.
#'
#' This page saves data in results_overview and marks the questionnaire as
#' complete.
#'
#' @param ... All additional parameters are passed to [new_page()]
#'
#' @return A page object.
#' @seealso [new_page()]
#' @export
page_final <- function(...) {
  new_page(
    page_id = "final",
    run_before = function(session, page, ...) {
      save_results_overview(session)
    },
    render = function(session, page, ...) {
      list(
        p("Befragten automatisch weiterleiten. (Technik muss noch gekl\u00e4rt werden)"),
        h4(class = "interviewer", "Das Browser-Fenster kann nun geschlossen werden."),
        button_previous(),
        mark_questionnaire_complete(),
        actionButton("finalButton", "Fenster Schlie\u00dfen"),
        tags$script('$("#finalButton").click(function(){
          //  if (window.confirm("Das vorherige Fenster ist nicht bekannt. Fenster schlie\u00dfen?")) {
                self.close();
          //  }
          })')
      )
    },
    ...
  )
}

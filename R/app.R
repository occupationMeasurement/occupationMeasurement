options(stringsAsFactors = FALSE) # dont use factors in data.frames

#' @import shiny
#' @import data.table
NULL

# TODO: Double check these before release.
#' Create app_settings.
#'
#' This is the primary and most convenient way of configuring the app.
#'
#' @param require_id Are user_ids required?
#'   Defaults to FALSE
#' @param warn_before_leaving Should users be warned that their progress will
#'   be lost upon leaving the site? Defaults to FALSE.
#' @param skip_followup_types A vector of strings corresponding to the
#'   question_type of followup_question that should be skipped.
#' @param handle_data Callback function to handle data from the app.
#'   This setting takes a function that get's passed 3 parameters:
#'   table_name (A reference name indicating which data to save),
#'   data (A dataframe of data to save), session (the user's current session).
#' @param save_to_file Should responses be saved as files in
#'   response_output_dir? Defaults to use the SAVE_TO_FILE environment variable
#'   or TRUE if it is not set.
#' @param response_output_dir Path to the directory in which to store data
#'   from the app. Defaults to `./output/responses/`.
#' @param verbose Should additional output be printed when running?
#'   Defaults to TRUE.
#' @param .validate Whether the created app_settings should be validated.
#'   Defaults to TRUE.
#'
#' @inheritParams get_job_suggestions
#'
#' @return A list of app_settings.
#' @export
#'
#' @examples
#' app_settings <- create_app_settings(require_id = TRUE)
create_app_settings <- function(suggestion_type = "auxco",
                                require_id = FALSE,
                                warn_before_leaving = FALSE,
                                skip_followup_types = c(),
                                save_to_file = TRUE,
                                response_output_dir = file.path("output", "responses"),
                                handle_data = NULL,
                                verbose = TRUE,
                                .validate = TRUE) {
  final_app_settings <- list(
    suggestion_type = suggestion_type,
    require_id = require_id,
    verbose = verbose,
    warn_before_leaving = warn_before_leaving,
    response_output_dir = response_output_dir,
    handle_data = handle_data,
    save_to_file = save_to_file,
    skip_followup_types = skip_followup_types
  )

  if (.validate) {
    validate_app_settings(final_app_settings)
  }

  return(final_app_settings)
}

validate_app_settings <- function(app_settings) {
  # Validate sugestion_type
  possible_suggestion_types <- c(
    "auxco", "kldb"
  )
  stopifnot(app_settings$suggestion_type %in% possible_suggestion_types)

  # Validate followup types
  possible_followup_types <- c(
    "anforderungsniveau",
    "aufsicht",
    "spezialisierung",
    "sonstige"
  )
  stopifnot(
    sum(app_settings$skip_followup_types %in% possible_followup_types) ==
      length(app_settings$skip_followup_types)
  )

  # Add warnings for odd / dangerous settings
  if (!app_settings$require_id) {
    warning("User Ids are currently not required, this could lead to unmatchable data.")
  }

  if (!app_settings$save_to_file) {
    warning("No data will be saved, as saving to files has been disabled.")
  }
}

validate_questionnaire <- function(questionnaire, verbose) {
  page_ids <- lapply(questionnaire, function(page) page$page_id)
  has_duplicates <- sum(duplicated(page_ids)) > 0

  if (verbose || has_duplicates) {
    cat("Page Ids:", paste(page_ids, collapse = ", "), "\n")
  }

  if (has_duplicates) {
    stop("Duplicated page_ids detected.")
  }
}


#' Start the interactive shiny occupation coding app.
#'
#' @param questionnaire The questionnaire to load.
#'   (Defaults to the questionnaire returned by \link{questionnaire_web_survey}().)
#' @param app_settings The app_settings to use. Check the documentation for
#'   create_app_settings to learn about the options.
#' @param css_file Path to a CSS file to be included in the app.
#' @param resource_dir From which directory to static files e.g. styles.
#'   If you want to load additional resources from outside the package, you
#'   should rather do so with [shiny::addResourcePath] rather than with this
#'   parameter.
#' @param ... Any additional parameters will be forwarded to shiny::shinyApp().
#'
#' @return A shiny app.
#' @seealso questionnaire_web_survey
#' @export
#'
#' @examples
#' \dontrun{
#' app(
#'   app_settings = create_app_settings(
#'     save_to_file = TRUE
#'   )
#' )
#' }
app <- function(questionnaire = questionnaire_web_survey(),
                app_settings = create_app_settings(),
                css_file = NULL,
                resource_dir = system.file("www", package = "occupationMeasurement"),
                ...) {
  require_dependencies()

  validate_questionnaire(questionnaire, verbose = app_settings$verbose)

  shiny::addResourcePath("www", resource_dir)

  ui <- shinyUI(fluidPage(
    tags$head(
      # Load the default / base styles
      tags$link(rel = "stylesheet", type = "text/css", href = "/www/base-styles.css"),

      # Include additional user-provided CSS
      # Note that we can't load this via a HTML tag, as we would have to add
      # the resourcePath, includeCSS provides a nice workaround for this by
      # just inlining the styles from a file
      if (!is.null(css_file)) includeCSS(css_file),

      # Show message when the user tries to leave the page
      if (app_settings$warn_before_leaving) {
        tags$script(
          paste(
            "window.study_completed = false;",
            "window.onbeforeunload = function(e) {",
            "  if(!study_completed) {",
            "    e.preventDefault();",
            "    return 'Your work will be lost.';",
            "  }",
            "};",
            sep = "\n"
          )
        )
      },
    ),
    titlePanel("", windowTitle = "Berufsmodul"),
    uiOutput("MainAction", style = "margin-top:40px;margin-left:auto;margin-right:auto;width:800px")
  ))

  server <- shinyServer(function(input, output, session) {
    session$userData$control <- reactiveValues(current_question = 1, history = 1, log = 1)
    # current_question : position of the current question
    # history : complete history to implement the previous-button, starts with current_question and is updated when current_question changes
    # log : log-file which contains the actual sequence how question have been asked

    output$MainAction <- renderUI({
      if (is.null(session$userData$user_info$session_id)) {
        # run subsequent code only if no session id exists yet, i.e. on the first page (we could run it again on later pages, but this might take a few milliseconds)
        session$userData$user_info$query <- parseQueryString(session$clientData$url_search)
        session$userData$user_info$url_search <- session$clientData$url_search # this will be used to save url_query in the database

        # STOP if no respondent ID available
        if (is.null(session$userData$user_info$query$id)) {
          if (app_settings$require_id) {
            return(list(p(strong(h5("Error: No ID has been supplied. IDs are required for linking data.")))))
          } else {
            session$userData$user_info$id <- NA
          }
        } else {
          session$userData$user_info$id <- session$userData$user_info$query$id
        }

        # create a unique session_id
        session$userData$user_info$session_id <- sprintf("%s_%s_%s", session$userData$user_info$id, as.integer(Sys.time()), sample(0L:9L, 1))

        query_value <- function(name_in_query, default, validate = NULL) {
          value_in_query <- session$userData$user_info$query[[name_in_query]]

          if (is.null(value_in_query)) {
            # Use default value, if nothing is present in query
            return(default)
          } else {
            # Validate query value
            if (!is.null(validate)) {
              if (is.function(validate)) {
                valid <- validate(value_in_query)
              } else {
                valid <- value_in_query %in% validate
              }

              stopifnot(valid)
            }

            return(value_in_query)
          }
        }

        # Create list that will hold questionnaire data
        session$userData$questionnaire_data <- list()

        # Save settings on the app level and settings on the session level
        session$userData$app_settings <- app_settings
        session$userData$session_settings <- list(
          # We don't always want to ask for the current job.
          # If a different, past job is needed the question texts need to change
          tense = query_value(name_in_query = "tense", default = "present", validate = c("present", "past")),
          # Is conventional interviewing turned on?
          # Only in this case are the job titles and the task descriptions shown.
          # (default: on)
          extra_instructions = query_value(name_in_query = "extra_instructions", default = "on", validate = c("on", "off")),
          # Number of response options to be shown (default: 5)
          num_suggestions = as.integer(query_value(name_in_query = "num_suggestions", default = 5, validate = function(val) !is.na(as.integer(val))))
        )
      }


      # STOF if questionnaire is finished
      if (session$userData$control$current_question > length(questionnaire)) {
        return(list(p(strong(h5("Error: Questionnaire finished. ")))))
      }

      #  Dynamische Frames --------------------------------------------------------------------------
      # controlled via the expressions in questionnaire

      # run Code defined in questionnaire
      page <- questionnaire[[session$userData$control$current_question]]
      run_before_output <- execute_run_before(
        page = page,
        session = session
      ) # prev to output
      return(
        execute_render(
          page = page,
          session = session,
          run_before_output = run_before_output
        )
      ) # return output to show
    })


    # ==== React to events on the page ====

    # Go to the next question
    observeEvent(input$nextButton, {
      # evaluate question Content after the question was answered
      execute_run_after(
        page = questionnaire[[session$userData$control$current_question]],
        session = session,
        input = input
      )

      # Determine the next question
      next_question <- session$userData$control$current_question
      repeat({
        next_question <- next_question + 1
        # stop increasing after the last question
        if (next_question > length(questionnaire)) break
        # or if an condition evalutes to TRUE
        if (check_condition(questionnaire[[next_question]], session = session)) break
      })
      # Update current_question if we found the next question
      session$userData$control$current_question <- next_question

      # Add new page to history
      session$userData$control$history <- c(session$userData$control$history, session$userData$control$current_question)
    })

    # Go to the previous question
    observeEvent(input$previousButton, {
      if (session$userData$control$current_question > 1) { # There is no page previous to one.
        session$userData$control$history <- session$userData$control$history[-length(session$userData$control$history)]
        session$userData$control$current_question <- session$userData$control$history[length(session$userData$control$history)]
      }
    })

    # Register when someone expands the description text
    observeEvent(input$toggleLongDesc, {
      # some logging if people click on the job titles to toggle the descriptions
      # session_id: session id
      # toggleMessage: an action send via javaScipt input$toggleLongDesc actions
      # time: timestamp when action was saved

      df <- data.frame(session_id = session$userData$user_info$session_id, toggleMessage = input$toggleLongDesc, time = as.character(Sys.time()))
      save_data("toggle_submitted", df, session)
    })
  })

  shiny_app <- shinyApp(ui = ui, server = server, ...)

  return(shiny_app)
}

#' Run a demo version of the interactive app with additional explanations.
#'
#' This the same as \link{app}, using the \link{questionnaire_demo} instead
#' of the \link{questionnaire_web_survey}.
#'
#' @param ... All paramaters will be passed on to \link{app}
#'
#' @seealso app, questionnaire_demo
#' @return A shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' demo_app()
#' }
demo_app <- function(...) {
  app(questionnaire = questionnaire_demo(), ...)
}

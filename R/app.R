#' @import shiny
#' @import data.table
NULL

#' Validate (and sanitize the questionnaire)
#' @param questionnaire The questionnaire passed to [app()]
#' @param verbose Should information about the questionnaire be printed?
#' @return Sanitized questionnaire
#' @keywords internal
validate_questionnaire <- function(questionnaire, verbose) {
  page_ids <- lapply(questionnaire, function(page) page$page_id)
  has_duplicates <- sum(duplicated(page_ids)) > 0

  if (verbose || has_duplicates) {
    message(paste("Page Ids:", paste(page_ids, collapse = ", ")))
  }

  if (has_duplicates) {
    stop("Duplicated page_ids detected.")
  }

  # Remove any NULLs from the questionnaire
  # These might be introduced from e.g. "if" statements
  questionnaire <- questionnaire[!sapply(questionnaire, is.null)]

  return(questionnaire)
}


#' Get an instance of the interactive shiny occupation coding app.
#'
#' Printing the returned instance or returning it without saving it in a
#' variable will start the app.
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
#' @return A shiny app instance.
#' @seealso `vignette("app")`, [questionnaire_web_survey()]
#' @export
#' @examples
#' app_instance <- app(
#'   app_settings = create_app_settings(
#'     # Important to save results from the app
#'     save_to_file = TRUE
#'   )
#' )
#'
#' # Start the app
#' if (interactive()) {
#'   app_instance
#' }
app <- function(questionnaire = questionnaire_web_survey(),
                app_settings = create_app_settings(save_to_file = TRUE),
                css_file = NULL,
                resource_dir = system.file("www", package = "occupationMeasurement"),
                ...) {
  require_dependencies()

  questionnaire <- validate_questionnaire(questionnaire, verbose = app_settings$verbose)

  shiny::addResourcePath("www", resource_dir)

  ui <- shinyUI(fluidPage(
    tags$head(
      # Load the default / base styles
      tags$link(rel = "stylesheet", type = "text/css", href = "www/base-styles.css"),

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
    # current_question : position of the current question
    # history : complete history to implement the previous-button, starts with current_question and is updated when current_question changes
    session$userData$control <- reactiveValues(current_question = 1, history = 1)

    output$MainAction <- renderUI({
      if (is.null(session$userData$user_info$session_id)) {
        # Save settings on the app level and settings on the session level
        session$userData$app_settings <- app_settings

        # run subsequent code only if no session id exists yet, i.e. on the first page (we could run it again on later pages, but this might take a few milliseconds)
        session$userData$user_info$query <- parseQueryString(session$clientData$url_search)
        session$userData$user_info$url_search <- session$clientData$url_search # this will be used to save url_query in the database

        # STOP if no respondent ID available
        if (is.null(session$userData$user_info$query$respondent_id)) {
          if (app_settings$require_respondent_id) {
            return(list(p(strong(h5("Error: No ID has been supplied. IDs are required for linking data.")))))
          } else {
            session$userData$user_info$respondent_id <- NA
          }
        } else {
          session$userData$user_info$respondent_id <- session$userData$user_info$query$respondent_id
        }

        # create a unique session_id
        session$userData$user_info$session_id <- sprintf("%s_%s_%s", session$userData$user_info$respondent_id, as.integer(Sys.time()), sample(0L:9L, 1))

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

        # Initialize session settings based on query parameters
        # Defaults are set via app_settings
        session$userData$session_settings <- list(
          # We don't always want to ask for the current job.
          # If a different, past job is needed the question texts need to change
          tense = query_value(
            name_in_query = "tense",
            default = app_settings$default_tense,
            validate = c("present", "past")
          ),
          # Is conversational interviewing turned on?
          # Then some additional instructions for the interviwer will be shown.
          extra_instructions = query_value(
            name_in_query = "extra_instructions",
            default = app_settings$default_extra_instructions,
            validate = c("on", "off")
          ),
          # Number of response options to be shown
          num_suggestions = as.integer(query_value(
            name_in_query = "num_suggestions",
            default = app_settings$default_num_suggestions,
            validate = function(val) !is.na(as.integer(val))
          ))
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
      session$userData$current_page_id <- page$page_id
      run_before_output <- execute_run_before(
        page = page,
        session = session,
        input = input,
        output = output
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
        input = input,
        output = output
      )

      # Determine the next question
      next_question <- session$userData$control$current_question
      repeat({
        next_question <- next_question + 1
        next_page <- questionnaire[[next_question]]
        session$userData$current_page_id <- next_page$page_id
        # stop increasing after the last question
        if (next_question > length(questionnaire)) break
        # or if an condition evalutes to TRUE
        if (check_condition(next_page, session = session)) break
      })
      # Update current_question if we found the next question
      session$userData$control$current_question <- next_question

      # Add new page to history
      session$userData$control$history <- c(session$userData$control$history, session$userData$control$current_question)
    })

    # Go to the previous question
    observeEvent(input$previousButton, {
      # There is no page previous to one.
      if (session$userData$control$current_question > 1) {
        # Collect timestamp / finalize data, also when navigating backwards
        leaving_page_backwards(
          page = questionnaire[[session$userData$control$current_question]],
          session = session,
          input = input,
          output = output
        )

        session$userData$control$history <- session$userData$control$history[-length(session$userData$control$history)]
        session$userData$control$current_question <- session$userData$control$history[length(session$userData$control$history)]
      }
    })

    # On Stop will be called when the shiny app is stopped or when each
    # individual user session ends.
    onSessionEnded(function() {
      # Skip if no session has been initialized
      if (is.null(session$userData$user_info$session_id)) {
        return()
      }

      # Save the final, cleaned-up data (commented out for now: not really needed, and it throws errors if one leaves the site early)
      save_results_overview(session)

      # Save session data (to detect multiple sessions if there are issues)
      session_data <- list(
        session_id = session$userData$user_info$session_id,
        url_search = session$userData$user_info$url_search,
        respondent_id = session$userData$user_info$respondent_id,
        history = paste0(isolate(session$userData$control$history), collapse = "-"),
        time_session_ended = as.integer(Sys.time())
      )

      save_data("session_info", session_data, session = session)
    })
  })

  shiny_app <- shinyApp(ui = ui, server = server, ...)

  return(shiny_app)
}

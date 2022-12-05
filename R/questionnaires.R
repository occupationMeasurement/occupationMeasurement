#' A web survey which participants can navigate themselves.
#'
#' The basic default questionnaire. View the function's code to see the
#' used pages. This function is meant as a template that can be changed to meet your requirements.
#'
#' @param show_feedback_page Show the [page_feedback()] to evaluate the fit of
#'   the chosen suggestion.
#' @return A questionnaire for [app()], i.e. a list of pages.
#'
#' @export
#'
#' @examples
#' library(occupationMeasurement)
#'
#' # Inspect the code to create the questionnaire_web_survey
#' print(questionnaire_web_survey)
#'
#' \dontrun{
#' # Run the app with the questionnaire_web_survey
#' app(questionnaire = questionnaire_web_survey())
#'
#' # This is used by default within app
#' app()
#' }
questionnaire_web_survey <- function(show_feedback_page = TRUE) {
  list(
    page_first_freetext(),
    page_second_freetext(),
    page_select_suggestion(),
    page_none_selected_freetext(),
    page_followup(1),
    page_followup(2),
    if (show_feedback_page) page_feedback(),
    page_final()
  )
}


#' A questionnaire for interviewer-administered surveys
#'
#' A questionnaire for Computer-assisted Interviewing (CAI), i.e. telephone interviewing or personal interviewing. In both modes, interviewer asks questions to an interviewee.
#'
#' View the function's code to see the used pages. This function is meant as a template that can be changed to meet your requirements.
#'
#' @inheritParams questionnaire_web_survey
#' @return A questionnaire for [app()] i.e. a list of pages.
#'
#' @export
#'
#' @examples
#' library(occupationMeasurement)
#'
#' # Inspect the code to create the questionnaire_interviewer_administered
#' print(questionnaire_interviewer_administered)
#'
#' \dontrun{
#' # Run the app with the questionnaire_interviewer_administered
#' app(questionnaire = questionnaire_interviewer_administered())
#' }
questionnaire_interviewer_administered <- function(show_feedback_page = TRUE) {
  list(
    page_first_freetext(is_interview = TRUE),
    page_second_freetext(is_interview = TRUE),
    page_select_suggestion(is_interview = TRUE),
    page_none_selected_freetext(is_interview = TRUE),
    page_followup(1, is_interview = TRUE),
    page_followup(2, is_interview = TRUE),
    if (show_feedback_page) page_feedback(is_interview = TRUE),
    page_final()
  )
}

#' A demo questionnaire with additional explanations
#'
#' View the function's code itself to see the used pages.
#'
#' Note, that this function has more complex code to create
#' the additional pages.
#'
#' @inheritParams questionnaire_web_survey
#' @return A questionnaire for [app()] i.e. a list of pages.
#'
#' @export
#'
#' @examples
#' library(occupationMeasurement)
#'
#' # Inspect the code to create the questionnaire_demo
#' print(questionnaire_demo)
#'
#' \dontrun{
#' # Run the app with the questionnaire_demo
#' app(questionnaire = questionnaire_demo())
#' }
#'
questionnaire_demo <- function(show_feedback_page = TRUE) {
  list(
    page_welcome(
      title = "Herzlich Willkommen zum Demo-Modul zur automatischen Berufskodierung!",
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          h3("Hinweise zum Demo-Modul"),
          h4("Zum Fragebogen:"),
          p("Der im folgenden dargestellte Fragebogen entspricht dem questionnaire_interviewer_administered, der f\u00fcr interviewergest\u00fctzte Befragungen entwickelt wurde (z.B. Telefonbefragungen). Dabei wird Text in drei Farb-Codes verwendet:"),
          tags$ul(
            tags$li("Schwarzer Text: Muss zwingend vom Interviewer vorgelesen werden."),
            tags$li("Roter Text: Enth\u00e4lt Intervieweranweisungen. Soll auf keinen Fall vom Interviewer vorgelesen werden."),
            tags$li("Gr\u00fcner Text: Optional. Darf vorgelesen werden, falls dies zur Unterst\u00fctzung des Befragten hilfreich ist.")
          ),
          p("Weitere Erl\u00e4uterungen stehen zu Demonstrationszwecken unterhalb der Buttons."),
          h4("Wem sollen die folgenden Fragen gestellt werden?"),
          p("Die befragte Person ist erwerbst\u00e4tig. Bei Personen ohne Job (Arbeitslose, Sch\u00fcler, Rentner, Hausm\u00e4nner, ...) machen die folgenden Fragen ggf. weniger Sinn. Alle Nicht-Erwerbst\u00e4tigen sollten also bereits vorher herausgefiltert worden sein (prinzipiell lie\u00dfe sich dies \u00e4ndern, aber daf\u00fcr br\u00e4uchten wir Trainingsdaten)."),
          p("Falls Nebenjobs vorhanden sind, muss bereits klargestellt worden sein, dass wir nur die Haupt-Erwerbst\u00e4tigkeit erfassen wollen (oder nachfolgende Formulierungen m\u00fcssen angepasst werden)."),
          h4("Zur Funktionsweise:"),
          p("Nach Eingabe eines Textes k\u00f6nnen die folgenden Aktionen erfolgen:"),
          tags$ol(
            tags$li("Die Suche nach m\u00f6glichen Berufskategorien bleibt erfolglos. Der Befragte wird in einere weiteren Frage nach zus\u00e4tzlichen Details seines Berufs gefragt und die Suche startet erneut."),
            tags$li("Vorschlagen von m\u00f6glichen Berufskategorien mittels statistischer Methoden. Es wird eine halboffene Frage gestellt, aus der der Befragte eine T\u00e4tigkeit ausw\u00e4hlen kann. Als Ergebnis erhalten wir 5-stellige Codes aus der KldB 2010 und 4-stellige ISCO-08 Codes. Ca. 65-85% der Befragten w\u00e4hlen bisherigen Erfahrungen zufolge eine T\u00e4tigkeit aus und k\u00f6nnen somit im Interview kodiert werden."),
            tags$li("Keine automatische Kodierung m\u00f6glich. Eine weitere Freitextfrage ist zur manuellen Kodierung erforderlich.")
          ),
          p("Wenn der Befragte bestimmte T\u00e4tigkeiten bei der halboffenen Frage ausw\u00e4hlt, sind machmal zur Pr\u00e4zisierung f\u00fcr eine genaue Kodierung noch eine oder in seltenen F\u00e4llen auch zwei Folgefragen n\u00f6tig.")
        )
      }
    ),
    page_first_freetext(
      is_interview = TRUE,
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$p("Es gibt mehrere m\u00f6gliche Abl\u00e4ufe / Flows durch das Tool, je nach Antwort des Befragten."),
          shiny::tags$p("Mit der Liste an Beispielantworten unten, k\u00f6nnen Sie die unterschiedlichen Abl\u00e4ufe testen:"),
          shiny::tags$ol(
            shiny::tags$li('Standardbeispiel: "Koch"'),
            shiny::tags$ul(
              shiny::tags$li("Verschiedene Folgefragen werden gestellt, wenn die Antworten 1, 2 oder 4 ausgew\u00e4hlt werden. Auf die Antwort 2 folgt eine Folgefrage zur Aufsichtst\u00e4tigkeit (strategische Entscheidungsbefugnis: Ja/Nein?, eine \u00fcberaus typische Folgefrage)"),
              shiny::tags$li('Wenn die Antwort "etwas anderes" gew\u00e4hlt wird, wird um eine Pr\u00e4zisierung der T\u00e4tigkeit gebeten f\u00fcr eine sp\u00e4tere manuelle Kodierung.'),
            ),
            shiny::tags$li('Beispiel: "Elektrotechniker"'),
            shiny::tags$ul(
              shiny::tags$li("Wenn Antwort 1 ausgew\u00e4hlt wird, werden zwei Folgefragen ben\u00f6tigt (ein seltener Fall). Auf die Antworten 1 und 4 folgen Fragen nach dem Anforderungsniveau (Abgeschlossenes Masterstudium: Ja/Nein?, eine weitere \u00fcberaus typische Folgefrage)."),
            ),
            shiny::tags$li('Beispiel: "Deponiearbeiter"'),
            shiny::tags$ul(
              shiny::tags$li("Antwort 1 ist ein gutes Beispiel daf\u00fcr, welche Antworten bei einer Folgefrage zum Anforderungsniveau in etwa m\u00f6glich sind."),
            ),
            shiny::tags$li('Beispiel: "Schiffslotsin"'),
            shiny::tags$ul(
              shiny::tags$li("Antwort 1 ist ein gutes Beispiel daf\u00fcr, welche Antworten bei einer Folgefrage zur Aufsichtst\u00e4tigkeit m\u00f6glich sind."),
            ),
            shiny::tags$li('Beispiel: "Briefmarkensammler" (Algorithmus findet keine Vorschl\u00e4ge)'),
            shiny::tags$ul(
              shiny::tags$li('Antwort auf erste und zweite Freitext-Frage jeweils "Briefmarkensammler"'),
              shiny::tags$li("Der Algorithmus findet keine Vorschl\u00e4ge und manuelle Kodierung ist n\u00f6tig."),
            ),
            shiny::tags$li('Beispiel: "Backwaren" (Algorithmus findet Vorschl\u00e4ge erst nach Pr\u00e4zisierung)'),
            shiny::tags$ul(
              shiny::tags$li('Anwort auf erste Frage: "Backwaren", Antwort auf zweite Frage: "In der B\u00e4ckerei backen"'),
              shiny::tags$li('Da hier bereits zwei Freitextfragen gestellt wurden, wird bei der Auswahl von "etwas anderes" keine dritte Freitextfrage gestellt.'),
            ),
            shiny::tags$li('Beispiel: "Kr\u00e4utersammler" (Algorithmus findet nur einen einzigen Vorschlag)'),
            shiny::tags$ul(
              shiny::tags$li("Die Frage ist etwas anders formuliert als sonst.")
            )
          )
        )
      }
    ),
    # Use a higher aggregate_score_threshold here, since time is more
    # expensive in telephone surveys and reading options alound much slower
    # than reading them yourself in a web-survey
    page_second_freetext(is_interview = TRUE, aggregate_score_threshold = 0.4),
    page_select_suggestion(
      is_interview = TRUE,
      render_after = function(session, ...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$ul(
            shiny::tags$li("- Auf dieser Seite wurde maschinelles Lernen angewandt um die wahrscheinlichsten Antwortoptionen zu finden,
            damit wir Ihren Job in die Klassifikation der Berufe 2010 (KldB 2010) und in die International Standard Classification of Occupations (ISCO-08) einordnen k\u00f6nnen."),
            shiny::tags$li("- Die f\u00fcnf oben gezeigten Antworten sind die f\u00fcnf wahrscheinlichsten berufsbezogenen Beschreibungen,
            von denen der Machine-Learning-Algorithmus vorhersagt, dass sie Ihren Job passend beschreiben
            und bei der Zuweisung von KldB- und ISCO-Codes helfen w\u00fcrden."),
            shiny::tags$li(
              "Die Liste aller berufsbezogenenen Beschreibungen besteht aus \u00fcber 1100 m\u00f6glichen Antworten und wurde vom
            LMU-Team auf der Grundlage der offiziellen KldB-Klassifikation und unter Nutzung der ISCO-Klassifikation entwickelt.
            Die vollst\u00e4ndige Liste der Antworten (und evtl. weitere Nachfragen) sind",
              tags$a(href = "https://github.com/occupationMeasurement/auxiliary-classification", "in der Berufs-Hilfsklassifikation
            mit T\u00e4tigkeitsbeschreibungen auf Github", target = "_blank"),
              "ersichtlich. Als Sie Ihre berufliche T\u00e4tigkeit auf der vorherigen Seite eingegeben haben, hat der
            Algorithmus auf der Grundlage Ihrer vorherigen Antwort die Wahrscheinlichkeiten
            aller 1100+ Jobbeschreibungen aus unserer Liste berechnet und zeigt Ihnen die 5 wahrscheinlichsten an."
            ),
            shiny::tags$li("- Die Standard-KldB- und ISCO-Codes sind Codes, die vom LMU-Team jeder Antwortoption zugeordnet wurden, weil sie
            besonders plausibel sind. Allerdings sind manchmal weitere Nachfragen n\u00f6tig um die korrekte Kategorien genau zu
            erfassen. Wenn Nachfragen gestellt werden, sind die hier abgebildeten KldB-/ISCO-Codes daher nicht final."),
          ),
          shiny::tags$p("Unten sehen Sie die technischen Details dieser Berechnungen:"),
          renderTable(
            {
              default_suggestion_codes <- get_suggestion_info(
                session$userData$user_info$list_suggestions$auxco_id,
                include_default_codes = TRUE
              )[
                ,
                c("auxco_id", "default_kldb_id", "default_isco_id"),
                with = FALSE
              ]

              suggestions <- session$userData$user_info$list_suggestions |>
                merge(default_suggestion_codes, sort = FALSE, by = "auxco_id")

              tabprobabilities <- data.frame(
                "AuxCo_Id" = suggestions$auxco_id,
                "Wahrscheinlichkeit" = round(suggestions$score, digits = 2),
                "Berufsuntergruppe" = suggestions$title,
                "T\u00e4tigkeit" = suggestions$task,
                "Standard_KldB_Code" = suggestions$default_kldb_id,
                "Standard_ISCO_Code" = suggestions$default_isco_id
              )
              tabprobabilities
            },
            striped = TRUE,
            pagelength = 5,
            spacing = "l",
            width = "100%",
            align = "l",
            colnames = TRUE
          )
        )
      }
    ),
    page_none_selected_freetext(is_interview = TRUE),
    page_followup(1, is_interview = TRUE),
    page_followup(2, is_interview = TRUE),
    if (show_feedback_page) page_feedback(),
    page_results(),
    page_final()
  )
}

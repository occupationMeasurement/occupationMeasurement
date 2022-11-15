#' A Web survey which participants can navigate themselves.
#'
#' The basic default questionnaire. View the function's code to see the
#' used pages. This function is meant as a template that can be changed to meet your requirements.
#'
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
questionnaire_web_survey <- function() {
  list(
    page_welcome(), # welcomes respondents and is typically not shown in a survey
    page_first_freetext(),
    page_second_freetext(),
    page_select_suggestion(),
    page_none_selected_freetext(),
    page_followup(1),
    page_followup(2),
    # page_results(), # summarizes results and is typically not shown in a survey
    page_final()
  )
}


#' A questionnaire for interviewer-administered surveys
#'
#' A questionnaire for Computer-assisted Interviewing (CAI), i.e. telephone interviewing or personal interviewing. In both modes, interviewer asks questions to an interviewee.
#'
#' View the function's code to see the used pages. This function is meant as a template that can be changed to meet your requirements.
#'
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
questionnaire_interviewer_administered <- function() {
  list(
    page_welcome(), # welcomes interviewers and is typically not shown in a survey
    page_first_freetext(is_interview = TRUE),
    page_second_freetext(is_interview = TRUE),
    page_select_suggestion(is_interview = TRUE),
    page_none_selected_freetext(is_interview = TRUE),
    page_followup(1, is_interview = TRUE),
    page_followup(2, is_interview = TRUE),
    # page_results(), # summarizes results and is typically not shown in a survey
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
questionnaire_demo <- function() {
  list(
    page_welcome(
      title = "Herzlich Willkommen zum Demo-Modul zur automatischen Berufskodierung!",
      render_after = function(...) {
        list(
          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
          hr(),
          hr(),
          h3("Hinweise zum Demo-Modul"),
          br(),
          p(strong("Wem sollen die folgenden Fragen gestellt werden?")),
          p("Die befragte Person ist erwerbst\u00e4tig. Bei Personen ohne Job (Arbeitslose, Sch\u00fcler, Rentner, Hausm\u00e4nner, ...) machen die folgenden Fragen ggf. weniger Sinn. Alle Nicht-Erwerbst\u00e4tigen sollten also bereits vorher herausgefiltert worden sein (prinzipiell lie\u00dfe sich dies \u00e4ndern, aber daf\u00fcr br\u00e4uchten wir Trainingsdaten)."),
          p("Falls Nebenjobs vorhanden sind, muss bereits klargestellt worden sein, dass wir nur die Haupt-Erwerbst\u00e4tigkeit erfassen wollen (oder nachfolgende Formulierungen m\u00fcssen angepasst werden)."),
          br(),
          br(),
          p(strong("Zur Funktionsweise:")),
          p("TODO: Das folgende Diagramm und die nachfolgende Beschreibung muss angepasst werden. Der Abgleich mit dem Kodier-Index wird nicht mehr unterstützt und der Fragebogen hier ist allgemein ein wenig anders."),
          p("Nach Eingabe eines Textes k\u00f6nnen die folgenden Aktionen erfolgen:"),
          tags$img(src = "/www/Flowchart.jpg", height = "100%", width = "100%"),
          tags$ol(
            tags$li("Automatische Kodierung mit Kodier-Index (auf KldB-4-Steller-Ebene). Betrifft sch\u00e4tzungsweise ca. 20-30% der Befragten, ist jedoch nicht aktiviert (nur experimentell verf\u00fcgbar)."),
            tags$li("Vorschlagen von m\u00f6glichen Berufskategorien mittels statistischer Methoden. Es wird eine halboffene Frage gestellt, aus der der Befragte eine T\u00e4tigkeit ausw\u00e4hlen kann. Als Ergebnis erhalten wir 5-stellige KldBs und 4-stellige ISCO Codes. Betrifft sch\u00e4tzungsweise ca. 50-60% der Befragten die automatische Kodierung mit Kodier-Index aktiv ist oder 70-90% sonst (default)."),
            tags$li("Keine automatische Kodierung m\u00f6glich. Eine weitere Freitextfrage ist zur manuellen Kodierung erforderlich. Betrifft sch\u00e4tzungsweise ca. 20% der Befragten.")
          ),
          p("Wenn der eingegebene Text identisch mit bestimmten Bezeichnungen aus dem Kodier-Index ist und auch wenn der Befragte bestimmte T\u00e4tigkeiten bei der halboffenen Frage ausw\u00e4hlt, sind machmal zur Pr\u00e4zisierung f\u00fcr eine genaue Kodierung noch eine oder in seltenen F\u00e4llen auch zwei Folgefragen n\u00f6tig.")
        )
      }
    ),
    page_first_freetext(),
    page_second_freetext(),
    page_select_suggestion(
      render_after = function(session, ...) {
        list(
          br(),
          hr(),
          p("- Auf dieser Seite wurde maschinelles Lernen angewandt um die wahrscheinlichsten Antwortoptionen zu finden,
            damit wir Ihren Job in die Klassifikation der Berufe 2010 (KldB 2010) und in die International Standard Classification of Occupations (ISCO-08) einordnen k\u00f6nnen."),
          br(),
          p("- Die f\u00fcnf oben gezeigten Antworten sind die f\u00fcnf wahrscheinlichsten berufsbezogenen Beschreibungen,
            von denen der Machine-Learning-Algorithmus vorhersagt, dass sie Ihren Job passend beschreiben
            und bei der Zuweisung von KldB- und ISCO-Codes helfen w\u00fcrden."),
          br(),
          p(list("- Die Liste aller berufsbezogenenen Beschreibungen besteht aus \u00fcber 1100 m\u00f6glichen Antworten und wurde vom
            LMU-Team auf der Grundlage der offiziellen KldB-Klassifikation und unter Nutzung der ISCO-Klassifikation entwickelt.
            Die vollständige Liste der Antworten (und evtl. weitere Nachfragen) sind",
          tags$a(href="https://github.com/occupationMeasurement/auxiliary-classification", "in der Berufs-Hilfsklassifikation
            mit Tätigkeitsbeschreibungen auf Github", target="_blank"),
            "ersichtlich. Als Sie Ihre berufliche T\u00e4tigkeit auf der vorherigen Seite eingegeben haben, hat der
            Algorithmus auf der Grundlage Ihrer vorherigen Antwort die Wahrscheinlichkeiten
            aller 1100+ Jobbeschreibungen aus unserer Liste berechnet und zeigt Ihnen die 5 wahrscheinlichsten an.")),
          br(),
          p("- Die assoziierten KldB- und ISCO-Codes sind Codes, die vom LMU-Team jeder Antwortoption zugeordnet wurden, weil sie
            besonders plausibel sind. Allerdings sind manchmal weitere Nachfragen n\u00f6tig um die korrekte Kategorien genau zu
            erfassen. Wenn Nachfragen gestellt werden, sind die hier abgebildeten KldB-/ISCO-Codes daher nicht final."),
          br(),
          p("- Unten sehen Sie die technischen Details dieser Berechnungen:"),
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
                "Beruf ID" = suggestions$auxco_id,
                "Wahrscheinlichkeit" = round(suggestions$score, digits = 2),
                "Berufsuntergruppe" = suggestions$title,
                "Taetigkeit" = suggestions$task,
                "Assoziierter Kldb-Code" = suggestions$default_kldb_id,
                "Assoziierter ISCO-Code" = suggestions$default_isco_id
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
    page_none_selected_freetext(),
    page_followup(1),
    page_followup(2),
    page_results(),
    page_final()
  )
}

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
          p("Falls Nebenjobs vorhanden sind, muss bereits klargestellt worden sein, dass wir nur die Haupt-Erwerbst\u00e4tigkeit erfassen wollen (oder nachfolgende Formulierungen m\u00fcssen angepasst werden).")
        )
      }
    ),
    page_first_freetext(
      is_interview = TRUE,
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$p("Es gibt mehrere m\u00f6gliche Abl\u00e4ufe / Flows durch das Tool, je nach Antwort des Befragten."),
          shiny::tags$p("Mit der Liste an Beispielantworten unten k\u00f6nnen Sie die unterschiedlichen Abl\u00e4ufe testen:"),
          shiny::tags$ol(
            shiny::tags$li('Standardbeispiel: "Koch"'),
            shiny::tags$ul(
              shiny::tags$li("Verschiedene Folgefragen werden gestellt, wenn die Antworten 1, 2 oder 4 ausgew\u00e4hlt werden. Auf die Antwort 2 folgt beispielsweise eine Folgefrage zur Aufsichtst\u00e4tigkeit (strategische Entscheidungsbefugnis: Ja/Nein?, eine \u00fcberaus typische Folgefrage)"),
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
            shiny::tags$li('Beispiel: "Briefmarkensammler" und in der anschlie\u00dfenden Freitextfrage erneut "Briefmarkensammler" (Algorithmus findet keine Vorschl\u00e4ge)'),
            shiny::tags$ul(
              shiny::tags$li("Der Algorithmus findet keine Vorschl\u00e4ge und manuelle Kodierung ist n\u00f6tig."),
            ),
            shiny::tags$li('Beispiel: "Backwaren"  und in der anschlie\u00dfenden zweiten Freitextfrage "In der B\u00e4ckerei backen" (Algorithmus findet Vorschl\u00e4ge erst nach Pr\u00e4zisierung)'),
            shiny::tags$ul(
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
    page_second_freetext(is_interview = TRUE, aggregate_score_threshold = 0.4,
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$p("Wenn bei der ersten Freitextfrage eine sehr allgemeine Antwort gegeben wird oder der eingegebene Begriff nicht hinterlegt ist, k\u00f6nnen keine
            spezifischen Antworten vorgeschlagen werden. Es muss zun\u00e4chst nach n\u00e4heren Details gefragt werden."),
            shiny::tags$p('Die verlangte G\u00fcte der Vorschl\u00e4ge kann mithilfe des Parameters "aggregate_score_threshold" kontrolliert
              und auf die Umst\u00e4nde der jeweiligen Befragung angepasst werden. Es gilt eine Abw\u00e4gung zu treffen: Mit einem sehr kleinen threshold
              erhalten fast alle Befragten Vorschl\u00e4ge, aber einige dieser Vorschl\u00e4ge sind sehr schlecht und die Befragten w\u00e4hlen evtl. keine Antwort aus.
              Es wird empfohlen bei der ersten Freitextfrage einen h\u00f6heren Threshold zu w\u00e4hlen (default: 0.535) und ggf. bei der zweiten Freitextfrage
              einen niedrigen Wert zu w\u00e4hlen (hier: 0.4, aber auch Werte nahe 0 k\u00f6nnen sinnvoll sein.).')
          )
      }),
    page_select_suggestion(
      is_interview = TRUE,
      render_after = function(session, ...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$ul(
            shiny::tags$li("Auf dieser Seite wurde maschinelles Lernen angewandt um die wahrscheinlichsten Antwortoptionen zu finden,
            damit wir Ihren Job in die Klassifikation der Berufe 2010 (KldB 2010) und in die International Standard Classification of Occupations (ISCO-08) einordnen k\u00f6nnen."),
            shiny::tags$li("Die f\u00fcnf oben gezeigten Antworten sind die f\u00fcnf wahrscheinlichsten berufsbezogenen Beschreibungen,
            von denen der Machine-Learning-Algorithmus vorhersagt, dass sie Ihren Job passend beschreiben
            und bei der Zuweisung von KldB- und ISCO-Codes helfen w\u00fcrden."),
            shiny::tags$li("Ca. 65-85% der Befragten w\u00e4hlen bisherigen Erfahrungen zufolge eine Beschreibung aus und k\u00f6nnen somit bereits w\u00e4hrend der Befragung kodiert werden."),
            shiny::tags$li(
              "Die Liste aller berufsbezogenenen Beschreibungen besteht aus \u00fcber 1100 m\u00f6glichen Antworten und wurde vom
            LMU-Team auf der Grundlage der offiziellen KldB-Klassifikation und unter Nutzung der ISCO-Klassifikation entwickelt.
            Die vollst\u00e4ndige Liste der Antworten (und evtl. weitere Folgefragen) sind",
              tags$a(href = "https://github.com/occupationMeasurement/auxiliary-classification", "in der Berufs-Hilfsklassifikation
            mit T\u00e4tigkeitsbeschreibungen auf Github", target = "_blank"),
              "ersichtlich. Als Sie Ihre berufliche T\u00e4tigkeit auf der vorherigen Seite eingegeben haben, hat der
            Algorithmus auf der Grundlage Ihrer vorherigen Antwort die Wahrscheinlichkeiten
            aller 1100+ Jobbeschreibungen aus unserer Liste berechnet und zeigt Ihnen die 5 wahrscheinlichsten an."
            ),
            shiny::tags$li("Die Standard-KldB- und ISCO-Codes sind Codes, die vom LMU-Team jeder Antwortoption zugeordnet wurden, weil sie
            besonders plausibel sind. Allerdings sind manchmal weitere Nachfragen n\u00f6tig um die korrekte Kategorien genau zu
            erfassen. Wenn Nachfragen gestellt werden, sind die hier abgebildeten KldB-/ISCO-Codes daher nicht final."),
          ),
          shiny::tags$p("Unten sehen Sie technische Details zu den oben angezeigten Beschreibungen:"),
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
                "Berufsbezeichnung" = suggestions$title,
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
    page_none_selected_freetext(
      is_interview = TRUE,
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$p("Falls der Befragte sich f\u00fcr keine der zuvor angezeigten Beschreibungen entscheiden konnte, ist
            eine manuelle Kodierung n\u00f6tig. Hierf\u00fcr werden hier weitere Details erfasst. Die Seite wird \u00fcbersprungen,
            wenn eine Beschreibung ausgew\u00e4hlt wurde oder wenn bereits zwei Freitextfragen beantwortet wurden.")
          )
      }
    ),
    page_followup(1,
      is_interview = TRUE,
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$p("Die zuvor ausgew\u00e4hlte Beschreibung erlaubt keine pr\u00e4zise Zuordnung in die ISCO-08 bzw. in
          die KldB 2010 auf der h\u00f6chsten Detailstufe."),
          shiny::tags$p("Abh\u00e4ngig von der ausgew\u00e4hlten Beschreibung werden daher maximal zwei Folgefragen gestellt
          um eine genauere Zuordnung vorzunehmen. Vier Arten von m\u00f6glichen Folgefragen k\u00f6nnen unterschieden werden:"),
          shiny::tags$ul(
            shiny::tags$li("F\u00fchrungs- und Aufsichtst\u00e4tigkeiten (ISCO: managerial and supervisory occupations)"),
            shiny::tags$li("Anforderungsniveau (ISCO: skill level)"),
            shiny::tags$li("Berufsspezifische Spezialisierungen"),
            shiny::tags$li("Sonstige")
          ),
          shiny::tags$p("Die beiden erstgenannten Arten (F\u00fchrungs-/Aufsichtst\u00e4tigkeit bzw. Anforderungsniveau)
            werden von vielen Befragungen bereits an anderer Stelle erhoben. Beides kann dabei auf jeweils unterschiedliche Weise
            operationalisiert werden. Wenn solche Informationen bereits anderweitig erfasst werden, ist zu pr\u00fcfen, ob man
            die entsprechende Information hier erneut erheben oder Befragungszeit einsparen m\u00f6chte und die entsprechenden
            Folgefragen bei der Befragung ausl\u00e4sst.")
        )
      }
    ),
    page_followup(2, is_interview = TRUE),
    page_results(),
    if (show_feedback_page) page_feedback(
      is_interview = TRUE,
      render_after = function(...) {
        shiny::tags$div(
          class = "demo-text",
          shiny::tags$p("Es ist schwierig, geeignete Antworten f\u00fcr die Berufs-Hilfsklassifikation zu formulieren. Mithilfe eines klassischen Pretests
            k\u00f6nnen die Antworten auch nicht getestet werden, da viele Berufe extrem selten sind und im Pretest nie vorkommen."),
          shiny::tags$p("Daher wird mit dieser Frage erfasst, welche Antworten die tats\u00e4chlich ausge\u00fcbte T\u00e4tigkeit nur unzureichend beschreiben.
            Wenn bei bestimmten Antworten Probleme festgestellt werden, kann eine \u00dcberarbeitung der Berufs-Hilfsklassifikation erforderlich sein.")
        )
      }
    ),
    page_final()
  )
}

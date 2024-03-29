% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/questionnaire_helpers.R
\name{page_freetext}
\alias{page_freetext}
\title{Show a page with a text field where free text can be entered.}
\usage{
page_freetext(
  page_id,
  question_text = "Please enter your answer in the box below",
  is_interview = FALSE,
  no_answer_checkbox = TRUE,
  next_button = TRUE,
  previous_button = TRUE,
  trigger_next_on_enter = TRUE,
  render_question_text = TRUE,
  run_before = NULL,
  run_after = NULL,
  ...
)
}
\arguments{
\item{page_id}{A unique string identifiying this page. Used to store data.}

\item{question_text}{The question / text to display.
This can be either a string, which will simply be displayed or a function
to dynamically determine the question_text.}

\item{is_interview}{Should the page show slightly different / additional
instructions and answer options for an interview that is conducted by
another person? Defaults to FALSE.}

\item{no_answer_checkbox}{Whether to provide a checkbox to denote that no
answer has been provided.}

\item{next_button}{Whether to show the button to navigate to the next page?
Defaults to TRUE.}

\item{previous_button}{Whether to show the button to navigate to the preivous page?
Defaults to TRUE.}

\item{trigger_next_on_enter}{Whether the next button is triggered
when one presses enter. Defaults to TRUE. There are known issues with IE11.}

\item{render_question_text}{Whether the question text should be displayed?
Only set this to FALSE, if you wish to change the rendering of the
question_text by e.g. using \code{render_before}.
Defaults to TRUE.}

\item{run_before}{Similar to \code{run_before} in \code{new_page()}, passed explicitly
here as this page adds some of its own code to \code{run_before}.}

\item{run_after}{Similar to \code{run_after} in \code{new_page()}, passed explicitly
here as this page adds some of its own code to \code{run_after}.}

\item{...}{Other parametrs are passed on to \code{new_page()}}
}
\value{
A page object.
}
\description{
Show a page with a text field where free text can be entered.
}
\examples{
\dontshow{data.table::setDTthreads(1)}

\dontrun{
page_freetext(
  "test_page_freetext",
  question_text = "Hello there! Please fill in your name below:",
  no_answer_checkbox = TRUE
)
}
}
\seealso{
\code{\link[=new_page]{new_page()}}
}

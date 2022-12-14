% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/questionnaire_pages.R
\name{page_select_suggestion}
\alias{page_select_suggestion}
\title{Display the generated suggestions for the user to pick one.}
\usage{
page_select_suggestion(is_interview = FALSE, ...)
}
\arguments{
\item{is_interview}{Should the page show slightly different / additional
instructions and answer options for an interview that is conducted by
another person? Defaults to FALSE.}

\item{...}{All additional parameters are passed to \code{\link[=new_page]{new_page()}}}
}
\value{
A page object.
}
\description{
Display the generated suggestions for the user to pick one.
}
\seealso{
\code{\link[=new_page]{new_page()}}
}

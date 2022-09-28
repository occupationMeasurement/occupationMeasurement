# occupationMeasurement <img src="man/figures/logo.png" width="120" align="right" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/occupationMeasurement/occupationMeasurement/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/occupationMeasurement/occupationMeasurement/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A package to help with the (interactive) coding and measurement of occupations.

## Installation

```r
remotes::install_github("occupationMeasurement/occupationMeasurement")
```

## Usage

There are three main ways of using this package, with varying degrees of flexibility and convenience:

1. Use the included shiny app which provides all functionality to run your own survey out-of-the-box.
2. Use the included JSON API to connect your own survey or application for maximum flexibility.
3. Use the R functions themselves to implement your own custom functionality.

For a more detailed introduction refer to the `vignette("occupationMeasurement")`.

### 1. Using the interactive Shiny App

To start the interactive shiny app just run the `app()` function.

```r
# Run the interactive shiny app
occupationMeasurement::app()
```

The app also supports custom questionnaires, so you can build your own or use one some of the questionnaires included in the package e.g. the `demo_questionnaire` will explain the functionality of the app using the code below.

```r
library(occupationMeasurement)

# Run the app with additional explanations
app(questionnaire = demo_questionnaire())
```

### 2. Using the JSON API

If you want to include this package in your custom survey-tool or app or if you just need higher flexibility, you can use the included `api()`.

The api server can be started by simply running the code below (the API will open a page with additional documentation when you start it):

```r
# Start the API (and open its documentation)
occupationMeasurement::api()
```

### 3. Using the Exported R Functions

As this is an R package you can also use the functions within it directly. This way you can integrate into your codebase or extend its functionality.

If you want to, for example, generate some suggestions for a certain text input you can do so by just running the code below:

```r
library(occupationMeasurement)

# Generate some job suggestions
get_job_suggestions("Koch", num_suggestions = 3)
```

There are of course many other functions available as well, to check them out, just take a look at the documentation.

## Development

### Testing

Tests can be run with the following snippet:
```r
devtools::test()
```

### Formatting

After making changes to the code, it is advised to automatically format all code with the {styler} package. This can be done by running.

```r
styler::style_pkg()
```

### Building the documentation website
The documentation website is powered by {pkgdown}. It can be rebuilt by running 

```r
pkgdown::build_site()
```

To set up everything for hosting the documentation on github pages use `usethis::use_pkgdown_github_pages()`.
During development, `roxygen2::roxygenise()` is useful to update .Rd-files.

## Acknowledgments

This project is funded by the Deutsche Forschungsgemeinschaft (DFG, German
Research Foundation) – Project number 290773872.

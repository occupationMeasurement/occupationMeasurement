# occupationMeasurement <img src="man/figures/logo.png" align="right" />

R package providing an app and API for interactive coding of occupations.

## Installing the Package
```r
remotes::install_gitlab(
  "000000000135A127/occupationmeasurement-telephone-demo",
  host = "https://gitlab.lrz.de" # ,
  # auth_token = "your_personal_auth_token"
)
```

Note: This requires the environment variable GITLAB_PAT or the auth_token parameter to be set to a GITLAB Access Token with at least the `read_api` scope enabled. Environment variables can be set in `.Renviron`.

## Running the App
```r
library(occupationMeasurement)

app() # Run in production mode

demo_app() # Run with explanations
```

## Testing

Tests can be run with the following snippet:
```r
devtools::test()
```

## Formatting

After making changes to the code, it is advised to automatically format all code with the {styler} package. This can be done by running

```r
styler::style_pkg()
```

## Building the documentation website
The documentation website is powered by {pkgdown}. It can be rebuilt by running 

```r
pkgdown::build_site()
```

To set up everything for hosting the documentation on github pages use `usethis::use_pkgdown_github_pages()`.

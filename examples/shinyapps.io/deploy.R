library(rsconnect)

deployApp(appDir = "examples/shinyapps.io/app_web/", forceUpdate = TRUE)
deployApp(appDir = "examples/shinyapps.io/app_demo/", forceUpdate = TRUE)
deployApp(appDir = "examples/shinyapps.io/app_interviewer/", forceUpdate = TRUE)

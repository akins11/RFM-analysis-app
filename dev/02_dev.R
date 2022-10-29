# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "rfm_description", with_test = TRUE) # Name of the module
golem::add_module(name = "rfm_analysis", with_test = TRUE) # Name of the module
golem::add_module(name = "rfm_analysis_summary", with_test = TRUE)
golem::add_module(name = "segment_assignment", with_test = TRUE)
golem::add_module(name = "segment_summary", with_test = TRUE)
golem::add_module(name = "product_level", with_test = TRUE)
golem::add_module(name = "customer_level", with_test = TRUE)

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("load", with_test = TRUE)
golem::add_utils("load", with_test = TRUE)
golem::add_fct("rfm_description", with_test = TRUE)
golem::add_utils("rfm_description", with_test = TRUE)
golem::add_fct("rfm_analysis", with_test = TRUE)
golem::add_utils("rfm_analysis", with_test = TRUE)
golem::add_fct("analysis_summary", with_test = TRUE)
golem::add_utils("analysis_summary", with_test = TRUE)
golem::add_fct("segment_assignment", with_test = TRUE)
golem::add_utils("segment_assignment", with_test = TRUE)
golem::add_fct("level", with_test = TRUE)
golem::add_utils("level", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "a_retail", open = FALSE)
usethis::use_data_raw(name = "a_rfm_dt", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("rfm")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

usethis::use_package("shiny")
usethis::use_package("shinyWidgets")
usethis::use_package("bs4Dash")
usethis::use_package("shinyjs")
usethis::use_package("reactablefmtr")
# usethis::use_package("waiter")
usethis::use_package("fresh")
usethis::use_package("data.table")
usethis::use_package("reactable")
usethis::use_package("ggplot2")
usethis::use_package("plotly")
usethis::use_package("fontawesome")
usethis::use_package("rlang")
usethis::use_package("scales")
usethis::use_package("janitor")
usethis::use_package("shinycssloaders")

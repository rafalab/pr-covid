# -- Libraries
library(shinythemes)
library(tidyverse)
library(lubridate)
library(leaflet)
library(splines)
library(scales)
library(plotly)
library(tigris)
library(shiny)
library(httr)

# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

# -- Helper functions
logit <- function(x) { log(x / (1-x)) }
expit <- function(x) { 1/ (1 + exp(-x))  }

# -- Loading data
load("rdas/tests.rda")
load("rdas/tests_by_strata.rda")

# -- For confidence intervals
alpha <- 0.01
z <- qnorm(1-alpha/2)

# -- For maps
pr_gjson <- geojsonio::geojson_read("municipalities.geojson", what = "sp")

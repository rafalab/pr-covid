# -- Libraries
library(shinythemes)
library(tidyverse)
library(lubridate)
library(splines)
library(scales)
library(plotly)
# library(Hmisc)
library(shiny)
library(httr)


# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

# -- Helper functions
logit <- function(x) { log(x / (1-x)) }
expit <- function(x) { 1/ (1 + exp(-x))  }

# -- Loading data
load("rdas/tests.rda")
load("rdas/agegroup-tests.rda")
load("rdas/municipio-tests.rda")

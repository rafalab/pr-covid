# -- Libraries
library(shinythemes)
library(tidyverse)
library(lubridate)
library(splines)
library(scales)
library(plotly)
library(shiny)
library(sf)

# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

# -- Helper functions
logit <- function(x) { log(x / (1-x)) }
expit <- function(x) { 1/ (1 + exp(-x))  }

# -- Loading data
load(url("https://github.com/rafalab/pr-covid/raw/master/dashboard/rdas/tests.rda"))
load(url("https://github.com/rafalab/pr-covid/raw/master/dashboard/rdas/tests_by_strata.rda"))
load(url("https://github.com/rafalab/pr-covid/raw/master/dashboard/rdas/hosp_mort.rda"))
load(url("https://github.com/rafalab/pr-covid/raw/master/dashboard/rdas/poblacion_municipios.rda"))
load(url("https://github.com/rafalab/pr-covid/raw/master/dashboard/rdas/mapa.rda"))

# -- For confidence intervals
alpha <- 0.01
z <- qnorm(1-alpha/2)


library(tidyverse)
library(lubridate)
library(scales)
source("functions.R")

lag_to_complete <- 7
last_day <- today() - days(1) - days(lag_to_complete)

## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/rdas"
} else{
  rda_path <- "rdas"
}
# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

## Code to create data.rda is here: https://github.com/rafalab/pr-covid/tree/master/dashboard
load(file.path(rda_path,"data.rda"))





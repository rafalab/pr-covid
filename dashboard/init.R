# -- Libraries
library(tidyverse)
library(gridExtra)
library(lubridate)
library(splines)
library(scales)
library(plotly)
library(shiny)
library(shinythemes)
library(sf)
## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/rdas"
} else{
  rda_path <- "rdas"
}
# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

# -- Helper functions
expit <- function(x) { 1/ (1 + exp(-x))  }

# -- For confidence intervals
alpha <- 0.01
z <- qnorm(1-alpha/2)

# -- Loading data
pop <- read_csv("data/poblacion-municipios.csv") %>%
  slice(1) %>% unlist()
pop <- pop[-1]
names(pop)[names(pop)=="Comerio"]<- "Comerío"
poblacion_municipios <- tibble(patientCity = names(pop), poblacion = pop) %>%
  filter(patientCity != "Puerto Rico")

# -- For maps
map <- st_read("data/pri_adm_2019_shp/pri_admbnda_adm1_2019.shp") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -67.3, xmax = -65.3, ymin = 17.9, ymax = 18.5)
map <- cbind(map, st_coordinates(st_centroid(map)))




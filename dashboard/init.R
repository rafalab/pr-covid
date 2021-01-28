# -- Libraries
library(tidyverse)
library(lubridate)
library(gridExtra)
library(scales)
library(sf)
source("functions.R")

## if on the server get the latest data
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/pr-covid/dashboard/rdas"
} else{
  rda_path <- "rdas"
}
# -- Set locale
Sys.setlocale("LC_TIME", "es_ES")

# -- Loading population data 
pop <- read_csv("data/poblacion-municipios.csv") %>%
  slice(1) %>% unlist()
pop <- pop[-1]
names(pop)[names(pop)=="Comerio"]<- "Comerío"
poblacion_municipios <- tibble(patientCity = names(pop), poblacion = pop) %>%
  filter(patientCity != "Puerto Rico")

#region_pop <- read_csv("data/poblacion-region.csv", skip = 1,
#                       col_names=c("row", "region", "poblacion"))[,-1]

# -- For maps
map <- st_read("data/pri_adm_2019_shp/pri_admbnda_adm1_2019.shp") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -67.3, xmax = -65.3, ymin = 17.9, ymax = 18.5)
map <- cbind(map, st_coordinates(st_centroid(map)))

load(file.path(rda_path,"data.rda"))

lag_to_complete <- 7
last_day <- last_complete_day - days(lag_to_complete)



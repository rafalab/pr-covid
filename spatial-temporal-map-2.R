library(tidyverse)
library(lubridate)
library(sf)

# -- For maps
map <- st_read("dashboard/data/pri_adm_2019_shp/pri_admbnda_adm1_2019.shp") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -67.3, xmax = -65.3, ymin = 17.9, ymax = 18.5)
map <- cbind(map, st_coordinates(st_centroid(map)))

load("dashboard/rdas/data.rda")

MAX <- 0.15 ## maximum positivity rate
MIN <- 0.03
municipio_tests <- 
  tests_by_strata %>%
  filter(date > make_date(2020,3,11)) %>%
  filter(patientCity != "No reportado") %>%
  mutate(patientCity = droplevels(patientCity)) %>%
  group_by(date, patientCity, .drop=FALSE) %>%
  summarize(positives = sum(positives), tests=sum(tests)) %>%
  ungroup() %>%
  group_by(patientCity) %>%
  mutate(rate = zoo::rollsum(positives, k = 14, fill = NA, align = "right") / pmax(1,zoo::rollsum(tests, k = 14, fill = NA, align = "right"))) %>%
  ungroup() %>%
  mutate(rate = pmax(MIN, pmin(MAX, rate))) %>%
  na.omit() %>%
  mutate(rate = 100 * rate) %>% 
  filter(!is.na(rate))

dat <- right_join(map, municipio_tests, by = c("ADM1_ES"="patientCity")) 

dates <- c(unique(dat$date)[seq(1,)], rep(max(dat$date), 7))
library(animation)
saveGIF({
  for(i in seq_along(dates)){
    d <- dates[i]
    p <- dat %>% filter(date == d) %>%
      ggplot() +
      geom_sf(data = map, size=0.15) +
      geom_sf(aes(fill = rate), color="black", size=0.15) +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                           name = "Tasa de positividad basada en pruebas moleculares:",
                           limits= c(3, MAX*100)) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = paste('  Tasa de positividad en Puerto Rico', d,'\n  calculada en periodos de 14 días'))
    print(p)
  }}, movie.name = "ani.gif", ani.height = 480, ani.width = 960, interval = 0.25)
  
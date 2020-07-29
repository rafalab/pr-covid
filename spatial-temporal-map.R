library(tidyverse)
library(lubridate)
library(sf)
library(gganimate)
library(splines)
# -- For maps
map <- st_read("dashboard/data/pri_adm_2019_shp/pri_admbnda_adm1_2019.shp") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -67.3, xmax = -65.3, ymin = 17.9, ymax = 18.5)
map <- cbind(map, st_coordinates(st_centroid(map)))

load("dashboard/rdas/data.rda")

MAX <- 0.15 ## maximum positivity rate

municipio_tests <- 
  tests_by_strata %>%
  filter(date >= make_date(2020,3,15)) %>%
  filter(patientCity != "No reportado") %>%
  mutate(patientCity = droplevels(patientCity)) %>%
  group_by(date, patientCity, .drop=FALSE) %>%
  summarize(positives = sum(positives), tests=sum(tests)) %>%
  ungroup() %>%
  group_by(patientCity) %>%
  mutate(rate = zoo::rollsum(positives, k = 14, fill = NA) / pmax(1,zoo::rollsum(tests, k = 14,fill = NA))) %>%
  ungroup() %>%
  mutate(rate = pmin(MAX, rate)) %>%
  na.omit() %>%
  mutate(rate = 100 * rate) %>% 
  filter(!is.na(rate))

p <- right_join(map, municipio_tests, by = c("ADM1_ES"="patientCity")) %>%
  ggplot() +
  geom_sf(data = map, size=0.15) +
  geom_sf(aes(fill = rate), color="black", size=0.15) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                       name = "Tasa de Positividad:",
                       limits= c(0, MAX*100)) +
  theme_void() +
  theme(legend.position = "bottom") +
  transition_time(date) +
  labs(title = 'Tasa de positividad en Puerto Rico {frame_time}\ncalculada en periodos de 14 días')
    
                
animate(p, end_pause = 5, fps = 3)
anim_save("~/Desktop/map.gif")

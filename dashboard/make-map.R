library(sf)
library(tidyverse)
map <- st_read("data/pri_adm_2019_shp/pri_admbnda_adm1_2019.shp") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -67.3, xmax = -65.3, ymin = 17.9, ymax = 18.5)

map_centers <- data.frame(municipio = map$ADM1_ES, st_coordinates(st_centroid(map)))

map2 <- as(map, "Spatial")
map2 <- map2@polygons
names(map2) <- map$ADM1_ES
map <- map_df(seq_along(map2), function(i){
  tmp <-  map2[[i]]@Polygons

    map_df(seq_along(tmp), function(j){
    tmp2 <-  map2[[i]]@Polygons[[j]]@coords
    data.frame(municipio = names(map2)[i], part = j, X = tmp2[,1], Y = tmp2[,2])
  })
})

save(map, map_centers, file = "data/map.rda")
## tests
# map %>% ggplot(aes(x = X, y = Y, group = municipio)) + 
#   geom_polygon(fill = "white", color = "black") +
#   geom_text(data = map_centers, aes(x = X, y = Y, label=municipio), cex = 2)

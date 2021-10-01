fix_municipio <- function(x, min.dist = 2, 
                          min.score = 1/3, 
                          min.score.barrio = 2/9,
                          first.letter.match = TRUE,
                          return.dist = FALSE){
  
  library(tidyverse)
  library(stringdist)
  
  to_english <-  function(x){
    replace_na(x, "") %>%
      str_trim() %>%
      str_to_lower() %>%
      str_replace_all("á", "a") %>%
      str_replace_all("é", "e") %>%
      str_replace_all("í", "i") %>%
      str_replace_all("ó", "o") %>%
      str_replace_all("ú", "u") %>%
      str_replace_all("ü", "u") %>%
      str_replace_all("ñ", "n") %>%
      str_remove_all("\\s+") 
  }
  
  municipios <- c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "Añasco",
                  "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayamón", "Cabo Rojo", 
                  "Caguas", "Camuy", "Canóvanas", "Carolina", "Cataño", "Cayey", "Ceiba",
                  "Ciales", "Cidra", "Coamo", "Comerío", "Corozal", "Culebra", "Dorado", 
                  "Fajardo", "Florida", "Guánica", "Guayama", "Guayanilla", "Guaynabo", 
                  "Gurabo", "Hatillo", "Hormigueros", "Humacao", "Isabela", "Jayuya", 
                  "Juana Díaz", "Juncos", "Lajas", "Lares", "Las Marías", "Las Piedras", 
                  "Loíza", "Luquillo", "Manatí", "Maricao", "Maunabo", "Mayagüez", "Moca", 
                  "Morovis", "Naguabo", "Naranjito", "Orocovis", "Patillas", "Peñuelas", 
                  "Ponce", "Quebradillas", "Rincón", "Río Grande", "Sabana Grande", "Salinas", 
                  "San Germán", "San Juan", "San Lorenzo", "San Sebastián", "Santa Isabel", 
                  "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", 
                  "Vieques", "Villalba", "Yabucoa", "Yauco")
  
  ## If already correct return
  if(all(x %in% municipios)) return(x)
  
  ## convert to english, lowercase and cleanup
  
  map_mun <- data.frame(id = to_english(municipios), municipio = municipios)
  
  query <- x %>%
    to_english() %>%
    str_remove_all("[^a-zA-Z]+") 
  
  query[query=="puertorico" | query == "unknown" | query == "pr"] <- ""
  zipcode <- x %>% str_match("\\d+")
  
  ## we stro answer in ans
  ans <- as.character(rep(NA, length(x)))
  barrio <- as.character(rep(NA, length(x)))
  
  ind1 <- query %in% map_mun$id
  ans[ind1] <- map_mun$municipio[match(query[ind1], map_mun$id)]
  
  if(all(ind1)) return(ans)
  
  barrios <- read_csv("https://gist.githubusercontent.com/janielMartell/e629fe1448fd312c1830880fca78340d/raw/15e262d9d2e2e3619326d972e9e84f8436ae2e92/barrios.csv", 
                      col_types = cols(MUNICIPIO = col_character(),
                                       BARRIO = col_character())) %>%
    bind_rows(data.frame(
      BARRIO = c("Río Piedras", "Hato Rey", "Barrio Obrero", "Bo Obrero", "Condado", "Miramar", 
                 "Caparra Heights","College Park","Villa Palmeras", 
                 "Fort Buchanan", "Caparra",
                 "Mercedita", 
                 "Saint Just", "Trujillo", "Tru Alto",
                 "Levittown", 
                 "Castaner",
                 "Bajadero", 
                 "Ramey",
                 "Palmer",
                 "Isla Verde", "Urb Country Club",
                 "La Plata",
                 "Villa Rica", "Rio Hondo",
                 "Palmas del Mar",
                 "Cieba",
                 "Campanilla",
                 "Caomo",
                 "Yuaco",
                 "Lioza",
                 "Tao Baja",
                 "Tao Alta",
                 "A Buenas"),
      MUNICIPIO = c("sanjuan","sanjuan","sanjuan","sanjuan","sanjuan", "sanjuan",
                    "sanjuan","sanjuan","sanjuan", 
                    "guaynabo","guaynabo",
                    "ponce", 
                    "trujilloalto", "trujilloalto","trujilloalto",
                    "catano", 
                    "lares",
                    "arecibo",
                    "aguadilla",
                    "riogrande",
                    "carolina", "carolina",
                    "aibonito",
                    "bayamon", "bayamon",
                    "humacao",
                    "ceiba",
                    "toabaja",
                    "coamo",
                    "yauco",
                    "loiza",
                    "toabaja",
                    "toalta",
                    "aguasbuenas"))) %>%
    mutate(original_barrio = BARRIO) %>%
    mutate(BARRIO=to_english(BARRIO), MUNICIPIO = to_english(MUNICIPIO)) %>%
    group_by(BARRIO) %>%
    mutate(n=n()) %>% ##remove duplicates
    filter(n==1) %>%
    select(-n) %>%
    rename(id = MUNICIPIO, barrio = BARRIO) %>%
    left_join(map_mun, by = "id")
  barrios$barrio[barrios$barrio == "barriopueblo(isabelii)"] <- "isabellii"
  barrios$barrio[barrios$barrio == "islademonaeislotemonito"] <- "islademona" 
  
  tmp <- data.frame(barrio = query[!ind1]) %>% left_join(barrios, by = "barrio") 
  
  ans[!ind1] <- tmp$municipio
  barrio[!ind1] <- tmp$original_barrio
  
  
  ind <- which(is.na(ans))
  
  ##check in municpio included
  for(i in 1:nrow(map_mun)){
    ind2 <- str_which(query[ind], map_mun$id[i])
    if(length(ind2)>0) ans[ind[ind2]] <- map_mun$municipio[i]
  }
  
  ind <- which(is.na(ans))
  
  ## distance to municipio
  d_mun <- stringdistmatrix(query[ind], map_mun$id, method = "lv")
  ind_mun <- apply(d_mun, 1, which.min)
  min_d_mun <- apply(d_mun, 1, min) 
  if(first.letter.match){
    min_d_mun[str_sub(query[ind],1,1) != str_sub(map_mun$id[ind_mun],1,1)] <- Inf
  }
  score_mun <- min_d_mun / nchar(query[ind])
  
  #distance to barrio
  d_bar <- stringdistmatrix(query[ind], barrios$barrio, method = "lv")
  ind_bar <- apply(d_bar, 1, which.min)
  min_d_bar <- apply(d_bar, 1, min) 
  if(first.letter.match){
    min_d_bar[str_sub(query[ind], 1, 1) != str_sub(barrios$barrio[ind_bar], 1, 1)] <- Inf
  }
  
  score_bar <- min_d_bar / nchar(query[ind])
  min_d_bar[score_bar  > min.score.barrio] <- Inf
  
  tmp_ans <- ifelse(min_d_bar < min_d_mun, 
                    barrios$municipio[ind_bar], 
                    map_mun$municipio[ind_mun])
  tmp_barrio <- ifelse(min_d_bar < min_d_mun, 
                       barrios$original_barrio[ind_bar], 
                       NA)
  
  min_d <- pmin(min_d_bar, min_d_mun)
  score <- pmin(score_bar, score_mun)
  
  keep <- min_d <= min.dist & score <= min.score 
  ans[ind][keep] <- tmp_ans[keep]
  barrio[ind][keep] <- tmp_barrio[keep]
  
  the_dist <- rep(0, length(ans)); the_dist[ind][keep] <- min_d[keep]
  the_score <- rep(0, length(ans)); the_score[ind][keep] <- score[keep]
  
  
  ind <- which(is.na(ans))
  
  ##check if barrio included
  for(i in 1:nrow(barrios)){
    ind2 <- str_which(query[ind], barrios$barrio[i])
    if(length(ind2)>0){
      ans[ind[ind2]] <- barrios$municipio[i]
      barrio[ind[ind2]] <- barrios$original_barrio[i]
    }
  }
  
  if(return.dist){
    data.frame(original = x,
               municipio = ans, 
               barrio = barrio,
               dist = the_dist,
               score = the_score)
  } else{
    return(ans)
  }  
}

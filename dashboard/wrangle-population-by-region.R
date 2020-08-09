library(tidyverse)
library(lubridate)

# -- Loading data
pop <- read_csv("data/poblacion-municipios.csv") %>%
  slice(1) %>% unlist()
pop <- pop[-1]
names(pop)[names(pop)=="Comerio"]<- "Comerío"
poblacion_municipios <- tibble(patientCity = names(pop), poblacion = pop) %>%
  filter(patientCity != "Puerto Rico")

# -- Population by region
poblacion_region <- poblacion_municipios %>%
  mutate(region = case_when(patientCity %in% c("Isabela", "Aguadilla", "Moca", "San Sebastián",
                                               "Aguada", "Rincón", "Añasco", "Las Marías",
                                               "Mayagüez", "Maricao", "Hormigueros", "San Germán",
                                               "Sabana Grande", "Cabo Rojo", "Lajas") ~ "Mayagüez",
                            patientCity %in% c("Guánica", "Yauco", "Guayanilla", "Peñuelas", "Adjuntas",
                                               "Ponce", "Jayuya", "Juana Díaz", "Villalba", "Coamo",
                                               "Santa Isabel", "Salinas", "Guayama", "Arroyo", "Patillas") ~ "Ponce",
                            patientCity %in% c("Aibonito", "Cayey", "Cidra", "Aguas Buenas", "Caguas", "Gurabo",
                                               "San Lorenzo", "Yabucoa", "Maunabo", "Juncos", "Las Piedras", "Humacao", "Naguabo") ~ "Caguas",
                            patientCity %in% c("Río Grande", "Luquillo", "Fajardo", "Ceiba", "Vieques", "Culebra") ~ "Fajardo",
                            patientCity %in% c("Guaynabo", "San Juan", "Carolina", "Trujillo Alto", "Canóvanas", "Loíza") ~ "Metro",
                            patientCity %in% c("Orocovis", "Barranquitas", "Comerío", "Corozal", "Naranjito",
                                               "Bayamón", "Toa Alta", "Vega Alta", "Dorado", "Toa Baja", "Cataño") ~ "Bayamón",
                            patientCity %in% c("Quebradillas", "Lares", "Camuy", "Hatillo", "Utuado", "Arecibo", 
                                               "Florida", "Barceloneta", "Ciales", "Manatí", "Morovis", "Vega Baja") ~ "Arecibo",
                            TRUE ~ "other")) %>%
  group_by(region) %>%
  summarize(poblacion = sum(poblacion)) %>%
  ungroup()

# -- save
write.csv(poblacion_region, file = "data/poblacion-region.csv")


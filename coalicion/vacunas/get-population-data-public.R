library(tidyverse)
library(readxl)
library(lubridate)
library(tidycensus)

# Get population estimates from ACS19 -------------------------------
pr_pop <- 3285874 ## population of puerto rico

census_api_key("YOUR_KEY_HERE")
v19 <- load_variables(2019, "acs1")
## Find the following names by looking at v19
vars <- c(paste0("B01001_", sprintf("%03d",c(seq(3, 25), seq(27,49)))))
names(vars) <- str_replace(v19$label[match(vars, v19$name)], "Under 5 years", "0 to 4") %>%
  str_replace("85 years and over", "85 to Inf") %>%
  str_replace("21", "21 to 21") %>%
  str_replace("20", "20 to 20") %>%
  str_replace("and", "to") %>%
  str_remove_all("Estimate!!Total:!!|:| years")

census_dat <-  get_acs(geography = "state", 
                       variables = vars,
                       year = 2019,
                       state = "Puerto Rico",
                       survey = "acs1")

correction <- pr_pop/sum(census_dat$estimate) 

pop_by_age_gender <- census_dat %>% 
  separate(variable, c("gender", "ageRange"), sep = "!!") %>%
  separate(ageRange, c("age_start", "age_end"), sep = " to ", convert = TRUE) %>%
  rename(poblacion = estimate) %>%
  mutate(poblacion = round(poblacion*correction)) %>%
  ungroup() %>%
  mutate(gender = ifelse(gender == "Female", "F", "M"))

# Now by municipio

pop_by_municipio  <-  get_estimates(geography = "county", 
                                    year = 2019,
                                    product = "population", 
                                    state = "Puerto Rico") %>%
  filter(variable == "POP") %>%
  mutate(NAME = str_remove(NAME, " Municipio, Puerto Rico")) %>%
  select(NAME, value) %>%
  setNames(c("municipio", "poblacion")) %>%
  mutate(poblacion = round(poblacion*correction))

## Now demographics by municipio
census_dat_by_municipio <- read_xlsx("data/EstimadosPoblacionales2019_Wilmari.xlsx", sheet = 2)

# From here: https://www.census.gov/data/tables/time-series/demo/popest/2010s-detail-puerto-rico-municipios.html
# note for future: look into why it does not show up with get_estimate
pop_by_municipio_age_gender <- census_dat_by_municipio %>%
  select(-FIPS, -TOTAL) %>%
  pivot_longer(cols = c("MALE", "FEMALE"), names_to = "gender", values_to = "poblacion") %>%
  rename(ageRange = AGEP, municipio = MUN) %>%
  mutate(ageRange = ifelse(ageRange == "Under 5 years", "0 to 4", ageRange)) %>%
  mutate(ageRange = ifelse(ageRange == "85 years and over", "85 to Inf", ageRange)) %>%
  mutate(ageRange = str_remove(ageRange, "years")) %>%
  separate(ageRange, c("age_start", "age_end"), sep = " to ", convert = TRUE) %>%
  mutate(gender = ifelse(gender == "MALE", "M", "F")) %>%
  mutate(municipio = ifelse(municipio=="San Sebastian", "San Sebastián", municipio)) %>% 
  group_by(municipio) %>%
  mutate(prop = poblacion/sum(poblacion)) %>%
  ungroup() %>%
  select(municipio, age_start, age_end, gender, prop)

save(pr_pop, pop_by_municipio, pop_by_age_gender, pop_by_municipio_age_gender, file = "rdas/population-tabs.rda")


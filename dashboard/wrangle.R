# -- Libraries
library(tidyverse)
library(lubridate)
library(splines)
source("functions.R")

# -- Fixed values
icu_beds <- 229 #if available beds is missing change to this

first_day <- make_date(2020, 3, 12)

last_day <- today() - days(6)

age_levels <-  c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", 
                 "70 to 79", "80 to 89", "90 to 99", "100 to 109", "110 to 119", "120 to 129")

test_url <- "https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"

cases_url <- "https://bioportal.salud.gov.pr/api/administration/reports/orders/minimal-info"

alpha <- 0.05

# Reading and wrangling test data from database ----------------------------------------------

all_tests <- jsonlite::fromJSON(test_url)

all_tests <- all_tests %>%  
  rename(patientCity = city) %>%
  as_tibble() %>%
  mutate(collectedDate  = mdy(collectedDate),
         reportedDate   = mdy(reportedDate),
         createdAt      = mdy_hm(createdAt),
         ageRange       = na_if(ageRange, "N/A"),
         ageRange       = factor(ageRange, levels = age_levels),
         patientCity    = ifelse(patientCity == "Loiza", "Loíza", patientCity),
         patientCity    = ifelse(patientCity == "Rio Grande", "Río Grande", patientCity),
         patientCity    = factor(patientCity),
         result         = tolower(result),
         result         = case_when(grepl("positive", result) ~ "positive",
                                    grepl("negative", result) ~ "negative",
                                    result == "not detected" ~ "negative",
                                    TRUE ~ "other")) %>%
  arrange(reportedDate, collectedDate) 

## fixing bad dates: if you want to remove bad dates instead, change FALSE TO TRUE
if(FALSE){
  ## remove bad dates
  all_tests <- all_tests %>% 
  filter(!is.na(collectedDate) & year(collectedDate) == 2020 & collectedDate <= today()) %>%
  mutate(date = collectedDate) 
} else{
  ## Impute missing dates and remove inconsistent dates
  all_tests <- all_tests %>% 
    mutate(date = if_else(is.na(collectedDate), reportedDate - days(2),  collectedDate)) %>%
    mutate(date = if_else(year(date) != 2020 | date > today(), reportedDate - days(2),  date)) %>%
    filter(year(date) == 2020 & date <= today()) %>%
    arrange(date, reportedDate)
}


# Reading and wrangling cases data from database ---------------------------

all_tests_with_id <- jsonlite::fromJSON(cases_url)

all_tests_with_id <- all_tests_with_id %>%  
  as_tibble() %>%
  mutate(collectedDate  = mdy(collectedDate),
         reportedDate   = mdy(reportedDate),
         createdAt      = mdy_hm(createdAt),
         ageRange       = na_if(ageRange, "N/A"),
         ageRange       = factor(ageRange, levels = age_levels),
         region         = ifelse(region == "Bayamon", "Bayamón", region),
         region         = ifelse(region == "Mayaguez", "Mayagüez", region),
         region         = factor(region),
         result         = tolower(result),
         result         = case_when(grepl("positive", result) ~ "positive",
                                    grepl("negative", result) ~ "negative",
                                    result == "not detected" ~ "negative",
                                    TRUE ~ "other")) %>%
  arrange(reportedDate, collectedDate, patientId) 

## fixing bad dates: if you want to remove bad dates instead, change FALSE TO TRUE
if(FALSE){
  ## remove bad dates
  all_tests_with_id <- all_tests_with_id %>% 
    filter(!is.na(collectedDate) & year(collectedDate) == 2020 & collectedDate <= today()) %>%
    mutate(date = collectedDate) 
} else{
  ## Impute missing dates
  all_tests_with_id <- all_tests_with_id %>% mutate(date = if_else(is.na(collectedDate), reportedDate - days(2),  collectedDate)) %>%
    mutate(date = if_else(year(date) != 2020 | date > today(), reportedDate - days(2),  date)) %>%
    filter(year(date) == 2020 & date <= today()) %>%
    arrange(date, reportedDate)
}

# -- Computing observed positivity rate
tests <- all_tests_with_id %>%
  filter(date >= first_day & testType %in% c("Molecular", "Serological") & 
           result %in% c("positive", "negative")) %>%
  group_by(testType, date) %>%
  summarize(positives = n_distinct(patientId[result == "positive"]),
            tests = n_distinct(patientId),
            all_positives = sum(result == "positive"),
            all_tests = n(),
            .groups = "drop") %>%
  mutate(rate = positives / tests,
         old_rate = all_positives / all_tests)


positivity <- function(dat){
  day_seq <- seq(first_day + weeks(1), max(dat$date), by = "day")
  map_df(day_seq, function(the_day){
  dat %>% filter(date >= the_day - weeks(1) & date <= the_day) %>%
    summarize(date = the_day, 
              positives = n_distinct(patientId[result == "positive"]),
              tests = n_distinct(patientId),
              fit = positives / tests,
              lower = qbinom(0.025, tests, fit)/tests,
              upper = qbinom(0.975, tests, fit)/tests) %>%
      select(date, fit, lower, upper)
  })
}

fits <- all_tests_with_id %>% 
  filter(date >= first_day & testType %in% c("Molecular", "Serological") & 
           result %in% c("positive", "negative")) %>%
  nest_by(testType) %>%
  summarize(positivity(data), .groups = "drop")
  
tests <- left_join(tests, fits, by = c("testType", "date"))


if(FALSE){
  plot_positivity(tests, first_day, today()) +
    geom_smooth(method = "loess", formula = "y~x", span = 0.2, method.args = list(degree = 1, weight = tests$tests), color = "red", lty =2, fill = "pink") 
}

tests$tests_week_avg <- with(tests, ma7(date, all_tests))$moving_avg

if(FALSE){
  plot_test(tests, first_day, today())
  plot_test(tests, first_day, today(), type  = "Serological")
}

# unique cases ------------------------------------------------------------
all_cases <- all_tests_with_id %>%      
  filter(date>=first_day & result == "positive" &
           testType %in% c("Molecular", "Serological")) %>%
  group_by(testType, patientId) %>%
  mutate(n=n()) %>%
  arrange(date) %>%
  slice(1) %>% 
  ungroup() %>%
  mutate(region = fct_explicit_na(region, "No reportado")) %>%
  mutate(ageRange = fct_explicit_na(ageRange, "No reportado")) %>%
  select(-patientId, -result) %>%
  arrange(testType, date)

  
# Add cases to tests data frame -------------------------------------------
cases <- all_cases %>%
  group_by(testType, date) %>% 
  summarize(cases = n(), .groups = "drop")

## Make sure all dates are included
cases <-  left_join(select(tests, testType, date), cases, by = c("testType","date")) %>%
  replace_na(list(cases = 0))

fits <- cases %>% 
  group_by(testType) %>%
  do(ma7(d = .$date, y = .$cases))

cases <- left_join(cases, fits, by = c("testType", "date"))

if(FALSE){
  plot_cases(cases)
}

# -- summaries stratified by age group and patientID
tests_by_strata <- all_tests %>%  
  filter(date>=first_day) %>%
  filter(result %in% c("positive", "negative")) %>%
  mutate(patientCity = fct_explicit_na(patientCity, "No reportado")) %>%
  mutate(ageRange = fct_explicit_na(ageRange, "No reportado")) %>%
  group_by(testType, date, patientCity, ageRange, .drop = FALSE) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n(), .groups="drop") %>%
  ungroup()

# --Mortality and hospitlization
hosp_mort <- read_csv("https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/DatosMortalidad.csv") %>%
  mutate(date = mdy(Fecha)) %>% 
  filter(date >= first_day) 

## we started keeping track of available beds on 2020-09-20 
hosp_mort <- hosp_mort %>% 
  replace_na(list(CamasICU_disp = icu_beds))

# -- model to deaths. Here there is no weekend effect
fits <- with(hosp_mort, 
             ma7(d = date, y = IncMueSalud))
hosp_mort$fit <- fits$moving_avg


# Compute time it takes tests to come in ----------------------------------

rezago <- all_tests_with_id  %>% 
  filter(result %in% c("positive", "negative")) %>%
  group_by(testType) %>%
  mutate(diff =  as.numeric(as_date(createdAt) - collectedDate), 
         Resultado = factor(result, labels = c("Negativos", "Positivos"))) %>%
  ungroup %>%
  select(testType, date, Resultado, diff) %>%
  filter(!is.na(diff))

if(FALSE){
 plot_deaths(hosp_mort)
}
# -- Save data

## if on server, save with full path
## if not on server, save to home directory
#if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
if(FALSE){
  rda_path <- "/homes10/rafa/dashboard/rdas"
} else{
  rda_path <- "rdas"
}

## define date and time of latest download
the_stamp <- now()
save(first_day, last_day, alpha, the_stamp, 
     tests, tests_by_strata, cases,
     hosp_mort, rezago,
     file = file.path(rda_path, "data.rda"))

## Because API added an ID to one of the tables, we will remove download option
## saveRDS(all_tests, file = file.path(rda_path, "all_tests.rds"), compress = "xz")


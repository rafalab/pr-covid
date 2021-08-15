# -- Libraries   
library(tidyverse)
library(lubridate)
library(splines)

# fit glm spline ----------------------------------------------------------
# no longer used. we now use moving average to match other dashboards
# spline_fit <- function(d, y, n = NULL, 
#                        week_effect = TRUE, 
#                        knots_per_month = 2, 
#                        family = quasibinomial, 
#                        alpha = 0.05){
#   
#   z <- qnorm(1 - alpha/2)
#   
#   x <- as.numeric(d)
#   
#   df  <- round(knots_per_month * length(x) / 30) + 1
#   
#   if(family()$family %in% c("binomial", "quasibinomial")){
#     if(is.null(n)) stop("Must supply n with binomial or quasibinomial")
#     y <- cbind(y, n-y)
#   }
#   
#   if(week_effect){
#     
#     w <- factor(wday(d))
#     contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
#     w <- model.matrix(~w)[,-1]
#     
#     glm_fit  <- glm(y ~ ns(x, df = df, intercept = TRUE) + w - 1, family = family)
#     
#   } else {
#     
#     glm_fit  <- glm(y ~ ns(x, df = df, intercept = TRUE) - 1, family = family)
#     
#   }
#   
#   glm_pred <- predict(glm_fit, type = "terms", se.fit = TRUE)
#   
#   fit <- family()$linkinv(glm_pred$fit[,1])
#   
#   lower <- family()$linkinv(glm_pred$fit[,1] - z * glm_pred$se.fit[,1])
#   
#   upper <- family()$linkinv(glm_pred$fit[,1] + z * glm_pred$se.fit[,1])
#   
#   return(tibble(date = d, fit = fit, lower = lower, upper = upper))  
# }

# moving average ----------------------------------------------------------

ma7 <- function(d, y, k = 7) 
  tibble(date = d, moving_avg = as.numeric(stats::filter(y, rep(1/k, k), side = 1)))

sum7 <- function(d, y, k = 7) 
  tibble(date = d, moving_sum = as.numeric(stats::filter(y, rep(1, k), side = 1)))

# -- Fixed values
pr_pop <- 3285874 ## population of puerto rico

icu_beds <- 229 #if available beds is missing change to this

first_day <- make_date(2020, 3, 12)

last_complete_day <- today() - 1

the_years <- seq(2020, year(today()))

age_levels <-  c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", 
                 "70 to 79", "80 to 89", "90 to 99", "100 to 109", "110 to 119", "120 to 129")

imputation_delay  <- 2

alpha <- 0.05

test_url <- "https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"

cases_url <- "https://bioportal.salud.gov.pr/api/administration/reports/orders/basic"

get_bioportal <- function(url){
  jsonlite::fromJSON(
    rawToChar(
      httr::GET(url, httr::content_type('application/json'), 
                        httr::add_headers('Accept-Enconding'="br"))$content)
  )
}

#test_types <- c("Molecular", "Serological", "Antigens", "Molecular+Antigens")
#original_test_types <- c("Molecular", "Serological", "Antigens")
test_types <- c("Molecular", "Antigens", "Molecular+Antigens")
original_test_types <- c("Molecular", "Antigens")

# Reading and wrangling test data from database ----------------------------------------------
message("Reading test data.")

all_tests <- get_bioportal(test_url)

message("Processing test data.")

all_tests <- all_tests %>%  
  rename(patientCity = city) %>%
  as_tibble() %>%
  mutate(testType = str_to_title(testType),
         testType = ifelse(testType == "Antigeno", "Antigens", testType),
         collectedDate  = mdy(collectedDate),
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
  filter(!is.na(collectedDate) & year(collectedDate) %in% the_years & collectedDate <= today()) %>%
  mutate(date = collectedDate) 
} else{
  ## Impute missing dates and remove inconsistent dates
  all_tests <- all_tests %>% 
    mutate(date = if_else(collectedDate > reportedDate, reportedDate, collectedDate)) %>% ## if collectedDate is in the future make it reportedDate
    mutate(date = if_else(is.na(collectedDate), reportedDate - days(imputation_delay),  collectedDate)) %>%
    mutate(date = if_else(!year(date) %in% the_years, reportedDate - days(imputation_delay),  date)) %>%
    filter(year(date) %in% the_years & date <= today()) %>%
    arrange(date, reportedDate)
}


# Reading and wrangling cases data from database ---------------------------
age_levels <-  paste(seq(0, 125, 5), "to", seq(4, 129, 5))

message("Reading case data.")

all_tests_with_id <- get_bioportal(cases_url)

message("Processing case data.")

all_tests_with_id <- all_tests_with_id %>%  
  as_tibble() %>%
  mutate(testType = str_to_title(testType),
         testType = ifelse(testType == "Antigeno", "Antigens", testType),
         collectedDate = ymd_hms(collectedDate, tz = "America/Puerto_Rico"),
         reportedDate = ymd_hms(reportedDate, tz = "America/Puerto_Rico"),
         orderCreatedAt = ymd_hms(orderCreatedAt, tz = "America/Puerto_Rico"),
         resultCreatedAt = ymd_hms(resultCreatedAt, tz = "America/Puerto_Rico"),
         ageRange       = na_if(ageRange, "N/A"),
         ageRange       = factor(ageRange, levels = age_levels),
         region = na_if(region, "N/A"),
         region = ifelse(region == "Bayamon", "Bayamón", region),
         region = ifelse(region == "Mayaguez", "Mayagüez", region),
         region = replace_na(region, "No reportada"),
         region = factor(region),
         result = tolower(result),
         result = case_when(grepl("positive", result) ~ "positive",
                            grepl("negative", result) ~ "negative",
                            result == "not detected" ~ "negative",
                            TRUE ~ "other")) %>%
  arrange(reportedDate, collectedDate, patientId) 

## fixing bad dates: if you want to remove bad dates instead, change FALSE TO TRUE
if(FALSE){
  ## remove bad dates
  all_tests_with_id <- all_tests_with_id %>% 
    filter(!is.na(collectedDate) & year(collectedDate) %in% the_years & collectedDate <= today()) %>%
    mutate(date = as_date(collectedDate))
} else{
  ## Impute missing dates
  all_tests_with_id <- all_tests_with_id %>% 
    mutate(date = if_else(collectedDate > reportedDate, reportedDate, collectedDate)) %>% ## if collectedDate is in the future make it reportedDate
    mutate(date = if_else(is.na(collectedDate), reportedDate - days(imputation_delay),  collectedDate)) %>%
    mutate(date = if_else(!year(date) %in% the_years, reportedDate - days(imputation_delay),  date)) %>%  
    mutate(date = as_date(date)) %>%
    filter(year(date) %in% the_years & date <= today()) %>%
    arrange(date, reportedDate)
}

# -- Computing observed positivity rate
## adding a new test type that combines molecular and antigens
mol_anti <- all_tests_with_id %>%
  filter(date >= first_day & testType %in% c("Molecular", "Antigens") & 
           result %in% c("positive", "negative")) %>%
  mutate(testType = "Molecular+Antigens") 


## compute daily totals
tests <- all_tests_with_id %>%
  bind_rows(mol_anti) %>%
  filter(date >= first_day & 
           testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  group_by(testType, date) %>%
  summarize(people_positives = n_distinct(patientId[result == "positive"]),
            people_total = n_distinct(patientId),
            tests_positives = sum(result == "positive"),
            tests_total = n(),
            .groups = "drop") %>%
  mutate(rate = people_positives / people_total)

## define function to compute weekly distinct cases
## and use this to compute percent of people with positive tests

positivity <- function(dat){
  day_seq <- seq(first_day + weeks(1), max(dat$date), by = "day")
  map_df(day_seq, function(the_day){
    dat %>% filter(date > the_day - weeks(1) & date <= the_day) %>%
      mutate(obs = entry_date <= the_day) %>%
      summarize(date = the_day, 
                people_positives_week = n_distinct(patientId[result == "positive"]),
                people_total_week = n_distinct(patientId),
                fit = people_positives_week / people_total_week,
                lower = qbinom(0.025, people_total_week, fit) / people_total_week,
                upper = qbinom(0.975, people_total_week, fit) / people_total_week,
                obs_people_positives_week = n_distinct(patientId[result == "positive" & obs]),
                obs_people_total_week = n_distinct(patientId[obs]),
                obs_fit = obs_people_positives_week / obs_people_total_week,
                obs_lower = qbinom(0.025, obs_people_total_week, obs_fit) / obs_people_total_week,
                obs_upper = qbinom(0.975, obs_people_total_week, obs_fit) / obs_people_total_week) %>%
      select(date, fit, lower, upper, obs_fit, obs_lower, obs_upper, people_positives_week, people_total_week) 
  })
}

message("Computing positivity.")

## run the function on each test type
fits <- all_tests_with_id %>% 
  bind_rows(mol_anti) %>%
  mutate(entry_date = as_date(orderCreatedAt)) %>%
  filter(date >= first_day & testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  nest_by(testType) %>%
  summarize(positivity(data), .groups = "drop")
  
## add new variable to test data frame
tests <- left_join(tests, fits, by = c("testType", "date")) 

## compute weekly totals for positive tests and total tests
tests <- tests %>% 
  group_by(testType) %>%
  mutate(tests_positives_week = sum7(d = date, y = tests_positives)$moving_sum) %>%
  mutate(tests_total_week = sum7(d = date, y = tests_total)$moving_sum) %>%
  ungroup()

# compute unique cases ------------------------------------------------------------
all_cases <- all_tests_with_id %>%  
  bind_rows(mol_anti) %>%
  filter(date>=first_day & result == "positive" &
           testType %in% test_types) %>%
  group_by(testType, patientId) %>%
  mutate(n=n()) %>%
  arrange(date) %>%
  slice(1) %>% 
  ungroup() %>%
  select(-patientId, -result) %>%
  arrange(testType, date)

# compute daily new cases
cases <- all_cases %>%
  group_by(testType, date) %>% 
  summarize(cases = n(), .groups = "drop") 

# Make sure all dates are included
cases <-  left_join(select(tests, testType, date), cases, by = c("testType", "date")) %>%
  replace_na(list(cases = 0)) 

# compute daily weekly average and add to cases data frame
fits <- cases %>% 
  group_by(testType) %>%
  do(ma7(d = .$date, y = .$cases)) %>%
  rename(cases_week_avg = moving_avg)
cases <- left_join(cases, fits, by = c("testType", "date"))

## add new cases and weekly average to tests data frame
tests <- left_join(tests, cases, by = c("testType", "date")) %>%
  mutate(cases_plus_negatives = (people_total_week - people_positives_week + cases_week_avg * 7),
         cases_rate = cases_week_avg * 7 / cases_plus_negatives,
         cases_plus_negatives_daily = people_total - people_positives + cases,
         cases_rate_daily = cases / cases_plus_negatives_daily)
         
## Compute unique negatives

# compute unique negative cases ------------------------------------------------------------
message("Computing unique negatives.")

negative_cases <- all_tests_with_id %>%  
  bind_rows(mol_anti) %>%
  filter(date>=first_day & result == "negative" &
           testType %in% test_types) %>%
  group_by(testType, patientId) %>%
  arrange(date) %>%
  slice(1) %>% 
  ungroup() %>%
  select(-patientId, -result) %>%
  arrange(testType, date) %>%
  group_by(testType, date) %>% 
  summarize(negative_cases = n(), .groups = "drop")

# Make sure all dates are included
negative_cases <-  select(tests, testType, date) %>% 
  left_join(negative_cases, by = c("testType", "date")) %>%
  replace_na(list(negative_cases = 0))

# compute daily weekly average and add to negative_cases data frame
fits <- negative_cases %>% 
  group_by(testType) %>%
  do(ma7(d = .$date, y = .$negative_cases)) %>%
  rename(negative_cases_week_avg = moving_avg)
negative_cases <- left_join(negative_cases, fits, by = c("testType", "date"))

## add new cases and weekly average to tests data frame
tests <- left_join(tests, negative_cases, by = c("testType", "date"))

## the following are diagnostic plots
if(FALSE){
  library(scales)
  
  source("functions.R")
  lag_to_complete <- 7
  last_day <- today() - days(lag_to_complete)
  
  ## check positivity rate
  
  plot_positivity(tests, first_day, today(), type = "Molecular", show.all = FALSE) +
    geom_smooth(method = "loess", formula = "y~x", span = 0.2, method.args = list(degree = 1, weight = tests$tests), color = "red", lty =2, fill = "pink") 
  
  plot_positivity(tests, first_day, today(), type = "Molecular", show.all = TRUE) 
  ## check test plot
  plot_test(tests, first_day, today())
  plot_test(tests, first_day, today(), type  = "Serological")
  plot_test(tests, first_day, today(), type  = "Antigens")
  plot_test(tests, first_day, today(), type  = "Molecular+Antigens")

  ## check cases plot
  ys <- TRUE
  plot_cases(cases, yscale = ys)
  plot_cases(cases, first_day, today(), type  = "Serological", yscale = ys)
  plot_cases(cases, first_day, today(), type  = "Antigens", yscale = ys)
  plot_cases(cases, first_day, today(), type  = "Molecular+Antigens", yscale = ys)
  
}

# -- summaries stratified by age group and patientID
mol_anti_2 <-  all_tests %>%
  filter(date >= first_day & testType %in% c("Molecular", "Antigens") & 
           result %in% c("positive", "negative")) %>%
  mutate(testType = "Molecular+Antigens") 

tests_by_strata <- all_tests %>%  
  bind_rows(mol_anti_2) %>%
  filter(date >= first_day & testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  filter(date>=first_day) %>%
  mutate(patientCity = fct_explicit_na(patientCity, "No reportado")) %>%
  mutate(ageRange = fct_explicit_na(ageRange, "No reportado")) %>%
  group_by(testType, date, patientCity, ageRange, .drop = FALSE) %>%
  summarize(positives = sum(result == "positive"), tests = n(), .groups="drop") %>%
  ungroup()

# --Mortality and hospitlization
# use old handmade database to fill in the blanks
old_hosp_mort <- read_csv("https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/DatosMortalidad.csv") %>%
  mutate(date = mdy(Fecha)) %>%
  filter(date >= first_day) %>%
  arrange(date) %>%
  select(date, HospitCOV19, CamasICU_disp, CamasICU)
# we started keeping track of available beds on 2020-09-20
# hosp_mort <- hosp_mort %>%
#   replace_na(list(CamasICU_disp = icu_beds))

httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
url <- "https://covid19datos.salud.gov.pr/estadisticas_v2/download/data/sistemas_salud/completo"
hosp_mort <- read.csv(text = rawToChar(httr::content(httr::GET(url)))) %>% 
  mutate(date = as_date(FE_REPORTE)) %>%
  filter(date >= first_day) %>%
  full_join(old_hosp_mort, by = "date") %>%
  arrange(date) %>%
  ## add columns to match old table
  mutate(HospitCOV19 = ifelse(is.na(CAMAS_ADULTOS_COVID), HospitCOV19, CAMAS_ADULTOS_COVID),
         CamasICU = ifelse(is.na(CAMAS_ICU_COVID), CamasICU, CAMAS_ICU_COVID),
         CamasICU_disp = ifelse(is.na(CAMAS_ICU_DISP), CamasICU_disp, CAMAS_ICU_DISP))
         
# -- seven day averages 
# deaths gets added later
# fits <- with(hosp_mort, 
#              ma7(d = date, y = IncMueSalud))
# hosp_mort$mort_week_avg <- fits$moving_avg

fits <- with(hosp_mort, 
             ma7(d = date, y = HospitCOV19))
hosp_mort$hosp_week_avg <- fits$moving_avg

fits <- with(hosp_mort, 
             ma7(d = date, y = CamasICU))
hosp_mort$icu_week_avg <- fits$moving_avg

ind <- which(!is.na(hosp_mort$CAMAS_PED_COVID))
fits <- with(hosp_mort[ind,], 
             ma7(d = date, y = CAMAS_PED_COVID))
hosp_mort$ped_hosp_week_avg <- rep(NA, nrow(hosp_mort))
hosp_mort$ped_hosp_week_avg[ind] <- fits$moving_avg

ind <- which(!is.na(hosp_mort$CAMAS_PICU_COVID))
fits <- with(hosp_mort[ind,], 
             ma7(d = date, y = CAMAS_PICU_COVID))
hosp_mort$picu_week_avg <- rep(NA, nrow(hosp_mort))
hosp_mort$picu_week_avg[ind] <- fits$moving_avg



## Vaccine data
## We will add it to hosp_mort data
url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
vaccines <- read_csv(url) %>% 
  filter(location == "Puerto Rico") %>%
  select(date, total_distributed, total_vaccinations, people_vaccinated, people_fully_vaccinated) %>%
  arrange(date) 

## fill in NAs
for(j in which(names(vaccines)!="date")){
  for(i in 2:nrow(vaccines)){
    if(is.na(vaccines[[j]][i])) vaccines[[j]][i] <- max(vaccines[[j]][1:(i-1)], na.rm=TRUE)
  }
}

## fill in the NAs
hosp_mort <- full_join(hosp_mort, vaccines, by = "date") 


# if(FALSE){
#   plot_deaths(hosp_mort)
# }

# Compute time it takes tests to come in ----------------------------------

message("Computing lag statistics.")

rezago <- all_tests_with_id  %>% 
  filter(result %in% c("positive", "negative") & 
           testType %in% original_test_types &
           resultCreatedAt >= collectedDate) %>% ## based on @midnucas suggestion: can't be added before it's reported
  group_by(testType) %>%
  mutate(diff = (as.numeric(resultCreatedAt) - as.numeric(collectedDate)) / (60 * 60 * 24),
          Resultado = factor(result, labels = c("Negativos", "Positivos"))) %>%
  ungroup %>%
  select(testType, date, Resultado, diff) %>%
  filter(!is.na(diff))


# Computing positivity rate by lab ----------------------------------------

url <- "https://bioportal.salud.gov.pr/api/administration/reports/tests-by-collected-date-and-entity"

message("Reading lab data.")

all_labs_data <- jsonlite::fromJSON(url)

labs <- all_labs_data %>%
  select(-molecular, -serological, -antigens) %>%
  rename(Laboratorio = entityName,
         date = collectedDate) %>%
  mutate(date = as_date(date),
         Laboratorio = str_trim(str_remove_all(tolower(Laboratorio), "\t|inc|\\.|\\,")))

                           
##check the most common labs
if(FALSE){
  freqs <- bind_cols(labs, all_labs_data$molecular) %>% 
    filter(date > make_date(2021, 1, 1)) %>%
    group_by(Laboratorio) %>%
    summarize(freq = sum(total), .groups = "drop") %>% 
    ungroup()
  freqs %>% View()
}

message("Processing lab data.")

labs <- labs %>%
  mutate(Laboratorio = case_when(str_detect(Laboratorio, "toledo") ~ "Toledo",
                                 str_detect(Laboratorio, "bcel") ~ "BCEL",
                                 str_detect(Laboratorio, "landr") ~ "Landrón",
                                 str_detect(Laboratorio, "cmt") ~ "CMT",
                                 str_detect(Laboratorio, "villa ana") ~ "Villa Ana",
                                 str_detect(Laboratorio, "labcorp") ~ "LabCorp",
                                 str_detect(Laboratorio, "quest") ~ "Quest",
                                 str_detect(Laboratorio, "borinquen") ~ "Borinquen",
                                 str_detect(Laboratorio, "coreplus") ~ "CorePlus",
                                 str_detect(Laboratorio, "martin\\s") ~ "Marin",
                                 str_detect(Laboratorio, "noy") ~ "Noy",
                                 str_detect(Laboratorio, "hato rey pathology|hrp") ~ "HRP",
                                 str_detect(Laboratorio, "inno") ~ "Inno Diagnostics",
                                 str_detect(Laboratorio, "immuno reference lab") ~ "Immuno Reference",
                                 str_detect(Laboratorio, "forense") ~ "Ciencias Forense",
                                 #str_detect(Laboratorio, "nichols") ~ "Quest USA",
                                 #str_detect(Laboratorio, "southern pathology services") ~ "Southern Pathology",
                                 TRUE ~ "Otros"))

molecular <- all_labs_data$molecular %>% 
  mutate(testType = "Molecular",
         positives = positives + presumptivePositives,
         negatives = negatives,
         tests = positives + negatives) %>%
  select(testType, positives, tests)
molecular <- bind_cols(labs, molecular) 

serological <-  all_labs_data$serological %>%
  mutate(testType = "Serological",
         positives = positives,
         negatives = negatives,
         tests = positives + negatives) %>%
  select(testType, positives, tests)
serological <- bind_cols(labs, serological) 

antigens <-  all_labs_data$antigens %>%
  mutate(testType = "Antigens",
         positives = positives,
         negatives = negatives,
         tests = positives + negatives) %>%
  select(testType, positives, tests)
antigens <- bind_cols(labs, antigens) 

labs <- bind_rows(molecular, serological, antigens) %>%
  filter(date >= first_day & date <= today()) %>%
  group_by(testType, date, Laboratorio) %>%
  summarize(positives = sum(positives),
            tests = sum(tests),
            missing_city = sum(totalMissingCity),
            missing_phone = sum(totalMissingPhoneNumber),
            .groups = "drop")


lab_positivity <- function(dat){
  day_seq <- seq(first_day + weeks(1), max(labs$date), by = "day")
  map_df(day_seq, function(the_day){
    ret <- dat %>% 
      filter(date > the_day - weeks(1) & date <= the_day) %>%
      summarize(date = the_day, 
                n = sum(tests),
                tests_week_avg  = n / 7, 
                fit = ifelse(n==0, 0, sum(positives) / n),
                lower = qbinom(0.025, n, fit) / n,
                upper = qbinom(0.975, n, fit) / n) %>%
      select(date, fit, lower, upper, tests_week_avg)
  })
}

fits <- labs %>% 
  nest_by(testType, Laboratorio) %>%
  summarize(lab_positivity(data), .groups = "drop") %>%
  group_by(testType, date) %>%
  mutate(prop = tests_week_avg / sum(tests_week_avg)) 

labs <- left_join(fits, labs, by = c("testType", "date", "Laboratorio"))


## For Eddie
lab_tab <- all_labs_data %>%
  select(-molecular, -serological, -antigens) %>%
  rename(Laboratorio = entityName,
         date = collectedDate) %>%
  mutate(date = as_date(date),
         Laboratorio = str_trim(str_remove_all(tolower(Laboratorio), "\t|inc|\\.|\\,")))

## wrange some of the names
lab_tab <- lab_tab %>%
  mutate(Laboratorio = case_when(str_detect(Laboratorio, "toledo") ~ "Toledo",
                                 str_detect(Laboratorio, "bcel") ~ "BCEL",
                                 str_detect(Laboratorio, "landr") ~ "Landrón",
                                 str_detect(Laboratorio, "cmt") ~ "CMT",
                                 str_detect(Laboratorio, "villa ana") ~ "Villa Ana",
                                 str_detect(Laboratorio, "labcorp") ~ "LabCorp",
                                 str_detect(Laboratorio, "quest") ~ "Quest",
                                 str_detect(Laboratorio, "borinquen") ~ "Borinquen",
                                 str_detect(Laboratorio, "coreplus") ~ "CorePlus",
                                 str_detect(Laboratorio, "martin\\s") ~ "Marin",
                                 str_detect(Laboratorio, "noy") ~ "Noy",
                                 str_detect(Laboratorio, "hato rey pathology|hrp") ~ "HRP",
                                 str_detect(Laboratorio, "inno") ~ "Inno Diagnostics",
                                 str_detect(Laboratorio, "immuno reference lab") ~ "Immuno Reference",
                                 str_detect(Laboratorio, "forense") ~ "Ciencias Forense",
                                 str_detect(Laboratorio, "nichols") ~ "Quest USA",
                                 str_detect(Laboratorio, "southern pathology services") ~ "Southern Pathology",
                                 TRUE ~ str_replace(str_to_title(Laboratorio), "Ii", "II")))

molecular <- all_labs_data$molecular %>% 
  mutate(testType = "Molecular",
         tests = positives + presumptivePositives + negatives) %>%
  select(testType, tests)
molecular <- bind_cols(lab_tab, molecular) 

serological <-  all_labs_data$serological %>%
  mutate(testType = "Serological",
         tests = positives + negatives) %>%
  select(testType, tests)
serological <- bind_cols(lab_tab, serological) 

antigens <-  all_labs_data$antigens %>%
  mutate(testType = "Antigens",
         tests = positives + negatives) %>%
  select(testType, tests)
antigens <- bind_cols(lab_tab, antigens) 

lab_tab <- bind_rows(molecular, serological, antigens) %>%
  filter(date >= first_day & date <= today()) %>%
  group_by(testType, date, Laboratorio) %>%
  summarize(tests = sum(tests),.groups = "drop")

lab_tab  <- lab_tab %>% group_by(Laboratorio, testType) %>% 
  mutate(total = sum(tests), .groups = "drop") %>% 
  ungroup() %>%
  group_by(testType) %>%
  mutate(Laboratorio = ifelse(total < 100, "Otros", Laboratorio)) %>%
  group_by(testType, date, Laboratorio) %>%
  summarize(tests = sum(tests),.groups = "drop") 


# By Region ---------------------------------------------------------------

## We now create the tests data table but by region
## The code is repetivie becuase this was added after we had the code for the global case

message("Computing by region statistics.")

tests_by_region <- all_tests_with_id %>%
  bind_rows(mol_anti) %>%
  filter(date >= first_day & 
           testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  group_by(testType, region, date) %>%
  summarize(people_positives = n_distinct(patientId[result == "positive"]),
            people_total = n_distinct(patientId),
            tests_positives = sum(result == "positive"),
            tests_total = n(),
            .groups = "drop") %>%
  mutate(rate = people_positives / people_total)

## run the function on each test type
fits <- all_tests_with_id %>% 
  bind_rows(mol_anti) %>%
  mutate(entry_date = as_date(orderCreatedAt)) %>%
  filter(date >= first_day & testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  nest_by(testType, region) %>%
  summarize(positivity(data), .groups = "drop")

## add new variable to test data frame
tests_by_region <- left_join(tests_by_region, fits, by = c("testType", "region", "date")) 

## compute weekly totals for positive tests and total tests
tests_by_region <- tests_by_region %>% 
  group_by(testType, region) %>%
  mutate(tests_positives_week = sum7(d = date, y = tests_positives)$moving_sum) %>%
  mutate(tests_total_week = sum7(d = date, y = tests_total)$moving_sum) %>%
  ungroup()

# compute unique cases ------------------------------------------------------------
cases_by_region <- all_cases %>%
  group_by(testType, region, date) %>% 
  summarize(cases = n(), .groups = "drop") 

# Make sure all dates are included
cases_by_region <- left_join(select(tests_by_region, testType, region, date), 
                             cases_by_region, 
                             by = c("testType", "region", "date")) %>%
  replace_na(list(cases = 0)) 

# compute daily weekly average and add to cases data frame
fits <- cases_by_region %>% 
  group_by(testType, region) %>%
  do(ma7(d = .$date, y = .$cases)) %>%
  rename(cases_week_avg = moving_avg)
cases_by_region <- left_join(cases_by_region, fits, by = c("testType", "region", "date"))

## add new cases and weekly average to tests data frame
tests_by_region <- left_join(tests_by_region, cases_by_region, by = c("testType", "region", "date")) %>%
  mutate(cases_plus_negatives = (people_total_week - people_positives_week + cases_week_avg * 7),
         cases_rate = cases_week_avg * 7 / cases_plus_negatives,
         cases_plus_negatives_daily = people_total - people_positives + cases,
         cases_rate_daily = cases / cases_plus_negatives_daily)

## Not using unique negatives so removed them to save time
## Compute unique negatives

# compute unique negative cases ------------------------------------------------------------
negative_cases_by_region <- all_tests_with_id %>%
  bind_rows(mol_anti) %>%
  filter(date>=first_day & result == "negative" &
           testType %in% test_types) %>%
  group_by(testType, region, patientId) %>%
  arrange(date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-patientId, -result) %>%
  arrange(testType, date) %>%
  group_by(testType, region, date) %>%
  summarize(negative_cases = n(), .groups = "drop")

# Make sure all dates are included
negative_cases_by_region <-  select(tests_by_region, testType, region, date) %>%
  left_join(negative_cases_by_region, by = c("testType", "region", "date")) %>%
  replace_na(list(negative_cases = 0))

# compute daily weekly average and add to negative_cases data frame
fits <- negative_cases_by_region %>%
  group_by(testType, region) %>%
  do(ma7(d = .$date, y = .$negative_cases)) %>%
  rename(negative_cases_week_avg = moving_avg)
negative_cases_by_region <- left_join(negative_cases_by_region, fits, by = c("testType", "region", "date"))

## add new cases and weekly average to tests data frame
tests_by_region <- left_join(tests_by_region, negative_cases_by_region, by = c("testType", "region", "date"))

## add regions populations
pop_by_region <- read_csv("https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/poblacion-region.csv",
                          skip = 1, col_names = c("rn", "region", "poblacion")) %>% 
  select(region, poblacion) %>%
  mutate(region = factor(region, levels = region[order(poblacion, decreasing = TRUE)]))

tests_by_region$region <- factor(as.character(tests_by_region$region), 
                                 levels = c(levels(pop_by_region$region), "No reportada"))



# By age ------------------------------------------------------------------
message("Computing by age statistics.")

age_starts <- c(0, 10, 15, 20, 30, 40, 65, 75)
age_ends <- c(9, 14, 19, 29, 39, 64, 74, Inf)

## compute daily totals
age_levels <- paste(age_starts, age_ends, sep = " a ")
age_levels[length(age_levels)] <- paste0(age_starts[length(age_levels)],"+")

tests_by_age <- all_tests_with_id %>% 
  bind_rows(mol_anti) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ageRange = age_levels[as.numeric(cut(age_start, c(age_starts, Inf), right = FALSE))]) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>%
  filter(date >= first_day & 
           testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  group_by(testType, date, ageRange) %>% 
  summarize(people_positives = n_distinct(patientId[result == "positive"]),
            people_total = n_distinct(patientId),
            tests_positives = sum(result == "positive"),
            tests_total = n(),
            .groups = "drop") %>%
  mutate(rate = people_positives / people_total)
         
## run the function on each test type
fits <- all_tests_with_id %>% 
  bind_rows(mol_anti) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ageRange = age_levels[as.numeric(cut(age_start, c(age_starts, Inf), right = FALSE))]) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>%
  mutate(entry_date = as_date(orderCreatedAt)) %>%
  filter(date >= first_day & testType %in% test_types & 
           result %in% c("positive", "negative")) %>%
  nest_by(testType, ageRange) %>%
  summarize(positivity(data), .groups = "drop")

## add new variable to test data frame
tests_by_age <- left_join(tests_by_age, fits, by = c("testType", "date", "ageRange")) 

## compute weekly totals for positive tests and total tests
tests_by_age <- tests_by_age %>% 
  group_by(testType, ageRange) %>%
  mutate(tests_positives_week = sum7(d = date, y = tests_positives)$moving_sum) %>%
  mutate(tests_total_week = sum7(d = date, y = tests_total)$moving_sum) %>%
  ungroup()

# compute daily new cases
cases_by_age <- all_cases %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ageRange = age_levels[as.numeric(cut(age_start, c(age_starts, Inf), right = FALSE))]) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>% 
  group_by(testType, date, ageRange) %>% 
  summarize(cases = n(), .groups = "drop") 

# Make sure all dates are included
cases_by_age <-  left_join(select(tests_by_age, testType, date, ageRange), cases_by_age, by = c("testType", "date", "ageRange")) %>%
  replace_na(list(cases = 0)) 

# compute daily weekly average and add to cases data frame
fits <- cases_by_age %>% 
  group_by(testType, ageRange) %>%
  do(ma7(d = .$date, y = .$cases)) %>%
  rename(cases_week_avg = moving_avg)
cases_by_age <- left_join(cases_by_age, fits, by = c("testType", "date", "ageRange"))

## add new cases and weekly average to tests data frame
tests_by_age <- left_join(tests_by_age, cases_by_age, by = c("testType", "date", "ageRange")) %>%
  mutate(cases_plus_negatives = (people_total_week - people_positives_week + cases_week_avg * 7),
         cases_rate = cases_week_avg * 7 / cases_plus_negatives,
         cases_plus_negatives_daily = people_total - people_positives + cases,
         cases_rate_daily = cases / cases_plus_negatives_daily)

# compute unique negative cases ------------------------------------------------------------
negative_cases_by_age <- all_tests_with_id %>%
  bind_rows(mol_anti) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ageRange = age_levels[as.numeric(cut(age_start, c(age_starts, Inf), right = FALSE))]) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>%
  filter(date>=first_day & result == "negative" &
           testType %in% test_types) %>%
  group_by(testType, ageRange, patientId) %>%
  arrange(date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-patientId, -result) %>%
  arrange(testType, date) %>%
  group_by(testType, ageRange, date) %>%
  summarize(negative_cases = n(), .groups = "drop")

# Make sure all dates are included
negative_cases_by_age <-  select(tests_by_age, testType, ageRange, date) %>%
  left_join(negative_cases_by_age, by = c("testType", "ageRange", "date")) %>%
  replace_na(list(negative_cases = 0))

# compute daily weekly average and add to negative_cases data frame
fits <- negative_cases_by_age %>%
  group_by(testType, ageRange) %>%
  do(ma7(d = .$date, y = .$negative_cases)) %>%
  rename(negative_cases_week_avg = moving_avg)

negative_cases_by_age <- left_join(negative_cases_by_age, fits, by = c("testType", "ageRange", "date"))

## add new cases and weekly average to tests data frame
tests_by_age <- left_join(tests_by_age, negative_cases_by_age, by = c("testType", "ageRange", "date"))

## add age populations
pop_by_age <- read_csv("https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/poblacion-por-edad.csv") %>%
  rename(ageRange = agegroup, poblacion = population) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ageRange = age_levels[as.numeric(cut(age_start, c(age_starts, Inf), right = FALSE))]) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>% 
  group_by(ageRange) %>%
  summarize(poblacion = sum(poblacion), .groups = "drop") %>%
  mutate(ageRange = str_replace(ageRange, "-", " to ")) %>%
  mutate(ageRange = factor(ageRange, levels = levels(tests_by_age$ageRange)))
  
## add deaths by age

message("Computing deaths by age.")

url <- "https://bioportal.salud.gov.pr/api/administration/reports/deaths/summary"

deaths <- jsonlite::fromJSON(url) %>%
  mutate(date = as_date(ymd_hms(deathDate, tz = "America/Puerto_Rico"))) %>%
  mutate(date = if_else(date < first_day | date > today(), 
                        as_date(ymd_hms(reportDate, tz = "America/Puerto_Rico")),
                        date)) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ageRange = age_levels[as.numeric(cut(age_start, c(age_starts, Inf), right = FALSE))]) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) 
  
## replace the death data with BioPortal data for consistency

hosp_mort <- deaths %>%
  group_by(date) %>%
  summarize(deaths = n(), .groups = "drop") %>%
  full_join(hosp_mort, by = "date") %>%
  arrange(date) %>%
  mutate(deaths = replace_na(deaths,0)) %>%
  mutate(IncMueSalud = deaths,
         mort_week_avg =  ma7(date, deaths)$moving_avg) %>%
  select(-deaths)

## Use this to assure all dates are included
all_dates <- data.frame(date = seq(first_day, max(c(last_complete_day, deaths$date)), by = "day"))
deaths_by_age <- deaths %>%
  filter(!is.na(ageRange)) %>%
  group_by(date, ageRange) %>%
  summarize(deaths = n(), .groups = "drop") %>%
  ungroup() %>%
  full_join(all_dates, by = "date") %>%
  complete(date, nesting(ageRange), fill = list(deaths = 0)) %>%
  filter(!is.na(ageRange)) %>%
  group_by(ageRange) %>%
  mutate(deaths_week_avg = ma7(date, deaths)$moving_avg)


## Rezagos muerte

## The following code commented out because we now use Salud API
## Adding rezago computation for deaths
# dat <- read_csv("https://raw.githubusercontent.com/sacundim/covid-19-puerto-rico/master/assets/data/cases/PuertoRico-bitemporal.csv",
#                 col_types = cols(bulletin_date = col_date(),
#                                  datum_date = col_date(),
#                                  confirmed_and_suspect_cases = col_integer(),
#                                  confirmed_cases = col_integer(),
#                                  probable_cases = col_integer(),
#                                  suspect_cases = col_integer(),
#                                  deaths = col_integer()))

rezago_mort <- deaths %>% 
  filter(!is.na(date)) %>%
  mutate(bulletin_date = as_date(ymd_hms(reportDate, tz = "America/Puerto_Rico"))) %>%
  arrange(date, bulletin_date) %>%
  mutate(diff = (as.numeric(bulletin_date) - as.numeric(date))) %>%
  select(date, diff)


## add passanger data

message("Computing traveler statistics.")

url <- "https://BioPortal.salud.gov.pr/api/administration/reports/travels/total-forms-by-reported-arrival-date"

travelers <- jsonlite::fromJSON(url) %>%
  mutate(date = mdy(date)) %>%
  filter(year(date)>=2020) %>%
  filter(date <= today()) %>%
  mutate(residents_week_avg =  ma7(date, residents)$moving_avg,
         perc_residents = percentageResidentsArrivedWithNegativePcrResults/100,
         perc_residents_week_avg = ma7(date, perc_residents*residents)$moving_avg/residents_week_avg,

         short = nonResidentsStayingLessThan5Days,
         short_week_avg =  ma7(date, short)$moving_avg,
         perc_short = percentageNonResidentsStayingLessThan5DaysArrivedWithNegativePcrResults/100,
         perc_short_week_avg  = ma7(date, perc_short*short)$moving_avg/short_week_avg,

         long = nonResidentsStaying5DaysOrMore,
         long_week_avg =  ma7(date, nonResidentsStaying5DaysOrMore)$moving_avg,
         perc_long = percentageResidentsStaying5DaysOrMoreArrivedWithNegativePcrResults/100,
         perc_long_week_avg  = ma7(date, perc_long*long)$moving_avg/long_week_avg) %>%
  select(date, 
         residents, perc_residents, short, perc_short, long, perc_long,
         residents_week_avg, perc_residents_week_avg, short_week_avg, perc_short_week_avg, long_week_avg, perc_long_week_avg)


# -- Save data

message("Saving data.")

## if on server, save with full path
## if not on server, save to home directory
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/pr-covid/dashboard/rdas"
} else{
  rda_path <- "rdas"
}
## define date and time of latest download
the_stamp <- now()
save(first_day, last_complete_day,
     alpha, the_stamp, 
     tests, cases,
     hosp_mort, labs, pr_pop, 
     file = file.path(rda_path, "data.rda"))

save(labs, file = file.path(rda_path, "labs.rda"))

save(tests_by_strata, file = file.path(rda_path, "tests_by_strata.rda"))

save(lab_tab, file = file.path(rda_path, "lab_tab.rda"))

save(rezago, file = file.path(rda_path, "rezago.rda"))

save(rezago_mort, file = file.path(rda_path, "rezago_mort.rda"))

save(tests_by_region, pop_by_region, file = file.path(rda_path, "regions.rda"))

save(deaths_by_age, tests_by_age, pop_by_age, file = file.path(rda_path, "by-age.rda"))

save(travelers, file = file.path(rda_path, "travelers.rda"))

## For backward compatibility
all_tests <- all_tests %>%  filter(testType == "Molecular")
saveRDS(all_tests, file = file.path(rda_path, "all_tests.rds"), compress = "xz")
saveRDS(all_tests_with_id, file = file.path(rda_path, "all_tests_with_id.rds"), compress = "xz")




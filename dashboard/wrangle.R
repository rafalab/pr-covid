# -- Libraries
library(tidyverse)
library(lubridate)
library(splines)

# -- Fixed values
first_day <- make_date(2020, 3, 12)
url <- "https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"

# Reading data from database ----------------------------------------------
all_tests <- jsonlite::fromJSON(url)

# Wrangling test data -----------------------------------------------------

## defining age levels
age_levels <-  c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", 
                 "70 to 79", "80 to 89", "90 to 99", "100 to 109", "110 to 119", "120 to 129")

## Defining rda with all the tests 
all_tests <- all_tests %>%  
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

## fixing dates: if you want to remove bad dates instead, change FALSE TO TRUE
if(FALSE){
  ## remove bad dates
  all_tests <- all_tests %>% 
  filter(!is.na(collecedDate) & year(collectedDate) == 2020 & collectedDate <= today()) %>%
  mutate(date = collectedDate) 
} else{
  ## Impute missing dates
  all_tests <- all_tests %>% mutate(date = if_else(is.na(collectedDate), reportedDate - days(2),  collectedDate))
  ## Remove inconsistent dates
  all_tests <- all_tests %>%
    mutate(date = if_else(year(date) != 2020 | date > today(), reportedDate - days(2),  date)) %>%
    filter(year(date) == 2020 & date <= today()) %>%
    arrange(date, reportedDate)
}

# -- Computing observed tasa de positividad and smooth fit
tests <- all_tests %>%  
  filter(date >= first_day & testType %in% c("Molecular", "Serological")) %>%
  filter(result %in% c("positive", "negative")) %>%
  group_by(testType, date) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  ungroup() %>%
  mutate(rate = positives / tests) %>%
  mutate(weekday = factor(wday(date)))

spline_fit <- function(d, y, n = NULL, ind = rep(TRUE, length(d)), week_effect = TRUE, knots_per_month = 2, family = "quasibinomial"){
  x <- as.numeric(d)
  ind <- which(ind)
  ## Design matrix for splines
  ## We are using 3 knots per monnth
  ## And ignoring last week
  df  <- round(knots_per_month * length(ind)/30)
  nknots <- df - 1
  # remove boundaries and also 
  knots <- seq.int(from = 0, to = 1, length.out = nknots + 2L)[-c(1L, nknots + 2L)]
  knots <- quantile(x[ind], knots)
  if(week_effect){
    x_s <- ns(x, knots = knots, Boundary.knots = range(x[ind]), intercept = FALSE)
    i_s <- 1:(ncol(x_s)+1) ## last column comes from first column of w which is intercept
    ## Design matrix for weekday effect
    w            <- factor(wday(d))
    contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
    x_w          <- model.matrix(~w)
    
    ## Design matrix
    X <- cbind(x_s, x_w)
  } else{
    x_s <- ns(x, knots = knots, Boundary.knots = range(x[ind]), intercept = TRUE)
    i_s <- 1:(ncol(x_s)) ## last column comes from first column of w which is intercept
    X <- x_s
  }
  ## Fitting model 
  if(family %in% c("quasibinomial", "binomial")){
    if(is.null(n)) stop("Must define n.")
    glm_fit  <- glm(cbind(y, n-y)[ind,] ~ -1 + X[ind,], family = family)
  } else{
    glm_fit <- glm(y[ind] ~ -1 + X[ind,], family = family)
  }
  
  beta <- coef(glm_fit)

  return(tibble(date = d,
               fit = as.vector(X[, i_s] %*% beta[i_s]),
               se  = sqrt(diag(X[, i_s] %*%
                                  summary(glm_fit)$cov.scaled[i_s, i_s] %*%
                                  t(X[, i_s])) * pmax(1,summary(glm_fit)$dispersion))))
}

fits <- tests %>% group_by(testType) %>%
  do(spline_fit(.$date, .$positives, .$tests, .$date <= today() - days(2)))
names(fits)[3:4] <- c("rate_fit", "rate_se")

tests <- left_join(tests, fits, by = c("testType", "date"))

if(FALSE){
  alpha <- 0.01
  z <- qnorm(1-alpha/2)
  
  expit <- function(x) { 1/ (1 + exp(-x))  }
  
  tests %>%
    filter(date >= make_date(2020,3,21) & date <= today()) %>%
    ggplot(aes(date, rate)) +
    geom_hline(yintercept = 0.05, lty=2, color = "gray") +
    geom_point(aes(date, rate), size=2, alpha = 0.65) +
    geom_ribbon(aes(ymin= expit(rate_fit - z*rate_se), ymax = expit(rate_fit + z*rate_se)), alpha = 0.35) +
    geom_line(aes(y = expit(rate_fit)), color="blue2", size = 0.80) +
    ylab("Tasa de positividad") +
    xlab("Fecha") +
    ggtitle("Tasa de Positividad en Puerto Rico") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_labels = "%B %d") +
    geom_smooth(method = "loess", formula = "y~x", span = 0.2, method.args = list(degree = 1, weight = tests$tests), color = "red", lty =2, fill = "pink") +
    facet_wrap(~testType) +
    theme_bw() 
}

##apply similar model to tests, show weekly average
tests <- tests %>% 
  mutate(week = floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(testType, week) %>%
  mutate(tests_week_avg = mean(tests, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-week)

if(FALSE){
  tests %>%
    filter(date >= make_date(2020,3,21) & date <= today()) %>%
    ggplot(aes(date, tests)) +
    geom_bar(stat = "identity", width = 0.75, alpha = 0.65) +
    geom_line(aes(y = tests_week_avg), color="blue2", size = 0.80) +
    ylab("Prueba") +
    xlab("Fecha") +
    ggtitle("Pruebas en Puerto Rico") +
    scale_x_date(date_labels = "%B %d") +
    geom_smooth(method = "loess", formula = "y~x", span = 0.2, method.args = list(degree = 1, weight = tests$tests), color = "red", lty = 2, fill = "pink") +
    facet_wrap(~testType) +
    theme_bw()
}

# unique cases ------------------------------------------------------------
cases <- all_tests %>%          
  filter(date>=first_day &                                   
         result == c("positive")) %>%
  group_by(testType, patientId) %>%
  mutate(n=n()) %>%
  filter(order(date) == 1) %>% ##min can be more than 1
  ungroup() %>%
  mutate(region = fct_explicit_na(region, "No reportado")) %>%
  mutate(ageRange = fct_explicit_na(ageRange, "No reportado")) %>%
  select(-patientId, -result) 

# Add cases to tests data frame -------------------------------------------
cases_per_day <- cases %>%
  group_by(testType, date) %>% 
  summarize(cases = n())
  
tests <-tests %>% left_join(cases_per_day, by=c("testType", "date")) %>%
  replace_na(list(cases = 0L))

fits <- tests %>% group_by(testType) %>%
  do(spline_fit(d = .$date, y = .$cases, ind = .$date <= today() - days(2), family = "quasipoisson"))
names(fits)[3:4] <- c("cases_fit", "cases_se")
tests <- left_join(tests, fits, by = c("testType", "date"))


if(FALSE){
  alpha <- 0.01
  z <- qnorm(1-alpha/2)
  
  tests %>%
    filter(date >= make_date(2020,3,21) & date <= today()) %>%
    ggplot(aes(date, cases)) +
    geom_hline(yintercept = 0.05, lty=2, color = "gray") +
    geom_point(aes(date, cases), size=2, alpha = 0.65) +
    geom_ribbon(aes(ymin= exp(cases_fit - z*cases_se), ymax = exp(cases_fit + z*cases_se)), alpha = 0.35) +
    geom_line(aes(y = exp(cases_fit)), color="blue2", size = 0.80) +
    ylab("Tasa de positividad") +
    xlab("Fecha") +
    ggtitle("Casos en Puerto Rico") +
    scale_x_date(date_labels = "%B %d") +
    geom_smooth(method = "loess", formula = "y~x", span = 0.2, method.args = list(degree = 1, weight = tests$tests), color = "red", lty =2, fill = "pink") +
    facet_wrap(~testType) +
    theme_bw()
}

# -- summaries stratified by age group and patientID
tests_by_strata <- all_tests %>%  
  filter(date >= first_day) %>%
  filter(result %in% c("positive", "negative")) %>%
  mutate(region = fct_explicit_na(region, "No reportado")) %>%
  mutate(ageRange = fct_explicit_na(ageRange, "No reportado")) %>%
  group_by(testType, date, region, ageRange, .drop = FALSE) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  ungroup()

cases_by_strata <- cases %>%
  filter(date>=first_day) %>%
  group_by(testType, date, region, ageRange, .drop = FALSE) %>%
  dplyr::summarize(cases = n()) %>%
  ungroup()

tests_by_strata <- tests_by_strata %>% 
  left_join(cases_by_strata, by = c("testType","date", "region", "ageRange")) %>%
  replace_na(list(cases = 0L))

# --Mortality and hospitlization
hosp_mort <- read_csv("https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/DatosMortalidad.csv") %>%
  mutate(date = mdy(Fecha)) %>% 
  filter(date >= first_day) 

# -- model to deaths. Here there is no weekend effect
fits <- with(hosp_mort, spline_fit(d = date, y = IncMueSalud, week_effect = FALSE, family = "quasipoisson"))
hosp_mort$fit <- fits$fit
hosp_mort$se  <- fits$se

if(FALSE){
  hosp_mort %>%
    ggplot(aes(date)) +
    geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
    geom_line(aes(y = exp(fit)), color="black", size = 1.25) +
    ylab("Muertes") +
    xlab("Fecha") +
    ggtitle("Muertes por COVID-19 en Puerto Rico") +
    scale_x_date(date_labels = "%B %d") +
    scale_y_continuous(breaks = seq(0, 15, 1)) +
    theme_bw()
}
# -- Save data

## if on server, save with full path
## if not on server, save to home directory
if(Sys.info()["nodename"] == "fermat.dfci.harvard.edu"){
  rda_path <- "/homes10/rafa/dashboard/rdas"
} else{
  rda_path <- "rdas"
}

## define date and time of latest download
the_stamp <- now()
save(tests, tests_by_strata, hosp_mort, the_stamp, file = file.path(rda_path,"data.rda"))
## all_tests no longer saved becuase API added an ID
## saveRDS(all_tests, file = file.path(rda_path, "all_tests.rds"), compress = "xz")


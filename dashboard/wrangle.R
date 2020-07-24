# -- Libraries
library(tidyverse)
library(lubridate)
library(splines)
library(scales)
library(sf)

# -- Fixed values
first_day <- make_date(2020, 3, 15)
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
         patientCity    = ifelse(patientCity == "Loiza", "Loíza", patientCity),
         patientCity    = ifelse(patientCity == "Rio Grande", "Río Grande", patientCity),
         patientCity    = factor(patientCity),
         result         = tolower(result),
         result         = case_when(grepl("positive", result) ~ "positive",
                                    grepl("negative", result) ~ "negative",
                                    result == "not detected" ~ "negative",
                                    TRUE ~ "other")) %>%
         arrange(reportedDate, collectedDate) 

## fixing dates: if you want to remove bad dates instead, change FALSE TO TRUE
if(FALSE){
  ## remove bad dates
  all_tests <- all_tests %>% 
  filter(!is.na(collectedDate) & year(collectedDate) == 2020 & collectedDate <= today()) %>%
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
  filter(date>=first_day) %>%
  filter(result %in% c("positive", "negative")) %>%
  group_by(date) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  mutate(rate = positives / tests) %>%
  mutate(weekday = factor(wday(date)))

##Extracting variables for model fit
x <- as.numeric(tests$date)
y <- tests$positives
n <- tests$tests

## Design matrix for splines
df  <- round(3 * nrow(tests)/30)
x_s <- ns(x, df = df, intercept = FALSE)
i_s <- c(1:(ncol(x_s)+1))

## Design matrix for weekday effect
w            <- factor(wday(tests$date))
contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
x_w          <- model.matrix(~w)

## Design matrix
X <- cbind(x_s, x_w)

## Fitting model 
fit  <- glm(cbind(y, n-y) ~ -1 + X, family = "quasibinomial")
beta <- coef(fit)

## Computing probabilities
tests$fit <- as.vector(X[, i_s] %*% beta[i_s])
tests$se  <- sqrt(diag(X[, i_s] %*%
                         summary(fit)$cov.scaled[i_s, i_s] %*%
                         t(X[, i_s])) * pmax(1,summary(fit)$dispersion))


# -- summaries stratified by age group and patientID
tests_by_strata <- all_tests %>%  
  filter(date>=first_day) %>%
  filter(result %in% c("positive", "negative")) %>%
  mutate(patientCity = fct_explicit_na(patientCity, "No reportado")) %>%
  mutate(ageRange = fct_explicit_na(ageRange, "No reportado")) %>%
  group_by(date, patientCity, ageRange, .drop = FALSE) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  ungroup()


# --Mortality and hospitlization
hosp_mort <- read_csv("https://raw.githubusercontent.com/rafalab/pr-covid/master/dashboard/data/DatosMortalidad.csv") %>%
  mutate(date = mdy(Fecha)) %>% 
  filter(date >= first_day) 


# -- Extracting variables for model fit
x <- as.numeric(hosp_mort$date)
y <- hosp_mort$IncMueSalud

# -- Design matrix for splines
df  <- round(nrow(hosp_mort)/30)
x_s <- ns(x, df = df, intercept = FALSE)
i_s <- c(1:(ncol(x_s)+1))

# -- Design matrix for weekday effect
w            <- factor(wday(hosp_mort$date))
contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
x_w          <- model.matrix(~w)

# -- Design matrix
X <- cbind(x_s, x_w)

# -- Fitting model 
fit  <- glm(y ~ -1 + X, family = "quasipoisson")
beta <- coef(fit)

# -- Computing probabilities
hosp_mort$fit <- as.vector(X[, i_s] %*% beta[i_s])
hosp_mort$se  <- sqrt(diag(X[, i_s] %*%
                             summary(fit)$cov.scaled[i_s, i_s] %*%
                             t(X[, i_s]))* pmax(1,summary(fit)$dispersion))

# -- Save data

## define date and time of latest download
the_stamp <- now()
save(tests, tests_by_strata, hosp_mort, the_stamp, file = "rdas/data.rda")
## save the big file for those that want to download it
saveRDS(all_tests, file = "rdas/all_tests.rds", compress = "xz")


## 
# -- Check with Visualizations
if(FALSE){
  tests %>%
    ggplot(aes(date, rate)) +
    geom_point(aes(date, rate), alpha=0.50, size=1) +
    geom_ribbon(aes(ymin= expit(fit - 3*se), ymax = expit(fit + 3*se)), alpha=0.20) +
    geom_line(aes(y = expit(fit)), color="blue2", size=0.80) +
    theme_bw()

  hosp_mort %>% 
    ggplot(aes(date, IncMueSalud)) +
    geom_ribbon(aes(ymin = exp(fit - 2*se), ymax = exp(fit + 2*se)), alpha = 0.5) +
    #geom_bar(stat = "identity") + 
    geom_point() +
    geom_line(aes(y = exp(fit))) 
  
  max_y <- pmax(max(hosp_mort$HospitCOV19, na.rm = TRUE), 700)
  hosp_mort %>% 
    filter(!is.na(HospitCOV19)) %>%
    ggplot(aes(date, HospitCOV19)) +
    geom_point() +
    geom_smooth(formula = y~x, method = "loess", span = 14/nrow(hosp_mort), method.args = list(degree = 1, family = "symmetric")) +
    scale_y_continuous(limits = c(0, max_y)) + 
    geom_hline(yintercept = 691, lty = 2, level = 1 - alpha) + 
    annotate("text", x = make_date(2020, 5, 1), y = 695, label = "Camas disponibles en los ICU", vjust = 0)
}

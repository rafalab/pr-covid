source("init.R")

# -- Getting url
url <- "https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"
dat <- jsonlite::fromJSON(url)

dat <- dat %>%  
  as_tibble() %>%
  mutate(ageRange       = gsub(" to ", "-", ageRange),
         collectedDate  = mdy(collectedDate),
         reportedDate   = mdy(reportedDate),
         createdAt      = mdy_hm(createdAt),
         ageRange       = na_if(ageRange, "N/A"),
         result         = tolower(result),
         result         = case_when(grepl("positive", result) ~ "positive",
                                    grepl("negative", result) ~ "negative",
                                    result == "not detected" ~ "negative",
                                    TRUE ~ "other")) %>%
         arrange(reportedDate, collectedDate) 

if(FALSE){
  ## remove bad dates
  dat <- dat %>% 
  filter(!is.na(collectedDate) & year(collectedDate) == 2020 & collectedDate <= today()) %>%
  mutate(date = collectedDate) 
} else{
  ## Impute missing dates
  dat <- dat %>% mutate(date = if_else(is.na(collectedDate), reportedDate - days(2),  collectedDate))

  ## Remove inconsistent dates
  dat <- dat %>%
    mutate(date = if_else(year(date) != 2020 | date > today(), reportedDate - days(2),  date)) %>%
    filter(year(date) == 2020 & date <= today()) %>%
    arrange(date, reportedDate)
}

# -- Observed tasa de positividad
tests <- dat %>%  
  filter(date>=make_date(2020, 3, 15)) %>%
  filter(result %in% c("positive", "negative")) %>%
  group_by(date) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  mutate(rate = positives / tests) %>%
  mutate(weekday = factor(wday(date)))

# -- Extracting from tests
x <- as.numeric(tests$date)
y <- tests$positives
n <- tests$tests

# -- Design matrix for splines
df  <- round(3 * nrow(tests)/30)
x_s <- ns(x, df = df, intercept = FALSE)
i_s <- c(1:(ncol(x_s)+1))

# -- Design matrix for weekday effect
w            <- factor(wday(tests$date))
contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
x_w          <- model.matrix(~w)

# -- Design matrix
X <- cbind(x_s, x_w)

# -- Fitting model 
fit  <- glm(cbind(y, n-y) ~ -1 + X, family = "quasibinomial")
beta <- coef(fit)

# -- Computing probabilities
tests$fit <- as.vector(X[, i_s] %*% beta[i_s])
tests$se  <- sqrt(diag(X[, i_s] %*%
                         summary(fit)$cov.scaled[i_s, i_s] %*%
                         t(X[, i_s])))

# -- Tasa de positividad per municipio
municipio_tests <- dat %>%
  filter(date >= make_date(2020, 3, 15)) %>%
  filter(result %in% c("positive", "negative")) %>%
  group_by(date, patientCity) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  ungroup() %>%
  mutate(rate = positives / tests) %>%
  mutate(weekday = factor(wday(date)))

# -- Tasa de positividad per age groups
the_breaks <- c(0, 20, 40, 60, Inf)
agegroup_tests <- dat %>%
  filter(date >= make_date(2020, 3, 15)) %>%
  filter(result %in% c("positive", "negative")) %>%
  separate(ageRange, c("age_lower", "age_upper"), sep = "-", remove = FALSE) %>%
  mutate(age_lower = as.numeric(age_lower),
         age_upper = as.numeric(age_upper),
         agegroup  = cut(age_lower, the_breaks, right = FALSE, labels = paste(the_breaks[-length(the_breaks)], c(the_breaks[-1]-1), sep="-")),
         agegroup  = factor(agegroup)) %>%
  group_by(date, agegroup) %>%
  dplyr::summarize(positives = sum(result == "positive"), tests = n()) %>%
  ungroup() %>%
  mutate(rate      = positives / tests,
         weekday   = factor(wday(date)))

# -- Model per agegroup
agegroup_tests <- map_df(levels(agegroup_tests$agegroup), function(x){
  
  tmp_dat <- filter(agegroup_tests, agegroup == x)
  Xm      <- X[1:nrow(tmp_dat),]
  
  y <- tmp_dat$positives
  n <- tmp_dat$tests
  
  # -- Fitting model 
  fit  <- glm(cbind(y, n-y) ~ -1 + Xm, family = "quasibinomial")
  beta <- coef(fit)
  
  # -- Computing probabilities
  tmp_dat$fit <- as.vector(Xm[, i_s] %*% beta[i_s])
  tmp_dat$se  <- sqrt(diag(Xm[, i_s] %*% 
                             summary(fit)$cov.scaled[i_s, i_s] %*%
                             t(Xm[, i_s])))
  
  return(tmp_dat)
})

save(tests, file = "rdas/tests.rda")
save(municipio_tests, file = "rdas/municipio-tests.rda")
save(agegroup_tests, file = "rdas/agegroup-tests.rda")
all_tests <- dat
attr(all_tests, "date") <- today()
save(all_tests, file = "rdas/all_tests.rda")

# -- Visualizations
if(FALSE){
tests %>%
  ggplot(aes(date, rate)) +
  geom_point(aes(date, rate), alpha=0.50, size=1) +
  geom_ribbon(aes(ymin= expit(fit - 3*se), ymax = expit(fit + 3*se)), alpha=0.20) +
  geom_line(aes(y = expit(fit)), color="blue2", size=0.80) +
  theme_bw()
}

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

## Impute missing dates
dat <- dat %>% mutate(date = if_else(is.na(collectedDate), reportedDate - days(2),  collectedDate))

## Remove inconsistent dates
dat <- dat %>%
  mutate(date = if_else(year(date) != 2020 | date > today(), reportedDate - days(2),  date)) %>%
  filter(year(date) == 2020 & date <= today()) %>%
  arrange(date, reportedDate)

# -- Observed tasa de positividad
tests <- dat %>%  
  filter(date>=make_date(2020, 3, 15)) %>%
  filter(result %in% c("positive", "negative")) %>%
  group_by(date) %>%
  summarize(positives = sum(result == "positive"), tests = n()) %>%
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

save(tests, file = "rdas/tests.rda")
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

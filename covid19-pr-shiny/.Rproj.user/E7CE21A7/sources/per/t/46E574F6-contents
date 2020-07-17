source("init.R")

# -- Getting url
url <- "https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"

# -- HTTP get
http_get <- GET(url)

# -- Turning object into json
json <- content(http_get)

# -- Wrangle
dat <- do.call(rbind, json) %>% 
  as_tibble() %>%
  mutate_all(as.character) %>%
  setNames(c("collected", "reported", "agegroup", "test_type", "result", "municipio", "created_at")) %>%
  mutate_all(as.character) %>%
  mutate(agegroup   = gsub(" to ", "-", agegroup),
         collected  = gsub("/", "-", collected),
         reported   = gsub("/", "-", reported),
         created_at = gsub("/", "-", created_at),
         collected  = mdy(collected),
         reported   = mdy(reported),
         created_at = mdy_hm(created_at),
         municipio  = ifelse(municipio=="NULL", NA, municipio),
         result     = ifelse(result=="NULL", NA, result),
         test_type  = ifelse(test_type=="NULL", NA, test_type),
         agegroup   = ifelse(agegroup %in% c("NULL", "N/A"), NA, agegroup),
         result     = tolower(result),
         result     = case_when(grepl("positive", result) ~ "positive",
                                grepl("negative", result) ~ "negative",
                                result == "not detected" ~ "negative",
                                TRUE ~ "other")) %>%
         filter(year(reported) == 2020, year(collected) == 2020) %>%
         arrange(reported, collected)
save(dat, file = "rdas/pruebas-pr.rda", compress = "xz")

# -- Observed tasa de positividad
rate <- dat %>%
  group_by(reported, result) %>%
  summarize(number_test = n()) %>%
  ungroup() %>%
  complete(reported, result, fill = list(number_test = 0)) %>%
  spread(result, number_test) %>%
  mutate(rate = positive / (positive+other+negative)) %>%
  select(reported, rate)

# -- Code to fit the model 
df <- dat %>%
  group_by(reported) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(reported, result) %>%
  mutate(y = n()) %>%
  ungroup() %>%
  filter(result == "positive") %>%
  select(y, n, reported) %>%
  unique() %>%
  rename(x = reported)

# -- Extracting from df
x <- df$x
y <- df$y
n <- df$n

# -- Design matrix for weekday effect
w            <- factor(wday(x))
contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
x_w          <- model.matrix(~w)
i_w <- 1:ncol(x_w)

# -- Design matrix for splines
x_s <- ns(x, df = 7, intercept = FALSE)
# x_s <- sweep(x_s, 2, colMeans(x_s))
i_s <- ncol(x_w) + 1:ncol(x_s)

# -- Design matrix
X <- cbind(x_w, x_s)

# -- Fitting model 
fit  <- glm(cbind(y, n-y) ~ -1 + X, family = "binomial")
summary(fit)
beta <- coef(fit)

# -- Computing probabilities
probs <- X[, i_s] %*% beta[i_s]
se    <- sqrt(diag(X[, i_s] %*% summary(fit)$cov.scaled[i_s, i_s] %*% t(X[, i_s])))
# probs <- X %*% beta
# se    <- sqrt(diag(X %*% summary(fit)$cov.scaled %*% t(X)))

# -- Visualizations
tibble(x, probs, se) %>%
  mutate(lwr   = probs - 1.96*se,
         upr   = probs + 1.96*se,
         probs = expit(probs),
         lwr   = expit(lwr),
         upr   = expit(upr)) %>%
  ggplot(aes(x, probs)) +
  geom_point(aes(reported, rate), data = rate, alpha=0.50, size=1) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA) +
  geom_line(color="blue2", size=0.80) +
  theme_bw()

# -- Pruebas por semana
tests <- dat %>% 
  filter(result %in% c("positive", "negative")) %>% 
  filter(reported >= make_date(2020, 3, 15)) %>%
  group_by(date = ceiling_date(reported, 
                               unit = "week", 
                               week_start = wday(max(dat$reported)))) %>%
  summarize(tests = n()) 

# -- Viz
tests %>%
  ggplot(aes(date, tests)) +
  geom_bar(color="black", size=0.20, stat = "identity") +
  ggtitle("Pruebas por semana") +
  xlab("Semana acabando en esta fecha") +
  ylab("Pruebas") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 30000, by = 5000)) +
  scale_x_date(date_labels = "%B %d") +
  theme_bw()













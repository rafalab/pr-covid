library(tidyverse)
library(lubridate)
library(readxl)

day_received <- make_date(2020, 7, 14)

fn <- "data/Bioportal\ molecular\ tests\ 7-14-2020.xlsx"
dat <- read_xlsx(fn) 

dat <- mutate(dat, date = mdy(collectedDate)) %>%
  mutate(result = tolower(result)) %>%
  mutate(result = case_when(str_detect(result, "igm") ~ "other",
                            str_detect(result, "positive") ~ "positive",
                            str_detect(result, "negative") ~ "negative",
                            result == "not detected" ~ "negative",
                            TRUE ~ "other")) 
## Impute missing dates
dat <- dat %>% mutate(date = if_else(is.na(date), mdy(dat$reportedDate) - days(2),  date))

## Remove inconsistent dates
dat <- dat %>% 
  mutate(date = if_else(year(date) != 2020 | date > day_received, mdy(reportedDate) - days(2),  date)) %>%
  filter(year(date) == 2020 & date <= day_received) %>%
  arrange(date)

## We look at this table to decide first and last day
## Often the last days very few tests
## below you can see our choice for this particular dataset
if(FALSE){
  dat %>% 
    group_by(date) %>%
    summarize(n=n()) %>% 
    arrange(date) %>%
    View()
}

first_day <- make_date(2020, 3, 16)
last_day <- make_date(2020, 7, 14)
print(data.frame(first_day= first_day, last_day = last_day))
dat <- filter(dat, date<= last_day & date >= first_day)

tests <- dat %>% 
  filter(result %in% c("positive", "negative")) %>% 
  group_by(date) %>%
  summarize(positives = sum(result == "positive"), tests = n()) %>%
  mutate(rate = positives / tests)

bioportal_data <- dat

save(bioportal_data, tests, file = "rdas/bioportal-data-2020-07-14.rda")

library(tidyverse)
library(readxl)
library(lubridate)

## Read-in sheets
fn <- "data/20_0615-Grafica y Tablas Pruebas Moleculares COVID- semana 15 junio.xlsx"
sheets <- excel_sheets(fn)

## pick sheets that start with a number or space and a number
sheets<- str_subset(sheets, "^\\d|^ \\d")

## covert sheet names to dates
dates <- sheets %>% str_trim %>% str_remove("to") %>% str_split_fixed("\\s+", 2) %>% .[,1] %>%
  str_split_fixed("-", 2)
dates <- make_date(2020, parse_number(dates[,1]), parse_number(dates[,2]))

## read in each sheet 
tabs <- lapply(seq_along(sheets), function(i){
  print(i)
  tab <-read_xlsx(fn, sheets[i], skip = 1)
  names(tab) <- names(tab) %>% str_remove_all("\\d|-|\\(|\\)| to ") %>% str_trim %>% str_replace_all("\\s+", "_") %>% tolower %>%
    str_remove_all("pcr_|by_pcr_") %>% make.unique()
  j <- which(tab$laboratory=="Total")
  tab <- tab %>% 
    slice(1:(j-1)) %>%
    select(c("laboratory", "total_tested_accumulated", "total_pos_accumulated","total_tested_week","total_pos_week")) %>%
    setNames(c("laboratory", "total_tests", "total_cases","new_tests", "new_cases")) %>%
    mutate(total_tests = parse_number(as.character(total_tests)),
           total_cases = parse_number(as.character(total_cases)),
           new_tests = parse_number(as.character(new_tests)), 
           new_cases = parse_number(as.character(new_cases))) %>%
    mutate(date = dates[i], location = "PR") %>% 
    mutate(laboratory = ifelse(laboratory == "Quest*", "Quest", laboratory)) %>%
    mutate(laboratory = ifelse(laboratory == "PRDoH", "PRDH", laboratory))
  return(tab)
})

## Cobmine sheets into one table
dat <- do.call(bind_rows, tabs)

## Repetitions, by hand
## currently not being saved
quest_repeats <- data.frame(
  date = c(make_date(2020, 5, 4), make_date(2020, 5, 11), make_date(2020, 5, 18), make_date(2020, 5, 25), 
           make_date(2020, 6, 1)),
  tests = c(84, 93, 108, 118, 124),
  positives = c(14, 10, 11, 13, 5))

## remove laboratories with just NAs, HRP as it only reports 1000 cases once
## and data from last week 6-29 to 7-5 since the data is incosistent

pr_by_lab <- dat %>% group_by(laboratory) %>%
  mutate(n = sum(!is.na(total_tests))) %>% 
  ungroup() %>%
  filter(n>0 & laboratory != "HRP" & date < max(date)) %>%
  select(-n)

save(pr_by_lab, file = "rdas/pr_by_lab.rda")

pr <- pr_by_lab %>% 
  group_by(date) %>%
  summarize(total_cases = sum(total_cases, na.rm=TRUE), 
            total_tests = sum(total_tests, na.rm=TRUE)) %>%
  mutate(new_cases = c(NA, diff(total_cases)),
         new_tests = c(NA, diff(total_tests)),
         location = "Puerto Rico") %>%
  filter(!is.na(new_cases))

save(pr, file = "rdas/pr.rda")


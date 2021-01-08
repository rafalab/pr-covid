compute_summary <- function(tests, hosp_mort, cases, type = "Molecular", day = today() - days(1)){
  
  ## function to turn proportions into pretty percentages
  make_pct <- function(x, digits = 0) paste0(format(round(100 * x, digits = digits), nsmall = digits), "%")
  
  
  ## dates that we will put in the table
  ## they are 4 entries, 1 week apart
  the_dates <- day - days(lag_to_complete) - weeks(0:3)
  
  ## positivity
  ## we include the latest day because we have a usable value
  ## we take it out later to keep the table dimensions the same
  pos <- filter(tests, testType == type & 
                   date %in% c(the_dates, day)) %>%
    arrange(desc(date))
  
  ## this computes the difference in positivity between weeks
  ##determines if they are significant
  ## and returns -1 (decrease), 0 (no change), 1 (increase)
  change_pos <- sapply(1:(nrow(pos)-1), function(i){
    x <- c(pos$lower[i], pos$upper[i])
    y <- c(pos$lower[i+1], pos$upper[i+1])
    signif <- !any(c(between(x, y[1], y[2])),c(between(y, x[1], x[2])))
    sign(pos$fit[i] - pos$fit[i+1]) * signif 
  })
  
  ## cases 
  ## same a pos but for cases
  cas <- filter(cases, testType == type & 
                   date %in% the_dates)  %>%
    arrange(desc(date))
  
  ## get overdisepersion, last day is a global variable defined in init
  ## we assume cases are Possion with the precalculated trend an offset.
  ## it will be a bit inflated due to the fact that moving average is lagged not centered
  phi <- cases %>% filter(date >= make_date(2020, 7, 1) & 
                            date <= last_day &
                            testType == type) %>%
    mutate(wd = factor(wday(date))) %>%
    glm(cases ~ wd, offset = log(moving_avg), data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  change_cas <- sapply(1:(nrow(cas)-1), function(i){
    d <- cas$moving_avg[i] - cas$moving_avg[i+1]
    se <- sqrt((phi*cas$moving_avg[i] + phi*cas$moving_avg[i+1])/7)
    signif <- abs(d/se) > qnorm(0.975)
    sign(d) * signif 
    })
        
  ## tests
  ##as pos but for nnumber of tests
  tes <- filter(tests, testType == type & 
                   date %in% the_dates) %>%
    arrange(desc(date))
  
  phi <- tests %>% filter(date >= make_date(2020, 7, 1) & date <= last_day &
                            testType == type) %>%
    mutate(wd = factor(wday(date))) %>%
    glm(all_tests ~ wd, offset = log(tests_week_avg), data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  change_tes <- sapply(1:(nrow(tes)-1), function(i){
    d <- tes$tests_week_avg[i] - tes$tests_week_avg[i+1]
    se <- sqrt((phi*tes$tests_week_avg[i] + phi*tes$tests_week_avg[i+1])/7)
    signif <- abs(d/se) > qnorm(0.975)
    sign(d) * signif 
  })
  
  ## Hosp
  ## as pos but for hospitalizations
   hos <- hosp_mort %>% select(date, HospitCOV19, hosp_week_avg) %>% 
    filter(!is.na(HospitCOV19)) %>%
    filter(date <= day) %>%
    filter(date %in% the_dates | date == max(date))  %>%
    arrange(desc(date))
  
  phi <- hosp_mort %>% filter(date >= make_date(2020, 7, 1) & date <= last_day) %>%
    filter(!is.na(HospitCOV19)) %>%
    mutate(wd = factor(wday(date))) %>%
    glm(HospitCOV19 ~ wd, offset = log(hosp_week_avg), data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  change_hos <- sapply(1:(nrow(hos)-1), function(i){
    d <- hos$hosp_week_avg[i] - hos$hosp_week_avg[i+1]
    se <- sqrt((phi*hos$hosp_week_avg[i] + phi*hos$hosp_week_avg[i+1])/7)
    signif <- abs(d/se) > qnorm(0.975)
    sign(d) * signif 
  })
  
  ## Muertes
  ## as pos but for deaths
  mor <- hosp_mort %>% select(date, mort_week_avg) %>% 
    filter(!is.na(mort_week_avg)) %>%
    filter(date %in% the_dates) %>%
    arrange(desc(date))
  
  ## no overdispersion for deaths. Straight poisson:
  change_mor <- sapply(1:(nrow(mor)-1), function(i){
    d <- mor$mort_week_avg[i] - mor$mort_week_avg[i+1]
    se <- sqrt((mor$mort_week_avg[i] + mor$mort_week_avg[i+1])/7)
    signif <- abs(d/se) > qnorm(0.975)
    sign(d) * signif 
  })
  
  
  ## here we decide what recommendation to make
  riesgo <- case_when(pos$fit[1] >= 0.20 | cas$moving_avg[1] >= 800 | hos$HospitCOV19[1] > 1000 ~ 4,
                      pos$fit[1] < 0.03 & cas$moving_avg[1] < 30 & hos$HospitCOV19[1] < 300 &
                        change_pos[1] <= 0 & change_pos[2] <= 0  ~ 1,
                      change_pos[1] > 0  | 
                        ((change_pos[1] >= 0 | change_pos[2] >= 0 | change_pos[3] >= 0) & 
                           pos$fit[1] > 0.05) ~ 3,
                      TRUE ~ 2)
  
  
  ## this is the htlm to make colored arrows:  down is green, sideways is yelloww, up is red (bad)
  arrows <- c( "<span style=\"color:#01D474;font-weight: bold;\">&#8595;</span>",
               "<span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span>", 
               "<span style=\"color:#FF0034;font-weight: bold;\">&#8593;</span>")
  
  ## for test since up is good
  arrows_2 <- c("<span style=\"color:#FF0034;font-weight: bold;\">&#8595;</span>",
              "<span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span>",
              "<span style=\"color:#01D474;font-weight: bold;\">&#8593;</span>")
  
  
  ## make arrow based on change values. +2 because turne -1,0,1 to 1,2,3
  make_arrow <- function(i){
    c(arrows[change_pos[i]+2], 
      arrows[change_cas[i]+2],
      arrows_2[change_tes[i]+2],
      arrows[change_hos[i]+2],
      arrows[change_mor[i]+2])
  }
  
  ## These are the positivity and hospitalizations for today
  ## we remove the first row to have them match the others
  positividad <- paste(make_pct(pos$fit[1], 1),  arrows[change_pos[1] + 2])
  pos <- pos[-1,]
  change_pos <- change_pos[-1]
  
  hosp <- paste(prettyNum(hos$HospitCOV19[1], big.mark = ","), arrows[change_hos[1]+1])
  hos <- hos[-1,]
  change_hos <- change_hos[-1]
  
  ## make the table
  tab <- tibble(metrica = c("Tasa de positividad", 
                            "Casos nuevos por día", 
                            "Pruebas por día", 
                            "Hospitalizaciones",
                            "Muertes por día"),
                
                valor =  c(make_pct(pos$fit[1]), 
                           round(cas$moving_avg[1]), 
                           prettyNum(round(tes$tests_week_avg[1]), 
                                     big.mark = ","),
                           prettyNum(round(hos$HospitCOV19[1]), 
                                     big.mark = ","),
                           round(mor$mort_week_avg[1])),
                meta = c("< 3%", 
                         "< 30", 
                         "> 4,500", 
                         "< 300",
                         "< 1"),
                cambio_1 = make_arrow(1),
                cambio_2 = make_arrow(2),
                cambio_3 = make_arrow(3)
  )
  
  colnames(tab) <- c("Métrica", "Nivel",  "Meta", "Cambio en 7 días", "Cambio anterio", "Cambio hace 14 días")
  
  return(list(tab = tab, riesgo = riesgo, positividad = positividad, hosp = hosp))
  
}

compute_summary <- function(tests, hosp_mort, cases, type = "Molecular", day = today() - days(1)){
  
  ## function to turn proportions into pretty percentages
  make_pct <- function(x, digits = 1) paste0(format(round(100 * x, digits = digits), nsmall = digits), "%")
  
  
  ## dates that we will put in the table
  ## they are 4 entries, 1 week apart
  ## lag_to_complete is a global var
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
    
    signif <- !any(c(between(x, y[1], y[2])), 
                   c(between(y, x[1], x[2])))
    
    sign(pos$fit[i] - pos$fit[i+1]) * signif 
  })
  
  ## cases 
  ## same a pos but for cases
  cas <- filter(cases, testType == type & 
                   date %in% the_dates)  %>%
    arrange(desc(date))
  
  ## get overdisepersion, last day is a global variable defined in init
  ## we assume cases are Possion with the precalculated trend an offset.
  phi <- cases %>% filter(date >= make_date(2020, 7, 1) & 
                            date <= make_date(2020, 11, 2) & ## avoid election thanksgiving and xmas
                            date <= last_day &
                            testType == type) %>%
    mutate(wd = factor(wday(date)), week = factor(round_date(date, "week"))) %>%
    glm(cases ~ wd + week, data = ., family = quasipoisson) %>%
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
  
  phi <- tests %>% filter(date >= make_date(2020, 7, 1) & 
                            date <= make_date(2020, 11, 2) & ## avoid elections, thanksgiving and xmas
                            date <= last_day &
                            testType == type) %>%
    mutate(wd = factor(wday(date)), week = factor(round_date(date, "week"))) %>%
    glm(all_tests ~ wd + week, data = ., family = quasipoisson) %>%
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
  
  
  tendencia <- case_when(change_pos[1] == 1 ~ 1,
                         change_pos[1] == -1 & change_pos[2] == -1 & change_pos[3] == -1 ~ -1,
                         TRUE ~ 0)
  
  nivel <- case_when(pos$fit[1] >= 0.20 | cas$moving_avg[1] >= 800 | hos$HospitCOV19[1] > 1000 ~ 4,
                     pos$fit[1] < 0.03 & cas$moving_avg[1] < 30 & hos$HospitCOV19[1] < 300 ~ 1,
                     pos$fit[2] >= 0.05 ~ 3,
                     TRUE ~ 2)
                     
  ## here we decide what recommendation to make
  riesgo <- case_when(nivel == 4 ~ 4,
                      nivel == 1 & tendencia == -1 ~ 1,
                      tendencia == 1 | (nivel == 3 & tendencia == 0) ~ 3,
                      TRUE ~ 2)
  
  ## this is the htlm to make colored arrows:  down is green, sideways is yelloww, up is red (bad)
  arrows <- c( "<span style=\"color:#01D474;font-weight: bold;\">&#8595;</span>",
               "<span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span>", 
               "<span style=\"color:#FF0034;font-weight: bold;\">&#8593;</span>")
  
  ## for test since up is good
  arrows_2 <- c("<span style=\"color:#FF0034;font-weight: bold;\">&#8595;</span>",
              "<span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span>",
              "<span style=\"color:#01D474;font-weight: bold;\">&#8593;</span>")
  
  no_arrow <- "<span style=\"color:#ffffff00;font-weight: bold;\">&#8596;</span>"
  
  ## make arrow based on change values. +2 because turne -1,0,1 to 1,2,3
  make_arrow <- function(i){
    replace_na(
      c(arrows[change_pos[i]+2], 
        arrows[change_cas[i]+2],
        arrows_2[change_tes[i]+2],
        arrows[change_hos[i]+2],
        arrows[change_mor[i]+2]),
     no_arrow)
  }
  
  make_values <- function(i){
    c(make_pct(pos$fit[i]), 
      round(cas$moving_avg[i]), 
      prettyNum(round(tes$tests_week_avg[i]), 
                big.mark = ","),
      prettyNum(round(hos$HospitCOV19[i]), 
                big.mark = ","),
      round(mor$mort_week_avg[i]))
  }
  
  ## These are the positivity and hospitalizations for today
  ## we remove the first row to have them match the others
  positividad <- paste(make_pct(pos$fit[1]),  arrows[change_pos[1] + 2])
  pos <- pos[-1,]
  change_pos <- change_pos[-1]
  
  hosp <- paste(prettyNum(hos$HospitCOV19[1], big.mark = ","), arrows[change_hos[1]+2])
  hos <- hos[-1,]
  change_hos <- change_hos[-1]
  
  ## make the table
  tab <- tibble(metrica = c("Tasa de positividad", 
                            "Casos nuevos por día", 
                            "Pruebas por día", 
                            "Hospitalizaciones",
                            "Muertes por día"),
                
                valor =  paste(make_values(1), make_arrow(1)),
                meta = c("< 3.0%", 
                         "< 30", 
                         "> 4,500", 
                         "< 300",
                         "< 1"),
                cambio_1 = paste(make_values(2), make_arrow(2)),
                cambio_2 = paste(make_values(3), make_arrow(3)),
  )
  
  colnames(tab) <- c("Métrica", "Nivel",  "Meta", "7 días antes",  "14 días antes")
  
  return(list(tab = tab, riesgo = riesgo, nivel = nivel, tendencia = tendencia, 
              positividad = positividad, hosp = hosp))
  
}

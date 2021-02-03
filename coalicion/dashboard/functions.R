compute_summary <- function(tests, hosp_mort, type = "Molecular", day = last_complete_day){
  
  ## function to turn proportions into pretty percentages
  make_pct <- function(x, digits = 1){
    ifelse(is.na(x), "", paste0(format(round(100 * x, digits = digits), nsmall = digits), "%"))
  }
  
  ## dates that we will put in the table
  ## they are 4 entries, 1 week apart
  ## lag_to_complete is a global var
  the_dates <- day - days(lag_to_complete) - weeks(0:3)
  
  ## positivity
  ## we include the latest day because we have a usable value
  ## we take it out later to keep the table dimensions the same
  pos <- filter(tests, testType == type & 
                  date %in% the_dates) %>%
    arrange(desc(date))
  
  latest <- tests %>% filter(date <= day & date > max(the_dates) & testType == type) %>%
    arrange(desc(date)) %>% slice(1)
  
  if(nrow(latest) > 0) pos <- bind_rows(latest, pos) else bind_rows(slice(pos, 1), pos)
  
  ## this computes the difference in positivity between weeks
  ##determines if they are significant
  ## and returns -1 (decrease), 0 (no change), 1 (increase)
  change_pos <- sapply(1:(nrow(pos)-1), function(i){
    p1 <- pos$fit[i] 
    p0 <- pos$fit[i+1]
    d <- p1 - p0
    se <- sqrt(p1*(1-p1) / pos$people_total_week[i] + p0*(1-p0) / pos$people_total_week[i+1])
    signif <- abs(d/se) > qnorm(0.975)
    sign(pos$fit[i] - pos$fit[i+1]) * signif 
  })
  
  casespos <- pos %>%
    mutate(n =  people_total_week - people_positives_week + cases_week_avg * 7,
           cases_rate = cases_week_avg * 7 / n)
  
  change_casespos <- sapply(1:(nrow(casespos)-1), function(i){
    p1 <- casespos$cases_rate[i] 
    p0 <- casespos$cases_rate[i+1]
    d <- p1 - p0
    se <- sqrt(p1*(1-p1) / casespos$n[i] + p0*(1-p0) / casespos$n[i+1])
    signif <- abs(d/se) > qnorm(0.975)
    sign(casespos$cases_rate[i] - casespos$cases_rate[i+1]) * signif 
  })
  
  ## cases 
  ## same a pos but for cases
  cas <- filter(tests, testType == type & 
                  date %in% the_dates)  %>%
    arrange(desc(date))
  
  ## get overdisepersion, last day is a global variable defined in init
  ## we assume cases are Possion with the precalculated trend an offset.
  phi <- tests %>% filter(date >= make_date(2020, 7, 1) & 
                            date <= make_date(2020, 11, 2) & ## avoid election thanksgiving and xmas
                            date <= last_day &
                            testType == type) %>%
    mutate(wd = factor(wday(date)), week = factor(round_date(date, "week"))) %>%
    glm(cases ~ wd + week, data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  change_cas <- sapply(1:(nrow(cas)-1), function(i){
    d <- cas$cases_week_avg[i] - cas$cases_week_avg[i+1]
    se <- sqrt((phi*cas$cases_week_avg[i] + phi*cas$cases_week_avg[i+1])/7)
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
    glm(people_total_week ~ wd + week, data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  change_tes <- sapply(1:(nrow(tes)-1), function(i){
    d <- (tes$people_total_week[i] - tes$people_total_week[i+1]) / 7
    se <- sqrt((phi*tes$people_total_week[i] + phi*tes$people_total_week[i+1]))/7
    signif <- abs(d/se) > qnorm(0.975)
    sign(d) * signif 
  })
  
  ## Hosp
  ## as pos but for hospitalizations
  hos <- hosp_mort %>% select(date, HospitCOV19, hosp_week_avg) %>% 
    filter() %>%
    filter(date <= day) %>%
    filter(date %in% the_dates)  %>%
    arrange(desc(date))
  
  latest <- hosp_mort %>% select(date, HospitCOV19, hosp_week_avg) %>% 
    filter(!is.na(HospitCOV19) & date <= day & date > max(the_dates)) %>%
    arrange(desc(date)) %>% slice(1)
  
  if(nrow(latest) > 0) hos <- bind_rows(latest, hos) else hos <- bind_rows(slice(hos, 1), hos)
  
  hosp_fit <- hosp_mort %>% filter(date >= make_date(2020, 7, 1) & date <= last_day) %>%
    filter(!is.na(HospitCOV19)) %>%
    mutate(wd = factor(wday(date))) %>%
    glm(HospitCOV19 ~ wd, offset = log(hosp_week_avg), data = ., family = quasipoisson) 
  
  phi <- hosp_fit %>%
    summary()  %>%
    .$dispersion
  
  ##becuase data is so correlated we have to compute correlation matrix to compute SE
  hosp_corrs <- acf(hosp_fit$resid)$acf[1:14, 1, 1]
  ## this is the transformation needed to compute difference of averages
  X <- matrix(rep(c(1/7, -1/7), each = 7))
  
  change_hos <- sapply(1:(nrow(hos)-1), function(i){
    d <- hos$hosp_week_avg[i] - hos$hosp_week_avg[i+1]
    
    ## compute the SE of the difference of averages given correlation
    Sigma <- matrix(0, 14, 14)
    ## these are the estimated standard deviations from Poisson assumption
    s<- sqrt(c(rep(phi*hos$hosp_week_avg[i], 7), rep(phi*hos$hosp_week_avg[i+1], 7)))
    for(j in 1:14) for(k in 1:14) Sigma[j, k] = hosp_corrs[abs(j-k)+1]*s[j]*s[k]
    se <- sqrt(t(X) %*% Sigma %*% X)
    
    ## because correlation is positive, the SE can't be smaller than the one obtined assuming IID
    se <- pmax(se, sqrt((phi*hos$hosp_week_avg[i] + phi*hos$hosp_week_avg[i+1])/7))
  
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
  
  ## Vacunas
  vac <- hosp_mort %>% select(date,  people_fully_vaccinated) %>% 
    filter(!is.na(people_fully_vaccinated)) %>%
    filter(date %in% the_dates) %>%
    arrange(desc(date)) 
  
  latest <- hosp_mort %>% select(date,  people_fully_vaccinated) %>% 
    filter(!is.na(people_fully_vaccinated) & date <= day & date > max(the_dates)) %>%
    arrange(desc(date)) %>% 
    slice(1)
  
  if(nrow(latest) > 0) vac <- bind_rows(latest, vac) else vac <- bind_rows(slice(vac, 1), vac)
  
  vac <- vac %>%
    mutate(pct_fully_vaccinated = people_fully_vaccinated/pr_pop)
  
  ### Definir tendencia y recomendación automatica
  tendencia <- case_when(change_pos[1] == 1 ~ 1,
                           change_pos[1] == -1 & change_pos[2] == -1 & change_pos[3] == -1 ~ -1,
                           TRUE ~ 0)
  
  nivel <- case_when(pos$fit[1] >= 0.20 | cas$cases_week_avg[1] >= 800 | hos$HospitCOV19[1] > 1000 ~ 4,
                     pos$fit[1] < 0.03 & cas$cases_week_avg[1] < 30 & hos$HospitCOV19[1] < 300 ~ 1,
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
        arrows[change_casespos[i]+2],
        arrows[change_cas[i]+2],
        arrows_2[change_tes[i]+2],
        arrows[change_hos[i]+2],
        arrows[change_mor[i]+2],
        NA),
     no_arrow)
  }
  
  make_values <- function(i){
    c(make_pct(pos$fit[i]), 
      make_pct(casespos$cases_rate[i]), 
      round(cas$cases_week_avg[i]), 
      prettyNum(round(tes$people_total_week[i] / 7), 
                big.mark = ","),
      prettyNum(round(hos$HospitCOV19[i]), 
                big.mark = ","),
      round(mor$mort_week_avg[i]),
      make_pct(vac$pct_fully_vaccinated[i]))
  }
  
  ## These are the positivity and hospitalizations for today
  ## we remove the first row to have them match the others
  positividad <- paste(make_pct(pos$fit[1]),  arrows[change_pos[1] + 2])
  pos <- slice(pos, -1)
  change_pos <- change_pos[-1]
  
  casos_positividad <- paste(make_pct(casespos$cases_rate[1]),  arrows[change_casespos[1] + 2])
  casespos <- slice(casespos, -1)
  change_casespos <- change_casespos[-1]
  
  hosp <- paste(prettyNum(hos$HospitCOV19[1], big.mark = ","), arrows[change_hos[1]+2])
  hos <- slice(hos, -1)
  change_hos <- change_hos[-1]
  
  vacunas <- paste(make_pct(vac$pct_fully_vaccinated[1]),  no_arrow)
  vac <- slice(vac, -1)
  
  vacs_per_day <- diff(vac$people_fully_vaccinated[1:2])/diff(as.numeric(vac$date[1:2]))
  dias_hasta_meta_vacunas <- paste(
    prettyNum(round((pr_pop*0.7 - vac$people_fully_vaccinated[1]) / vacs_per_day), big.mark = ","),
    no_arrow)

  if(type == "Molecular"){
    meta <- c("< 3.0%", "< 2.0%",  "< 30",  "> 4,500", "< 300", "< 1", "> 70%")
  } else {
    meta <- c("", "",  "",  "",  "< 300", "< 1", "> 70%")
  }
  ## make the table
  tab <- tibble(metrica = c("% pruebas positivas", 
                            "% casos nuevos",
                            "Casos nuevos por día", 
                            "Pruebas por día", 
                            "Hospitalizaciones",
                            "Muertes por día",
                            "% población vacunada"),
                
                meta = meta,
                
                 valor =  paste(make_values(1), make_arrow(1)),
                 
                cambio_1 = paste(make_values(2), make_arrow(2)),
                cambio_2 = paste(make_values(3), make_arrow(3)),
  )
  
  colnames(tab) <- c("Métrica", 
                     "Meta", 
                     paste0(format(pos$date[1]-days(6), "%b%d-"),format(pos$date[1], "%b%d")),
                     paste0(format(pos$date[2]-days(6), "%b%d-"),format(pos$date[2], "%b%d")),
                     paste0(format(pos$date[3]-days(6), "%b%d-"),format(pos$date[3], "%b%d")))
                     
                     
  return(list(tab = tab, riesgo = riesgo, nivel = nivel, tendencia = tendencia, 
              positividad = positividad, casos_positividad = casos_positividad, 
              hosp = hosp, vacunas = vacunas,
              dias_hasta_meta_vacunas = dias_hasta_meta_vacunas))
  
}

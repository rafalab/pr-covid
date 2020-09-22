# fit glm spline ----------------------------------------------------------
# no longer used. we now use moving average to match other dashboards
spline_fit <- function(d, y, n = NULL, 
                       week_effect = TRUE, 
                       knots_per_month = 2, 
                       family = quasibinomial, 
                       alpha = 0.05){
  
  z <- qnorm(1 - alpha/2)
  
  x <- as.numeric(d)
  
  df  <- round(knots_per_month * length(x) / 30) + 1
  
  if(family()$family %in% c("binomial", "quasibinomial")){
    if(is.null(n)) stop("Must supply n with binomial or quasibinomial")
    y <- cbind(y, n-y)
  }
  
  if(week_effect){
    
    w <- factor(wday(d))
    contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
    w <- model.matrix(~w)[,-1]
    
    glm_fit  <- glm(y ~ ns(x, df = df, intercept = TRUE) + w - 1, family = family)
  
  } else {
    
    glm_fit  <- glm(y ~ ns(x, df = df, intercept = TRUE) - 1, family = family)
  
  }
  
  glm_pred <- predict(glm_fit, type = "terms", se.fit = TRUE)
  
  fit <- family()$linkinv(glm_pred$fit[,1])

  lower <- family()$linkinv(glm_pred$fit[,1] - z * glm_pred$se.fit[,1])
  
  upper <- family()$linkinv(glm_pred$fit[,1] + z * glm_pred$se.fit[,1])
 
  return(tibble(date = d, fit = fit, lower = lower, upper = upper))  
}


# moving average ----------------------------------------------------------

ma7 <- function(d, y, k = 7) 
  tibble(date = d, moving_avg = as.numeric(stats::filter(y, rep(1/k, k), side = 1)))

plot_positivity <- function(tests, 
                            start_date = first_day, 
                            end_date = last_day, 
                            type = "Molecular", 
                            yscale = FALSE){
  ret <- tests %>%
    filter(testType == type &
           date >= start_date & date <= end_date) %>%
    ggplot(aes(date, rate)) +
    geom_hline(yintercept = 0.03, lty=2, color = "gray") +
    geom_hline(yintercept = 0.10, lty=2, color = "gray") +
    geom_hline(yintercept = 0.20, lty=2, color = "gray") +
    annotate("text", end_date + days(2), 0.015, label = "Bajo") + #, color = "#01D474") +
    annotate("text", end_date + days(2), 0.065, label = "Medio") + #, color = "#FFC900") +
    annotate("text", end_date + days(2), 0.15, label = "Alto") + #, color = "#FF9600") +
    annotate("text", end_date + days(2), 0.225, label = "Crítico") + #, color = "#FF0034") +
    geom_point(aes(date, rate), size=2, alpha = 0.65) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.35) +
    geom_line(aes(y = fit), color="blue2", size = 0.80) +
    ylab("Tasa de positividad") +
    xlab("Fecha") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_labels = "%B %d")  +
    labs(title = paste("Tasa de Positividad basada en pruebas" , 
                       ifelse(type=="Molecular", "moleculares", "serológicas"))) +
    theme_bw() +
    theme(plot.caption=element_text(hjust = 0))
  
  if(yscale){
    ret <- ret + coord_cartesian(ylim = c(0, 0.25))
  } 
  return(ret)
}


# ICU usage ---------------------------------------------------------------
plot_icu <- function(hosp_mort,  
                      start_date = first_day, 
                      end_date = last_day, 
                      yscale = FALSE){
  
  tmp <- hosp_mort %>% 
    filter(!is.na(CamasICU)) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(icu = CamasICU / (CamasICU + CamasICU_disp))
  
  if(yscale){
    lim  <- c(0,1)
  } else
  {
    lim <- c(min(tmp$icu, na.rm = TRUE),
             pmax(max(tmp$icu, na.rm = TRUE), 
                  1/max(tmp$CamasICU + tmp$CamasICU_disp, na.rm=TRUE)))
  }
  min_date <- min(tmp$date)
  
  ret <- tmp %>% 
    ggplot(aes(date, icu)) +
    geom_hline(yintercept = 0.50, lty=2, color = "gray") +
    geom_hline(yintercept = 0.60, lty=2, color = "gray") +
    geom_hline(yintercept = 0.70, lty=2, color = "gray") +
    geom_hline(yintercept = 1.00, lty=2, color = "gray") +
    annotate("text", end_date + days(2), 0.25, label = "Bajo") + #, color = "#01D474") +
    annotate("text", end_date + days(2), 0.55, label = "Medio") + #, color = "#FFC900") +
    annotate("text", end_date + days(2), 0.65, label = "Alto") + #, color = "#FF9600") +
    annotate("text", end_date + days(2), 0.75, label = "Crítico") + #, color = "#FF0034") +
    geom_line(lwd = 1.5, color = "darkblue") +
    xlab("Fecha") +
    ylab("Porciento") +
    labs(title = "Camas ICU disponibles usadas por pacientes COVID-19",
         caption = "Algunas pacientes en el ICU están ahí por otras causas.\nLa gráfica muestra el porcentaje de las camas restantes ocupadas por pacientes COVID-19.") +
    scale_x_date(date_labels = "%B %d") +
    scale_y_continuous(limits = lim, labels = scales::percent) +
    theme_bw() 
  
  return(ret)
}


# Deaths ------------------------------------------------------------------

plot_deaths <- function(hosp_mort,  
                        start_date = first_day, 
                        end_date = last_day, 
                        cumm = FALSE){
  if(cumm){
    hosp_mort %>%
      replace_na(list(IncMueSalud = 0)) %>%
      mutate(IncMueSalud = cumsum(IncMueSalud)) %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date)) +
      geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
      ylab("Muertes acumuladas") +
      xlab("Fecha") +
      ggtitle("Muertes acumuladas por COVID-19") +
      scale_x_date(date_labels = "%B %d") +
      theme_bw()
  } else{
    hosp_mort %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date)) +
      geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
      geom_line(aes(y = fit), color="black", size = 1.25) +
      ylab("Muertes") +
      xlab("Fecha") +
      ggtitle("Muertes por COVID-19") +
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(breaks = seq(0, 15, 1)) +
      theme_bw()
  }
}
  
plot_hosp <- function(hosp_mort,  
                      start_date = first_day, 
                      end_date = last_day){
  
  tmp <- hosp_mort %>% 
    filter(!is.na(HospitCOV19) & 
             date >= start_date & date <= end_date) %>% 
    select(date, HospitCOV19, CamasICU) 
  
  ret <- tmp %>% 
    ggplot(aes(x = date)) +
    geom_bar(mapping = aes(y = HospitCOV19), stat = "identity", width = 0.75, fill = "#8CC8F4") +
    geom_bar(mapping = aes(y = CamasICU), stat = "identity", width = 0.75, fill = "darkblue") +
    xlab("Fecha") +
    ylab("Pacientes") +
    ggtitle("Hospitalizaciones y ICU") +
    scale_x_date(date_labels = "%B %d") +
    theme_bw() 
    
  return(ret)
  
}

plot_cases <- function(cases, 
                       start_date = first_day, 
                       end_date = last_day, 
                       type = "Molecular", 
                       cumm = FALSE){
  if(cumm){
    cases %>% 
      filter(testType == type) %>%
      mutate(cases = cumsum(cases)) %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date, cases)) +
      geom_bar(stat = "identity", fill = "#FBBCB2", width= 0.75) +
      ggtitle(paste0("Casos acumulados basado en pruebas ", 
                     ifelse(type=="Molecular", "moleculares", "serológicas"))) +
      ylab("Casos") +
      xlab("Fecha") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%B %d") +
      theme_bw()
  } else{
    cases %>%
      filter(testType == type & date >= start_date & date <= end_date) %>%
      ggplot(aes(date, cases)) +
      geom_bar(stat = "identity", fill = "#FBBCB2", width= 0.75) +
      geom_line(aes(y = moving_avg), color = "#CC523A", size = 1.25) +
      ylab("Casos únicos") +
      xlab("Fecha") +
      ggtitle(paste0("Casos únicos basado en pruebas ", 
                    ifelse(type=="Molecular", "moleculares", "serológicas"))) +
      
      scale_x_date(date_labels = "%B %d") +
      theme_bw()
  }
}

plot_test <- function(tests, 
                      start_date = first_day, 
                      end_date = last_day, 
                      type = "Molecular", 
                      cumm = FALSE){
  if(cumm){
    tests %>%
      filter(testType == type) %>%
      mutate(tests = cumsum(all_tests)) %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date, tests)) +
      geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
      ylab("Pruebas acumuladas") +
      xlab("Fecha") +
      labs(title = paste("Total de pruebas", ifelse(type == "Molecular", "moleculares", "serológicas")),
           caption = "Incluye pruebas duplicadas.") + 
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() 
  } else{
    tests %>%
      filter(testType == type & date >= start_date & date <= end_date) %>%
      ggplot(aes(date, all_tests)) +
      geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
      geom_step(aes(y = tests_week_avg), color = "#31347A", size = 1.25) +
      ylab("Pruebas") +
      xlab("Fecha") +
      labs(title = paste("Pruebas", ifelse(type=="Molecular", "moleculares", "serológicas"), "por día"),
           caption = "Incluye pruebas duplicadas.") + 
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
  }    
}

plot_map <- function(test_by_strata,
                     start_date = first_day, 
                     end_date = last_day, 
                     type = "Molecular",
                     max_rate = 0.25){
    
  ret <- tests_by_strata %>%
    filter(date >= start_date &  date <= end_date & testType == type) %>%
    group_by(testType, date, patientCity) %>%
    summarize(positives = sum(positives),
              tests     = sum(tests), .groups = "drop") %>%
    ungroup() %>%
    group_by(patientCity) %>%
    summarize(positives  = sum(positives),
              tests      = sum(tests),
              rate       = positives / tests, groups = "drop") %>%
    ungroup() %>%
    mutate(rate = 100*pmin(max_rate, rate)) %>%
    na.omit() %>%
      {merge(map, .,by.x = "ADM1_ES", by.y = "patientCity", all.y = T)} %>%
      ggplot() +
      geom_sf(data = map, fill= "gray", size = 0.15) +
      geom_sf(aes(fill = rate), color = "black", size = 0.15) +
      geom_text(data = map, aes(X, Y, label = ADM1_ES),
                size  = 2.2,
                color = "black",
                fontface = "bold") +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                           name = "Tasa de Positividad:",
                           limits= c(0, 100*max_rate)) +
      theme_void() +
      theme(legend.position = "bottom") +
      ggtitle(paste("Tasa de positividad por municipio para el period de",
                    format(start_date, "%B %d"),
                    "a",
                    format(end_date, "%B %d."), "\n",
                    paste("Pruebs",  ifelse(type=="Molecular", "moleculares", "serológicas"))))
                
  return(ret)
}

make_table <- function(test, cases, hosp_mort, 
                       start_date = first_day, 
                       end_date = last_day, 
                       type = "Molecular"){
  
  tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU)

  cases <- filter(cases, testType == type)
  
  #cases$moving_avg[cases$date > last_day] <- NA

  ret <- tests %>%
    filter(testType == type) %>%
    left_join(cases, by = "date") %>%
    left_join(tmp, by = "date") %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(rate = format(round(rate, 2), nsmall = 2),
           fit = ifelse(is.na(fit), "", 
                        paste0(format(round(100*fit, 1), nsmall = 1), "% ",
                               "(", trimws(format(round(100*lower, 1), nsmall = 1)),"%" , ", ",
                               format(round(100*upper, 1), nsmall=1),"%", ")")), 
           moving_avg = round(moving_avg),
           dummy = date) %>%
    select(date, fit, cases, moving_avg, IncMueSalud, CamasICU, HospitCOV19, 
           positives, tests, rate, dummy) %>%
    arrange(desc(date)) %>%
    mutate(date = format(date, "%B %d")) %>%
    setNames(c("Fecha", "Tasa de positividad (IC)",  "Casos únicos", "Promedio de 7 días",
               "Muertes", "ICU", "Hospitali- zaciones", "Positivos", "Pruebas", 
               "Positivos/ Pruebas", "dateorder"))
  
      ret <- DT::datatable(ret, #class = 'white-space: nowrap',
                    caption = paste("Positivos y casos son de pruebas", 
                                    ifelse(type=="Molecular", "moleculares.", "serológicas."), 
                                    "La columna con fechas contiene el día en que se hizo la prueba.", 
                                    "La tasa de positividad se define como el número de personas con al menos una prueba positiva dividido por el número de personas que se han hecho la prueba.",
                                    "El estimado para cada día está basado en las pruebas hecha durante la semana acabando en ese día.",
                                    "IC = Intervalo de confianza del ", (1-alpha)*100,"%.",
                                    "Los casos único son el número de personas con su primera prueba positiva en ese día.",
                                    "El promedio de casos de 7 días está basado en la semana acabando ese día.",
                                    "Los columna de positivos muestra el número de personas que tuvieron una prueba positiva ese día (no necesariamente son casos únicos).",
                                    "La columna de pruebas es el número de personas que se hicieron una prueba ese día.",
                                    "Tengan en cuante que los fines de semana se hacen menos pruebas y por lo tanto se reportan menos casos.",
                                    "Las muertes, casos en el ICU y hospitalizaciones vienen del informe oficial del Departamento de Salud."),
      rownames = FALSE,
      options = list(dom = 't', pageLength = -1,
                     columnDefs = list(
                       list(targets = 0, orderData = 10),
                       list(targets = 10, visible = FALSE),
                       list(className = 'dt-right', targets = 2:9)))) %>%
        DT::formatStyle(1:2,"white-space"="nowrap")
      return(ret)
}


make_municipio_table <- function(test_by_strata, 
                                 start_date = first_day, 
                                 end_date = last_day, 
                                 type = "Molecular"){
  
    tmp <- tests_by_strata %>%
      filter(date >= start_date &  date <= end_date & testType == type) %>%
      mutate(patientCity = as.character(patientCity))
 
    children <- tmp %>%
      filter(as.numeric(ageRange) <= 2) %>%
      group_by(patientCity, ageRange) %>%
      summarize(value = sum(positives)) %>%
      ungroup() %>%
      spread(ageRange, value)
 
    ndays <- as.numeric(end_date - start_date) + 1
 
    ret <- tmp %>%
      group_by(patientCity) %>%
      summarize(positives = sum(positives), tests = sum(tests),
                rate =  positives/tests) %>%
      ungroup() %>%
      left_join(children, by = "patientCity") %>%
      left_join(poblacion_municipios, by = "patientCity") %>%
      mutate(lower = qbinom(alpha/2, tests, rate) / tests,
             upper = qbinom(1 - alpha/2, tests, rate) / tests,
             ppc = round(positives/poblacion * 100000 / ndays, 1)) %>%
      arrange(desc(rate)) %>%
      mutate(rate = paste0(format(round(100*rate,1), nsmall=1),"% ", "(",
                           format(round(100*lower, 1), nsmall=1),"%", ", ",
                           format(round(100*upper, 1), nsmall=1),"%", ")"),
             poblacion_text = prettyNum(poblacion, big.mark=",")) %>%
      select(patientCity, rate, positives, tests, ppc, poblacion_text, `0 to 9`, `10 to 19`, poblacion) %>%
      setNames(c("Municipio", "Tasa de positividad (IC)", "Positivos", "Pruebas",  
                 "Positivos por\n100,000 por día", "Población", "Positiovs 0 a 9 años", "Positivos 10 a 19 años", "dummy"))
 
    ret <- DT::datatable(ret, #class = 'white-space: nowrap',
                         caption = paste0("Por razones de privacidad, estos estimados de positividad están basado en datos sin remover duplicados.",
                                          " Tasa de positividad es un estimado basado en periodo ",
                                          format(start_date, "%B %d"),
                                          " a ",
                                          format(end_date, "%B %d"),
                                          ". IC = Intervalo de confianza del ", (1-alpha)*100,"%."),
                         rownames = FALSE,
                         options = list(dom = 't', pageLength = -1,
                                        columnDefs = list(
                                          list(targets = 5, orderData = 8),
                                          list(targets = 8, visible = FALSE),
                                          list(className = 'dt-right', targets = 2:7)))) %>%
      DT::formatStyle(1:2,"white-space"="nowrap")
 
    return(ret)
}

plot_agedist <- function(tests_by_strata,
                         start_date = first_day, 
                         end_date = last_day, 
                         type = "Molecular",
                         yscale = FALSE){
  
  age_levels <- levels(tests_by_strata$ageRange)
  ret <- tests_by_strata %>%
    filter(testType == type & date >= start_date & date <= end_date) %>%
    filter(ageRange != "No reportado") %>%
    mutate(ageRange = age_levels[ifelse(as.numeric(ageRange) >= 9, 9, as.numeric(ageRange))]) %>%
    mutate(ageRange = ifelse(ageRange == "80 to 89", "80+", ageRange)) %>%
    group_by(ageRange) %>%
    summarize(positives = sum(positives), .groups = "drop") %>%
    ungroup() %>%
    mutate(percent = positives/sum(positives)) %>%
    ggplot(aes(ageRange, percent)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    xlab("Edad") +
    ylab("Porciento") +
    ggtitle(paste("Distribución de pruebas",  ifelse(type=="Molecular", "moleculares", "serológicas"), "positivas por edad",
                  format(start_date, "%B %d"),
                  "a",
                  format(end_date, "%B %d."))) +
    theme_bw()
  
  if(yscale) ret <- ret+ coord_cartesian(ylim = c(0, 0.23))
  
  return(ret)
}

compute_summary <- function(tests, hosp_mort, cases){
  
  make_pct <- function(x) paste0(round(100 * x), "%")
  delta <- function(x){
    paste0(ifelse(x[2] - x[1]>0, "+", ""),
           paste0(round(100*(x[2] - x[1]) / x[1]), "%"))
  }

  tmp1 <- filter(tests, testType == "Molecular" & date %in% c(last_day, last_day - weeks(1))) %>%
    arrange(date)
  
  tmp2 <- hosp_mort %>% select(date, CamasICU, CamasICU_disp) %>% 
    filter(!is.na(CamasICU)) %>%
    mutate(camasICU = CamasICU / (CamasICU + CamasICU_disp)) %>%
    filter(date %in% c(max(date), max(date) - weeks(1))) %>%
    arrange(date)
  
  tmp3 <- filter(cases, testType == "Molecular" & date %in% c(last_day, last_day - weeks(1)))  %>%
    arrange(date)
  
  
  ret <- tibble(metrica = c("Tasa de positividad", "Uso de camas ICU", "Casos nuevos por día", "Pruebas por día"),
                meta = c("Menos de 3%", "Menos de 50%", "Menos de 30", ""),
                valor =  c(make_pct(tmp1$fit[2]), make_pct(tmp2$camasICU[2]), round(tmp3$moving_avg[2]), prettyNum(round(tmp1$tests_week_avg[2]), big.mark = ",")),
                cambio = c(delta(tmp1$fit), delta(tmp2$CamasICU), delta(tmp3$moving_avg), delta(tmp1$tests_week_avg)))

  colnames(ret) <- c("Métrica", "Meta", "Nivel actual", "Tendencia")
  
  ret <- DT::datatable(ret, class = 'white-space: nowrap',
                       rownames = FALSE,
                       options = list(dom = 't', ordering = FALSE, pageLength = -1, 
                                      columnDefs = list(
                                        list(className = 'dt-center', targets = 0:3)))) %>%
    DT::formatStyle(columns = 1:4, fontSize = '125%')
  
  
  ret
  return(ret)
  
}

plot_rezago <- function(rezago,
                        start_date = first_day, 
                        end_date = last_day, 
                        type = "Molecular"){
  
  rezago %>%
    filter(date >= start_date &  date <= end_date & testType == type) %>%
    filter(diff<20 & diff>=0) %>%
    ggplot(aes(x=diff, color = Resultado)) +
    stat_ecdf() + 
    xlab("Días") + 
    ylab("Porciento de pruebas") +
    ggtitle("Rezago entre toma de muestra y día en que se reporta") +
    scale_y_continuous(labels=scales::percent) +
    xlim(0,20) +
    theme_bw()
}
  
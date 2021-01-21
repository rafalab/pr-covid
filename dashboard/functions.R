# positivity plot ---------------------------------------------------------
plot_positivity <- function(tests, 
                            start_date = first_day, 
                            end_date = today(), 
                            type = "Molecular", 
                            yscale = FALSE,
                            version = c("pruebas", "casos")){
  version <- match.arg(version)
  
  dat <- tests %>%
    filter(testType == type &
           date >= start_date & date <= end_date) 
  
  if(version == "casos"){
    dat <- dat %>%
      mutate(n = people_total_week - people_positives_week + cases_week_avg * 7,
             fit =cases_week_avg * 7 / n,
             rate = cases / (people_total - people_positives + cases),
             lower = qbinom(0.025, n, fit) / n,
             upper = qbinom(0.975, n, fit) / n)
    the_title <- paste("% de personas que se hicieron prueba\n" , 
                       case_when(type == "Molecular" ~ "molecular", 
                                 type == "Serological" ~ "serológica",
                                 type == "Antigens" ~ "de antígeno",
                                 type == "Molecular+Antigens" ~ "moleculares o de antígenos"),
                       "que son casos únicos nuevos")
  } else{
    the_title <- paste("% de personas que se hicieron prueba\n" , 
                       case_when(type == "Molecular" ~ "molecular", 
                                 type == "Serological" ~ "serológica",
                                 type == "Antigens" ~ "de antígeno",
                                 type == "Molecular+Antigens" ~ "moleculares o de antígenos"),
                       "con resultado positivo")
  }
  
  ret <- dat %>%
    ggplot(aes(date, rate)) +
    geom_hline(yintercept = 0.03, lty=2, color = "gray") +
    geom_hline(yintercept = 0.10, lty=2, color = "gray") +
    geom_hline(yintercept = 0.20, lty=2, color = "gray") +
    annotate("text", end_date + days(2), 0.015, label = "Bajo") + #, color = "#01D474") +
    annotate("text", end_date + days(2), 0.065, label = "Medio") + #, color = "#FFC900") +
    annotate("text", end_date + days(2), 0.15, label = "Alto") + #, color = "#FF9600") +
    annotate("text", end_date + days(2), 0.225, label = "Crítico") + #, color = "#FF0034") +
    geom_point(size = 2, alpha = 0.65, show.legend = FALSE) +
    ylab("Tasa") +
    xlab("Fecha") +
    scale_x_date(date_labels = "%b", breaks = breaks_width("1 month")) +
    theme_bw() +
    geom_line(aes(date, fit, lty = date > last_day), color = "blue", size = 0.80, show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.35, show.legend = FALSE) +
    labs(title = the_title,
         caption = "Los puntos son el por ciento diarios, la curva el porciento semanal.")
    
  the_ylim <- dat %>%
    summarize(lower = min(lower, na.rm = TRUE), upper = max(upper, na.rm = TRUE))
  
  the_ylim <- c(the_ylim$lower, the_ylim$upper)
  
  if(yscale){
    ret <- ret + coord_cartesian(ylim = c(0, 0.25)) +
      scale_y_continuous(labels = scales::percent) 

  } else{
    ret <- ret + 
      scale_y_continuous(labels = scales::percent, 
                         limits = c(the_ylim)) 
  }
  return(ret)
}


# ICU usage ---------------------------------------------------------------
plot_icu <- function(hosp_mort,  
                      start_date = first_day, 
                      end_date = today(), 
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
    ylab("por ciento") +
    labs(title = "Camas ICU disponibles usadas por pacientes COVID-19",
         caption = "Algunas pacientes en el ICU están ahí por otras causas.\nLa gráfica muestra el porcentaje de las camas restantes ocupadas por pacientes COVID-19.") +
    scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
    scale_y_continuous(limits = lim, labels = scales::percent) +
    theme_bw() 
  
  return(ret)
}


# Deaths ------------------------------------------------------------------

plot_deaths <- function(hosp_mort,  
                        start_date = first_day, 
                        end_date = today(), 
                        cumm = FALSE,
                        yscale = FALSE){
  if(cumm){
    ret <- hosp_mort %>%
      replace_na(list(IncMueSalud = 0)) %>%
      mutate(IncMueSalud = cumsum(IncMueSalud)) %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date)) +
      geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
      ylab("Muertes acumuladas") +
      xlab("Fecha") +
      ggtitle("Muertes acumuladas") +
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      theme_bw()
  } else{
    
    hosp_mort$mort_week_avg[hosp_mort$date > last_day] <- NA
    
    ret <- hosp_mort %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date)) +
      ylab("Muertes") +
      xlab("Fecha") +
      ggtitle("Muertes") +
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      scale_y_continuous(breaks = seq(0, max(hosp_mort$IncMueSalud, na.rm=TRUE), 1)) +
      theme_bw()
    if(yscale){
      ret <- ret +  
        geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
        geom_line(aes(y = mort_week_avg), color="black", size = 1.25)
    } else{
      ret <- ret +  
        geom_point(aes(y = IncMueSalud),width = 0.75, alpha = 0.65) +
        geom_line(aes(y = mort_week_avg), color="black", size = 1.25)
    }
  }
  return(ret)
}
  
plot_hosp <- function(hosp_mort,  
                      start_date = first_day, 
                      end_date = today(),
                      yscale = FALSE){
  
  tmp <- hosp_mort %>% 
    filter(!is.na(HospitCOV19) & 
             date >= start_date & date <= end_date) %>% 
    select(date, HospitCOV19, hosp_week_avg, CamasICU, icu_week_avg) 
  
  if(yscale){
    ret <- tmp %>% 
      ggplot(aes(x = date)) +
      geom_bar(mapping = aes(y = HospitCOV19), stat = "identity", width = 0.75, fill = "#8CC8F4") +
      geom_line(aes(y = hosp_week_avg), color="#8CC8F4", size = 1.25) +
      geom_bar(mapping = aes(y = CamasICU), stat = "identity", width = 0.75, fill = "darkblue") +
      geom_line(aes(y = icu_week_avg), color="darkblue", size = 1.25) +
      ggtitle("Hospitalizaciones y ICU")
  } else{
    ret <- tmp %>% 
      ggplot(aes(x = date)) +
      geom_point(mapping = aes(y = HospitCOV19), width = 0.75, color = "#8CC8F4") +
      geom_line(aes(y = hosp_week_avg), color="#8CC8F4", size = 1.25) +
      ggtitle("Hospitalizaciones") 
  }
  
  ret <- ret +  
    xlab("Fecha") +
    ylab("Pacientes") +
    scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
    theme_bw() 
  
  return(ret)
}

plot_cases <- function(cases, 
                       start_date = first_day, 
                       end_date = today(), 
                       type = "Molecular", 
                       cumm = FALSE,
                       yscale = FALSE){
  if(cumm){
    ret <- cases %>% 
      filter(testType == type) %>%
      mutate(cases = cumsum(cases)) %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date, cases)) +
      geom_bar(stat = "identity", fill = "#FBBCB2", width= 0.75) +
      ggtitle(paste0("Casos acumulados basado en pruebas ", 
                     case_when(type == "Molecular" ~ "moleculares", 
                               type == "Serological" ~ "serológicas",
                               type == "Antigens" ~ "de antígenos",
                               type == "Molecular+Antigens" ~ "moleculares y de antígenos"))) +
      ylab("Casos") +
      xlab("Fecha") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      theme_bw()
  } else{
    
    cases$cases_week_avg[cases$date > last_day] <- NA
    
    ret <- cases %>%
      filter(testType == type & date >= start_date & date <= end_date) %>%
      ggplot(aes(date, cases)) +
      ylab("Casos únicos") +
      xlab("Fecha") +
      ggtitle(paste0("Casos únicos basado en pruebas ", 
                     case_when(type == "Molecular" ~ "moleculares", 
                               type == "Serological" ~ "serológicas",
                               type == "Antigens" ~ "de antígenos",
                               type == "Molecular+Antigens" ~ "moleculares y de antígenos"))) +
      
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      theme_bw()
    
    if(yscale){
      ret <- ret + 
        geom_bar(stat = "identity", fill = "#FBBCB2", width= 0.75) +
        geom_line(aes(y = cases_week_avg), color = "#CC523A", size = 1.25) 
    } else{
      ret <- ret + 
        geom_line(aes(y = cases_week_avg), color = "#CC523A", size = 1.25) 
    }
  }
  return(ret)
}

plot_test <- function(tests, 
                      start_date = first_day, 
                      end_date = today(), 
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
      labs(title = paste("Total de pruebas", 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos",
                                   type == "Molecular+Antigens" ~ "moleculares y de antígenos")),
           caption = "Incluye pruebas duplicadas.") + 
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() 
  } else{
    
    ## last_day is a global variable
    tests$tests_total_week[tests$date > last_day] <- NA
    
    tests %>%
      filter(testType == type & date >= start_date & date <= end_date) %>%
      ggplot(aes(date, tests_total)) +
      geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
      geom_line(aes(y = tests_total_week / 7), color = "#31347A", size = 1.25) +
      ylab("Pruebas") +
      xlab("Fecha") +
      labs(title = paste("Pruebas", 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos",
                                   type == "Molecular+Antigens" ~ "moleculares y de antígenos"), 
           "por día"),
           caption = "Incluye pruebas duplicadas.") + 
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
  }    
}

plot_positivity_by_lab <- function(labs, 
                            start_date = first_day, 
                            end_date = today()-1, 
                            type = "Molecular", 
                            yscale = FALSE){
  if(type == "Molecular+Antigens") return(NULL) else{
    levels <- labs %>% filter(date == pmin(end_date, max(labs$date)) & testType =="Molecular") %>%
      mutate(o = ifelse(Laboratorio == "Otros", -Inf, tests_week_avg)) %>%
      arrange(desc(o)) %>% 
      pull(Laboratorio) 
    
    ret <- labs %>% 
      filter(testType == type & date >= start_date & date <= end_date & tests > 0) %>%
      mutate(Laboratorio = factor(Laboratorio, levels = levels)) %>%
      ggplot(aes(date, fit)) + 
      geom_point(aes(y = positives/tests), alpha = 0.25) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.35) +
      geom_line(col = "blue") +
      scale_x_date(date_labels = "%b", breaks = scales::breaks_width("1 month"))  +
      xlab("Fecha") +
      ylab("Tasa") +
      theme_bw() +
      facet_wrap(~Laboratorio) +
      labs(title = paste("Por ciento de pruebas positivas por laboratorio basada en pruebas" , 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos")))
    if(yscale){
      ret <- ret + coord_cartesian(ylim = c(0, 0.5))
    } 
    return(ret)
  }
}

plot_tests_by_lab <- function(labs, 
                                   start_date = first_day, 
                                   end_date = today()-1, 
                                   type = "Molecular"){
  
  if(type == "Molecular+Antigens") return(NULL) else{
    levels <- labs %>% filter(date == pmin(end_date, max(labs$date)) & testType =="Molecular") %>%
      mutate(o = ifelse(Laboratorio == "Otros", -Inf, tests_week_avg)) %>%
      arrange(o) %>% 
      pull(Laboratorio) 
    
    labs %>% 
      filter(testType == type & date >= start_date & date <= end_date) %>%
      mutate(Laboratorio = factor(Laboratorio, levels = levels)) %>%
      ggplot(aes(date, prop, fill = Laboratorio)) + 
      geom_area() + 
      scale_x_date(date_labels = "%b", breaks = scales::breaks_width("1 month"))  +
      xlab("Fecha") +
      ylab("Proporción de pruebas") +
      theme_bw() +
      labs(title = paste("Proporcion de pruebas", 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos",
                                   type == "Molecular+Antigens" ~ "moleculares y de antígenos"),
                         "por laboratorio (basado en promedio de 7 días)"))
  }
}
plot_map <- function(tests_by_strata,
                     start_date = first_day, 
                     end_date = today(), 
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
              rate       = positives / tests, .groups = "drop") %>%
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
                           name = "Por ciento de pruebas positivas:",
                           limits= c(0, 100*max_rate)) +
      theme_void() +
      theme(legend.position = "bottom") +
      ggtitle(paste("Por ciento de pruebas positivas por municipio para el period de",
                    format(start_date, "%B %d"),
                    "a",
                    format(end_date, "%B %d."), "\n",
                    paste("Pruebas", 
                          case_when(type == "Molecular" ~ "moleculares", 
                                    type == "Serological" ~ "serológicas",
                                    type == "Antigens" ~ "de antígenos",
                                    type == "Molecular+Antigens" ~ "moleculares y de antígenos"))))
  return(ret)
}

make_table <- function(tests, hosp_mort, 
                       start_date = first_day, 
                       end_date = today(), 
                       type = "Molecular",
                       cumm = FALSE){
  
  tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU)

  ## last_day is a global variable 
  #cases$moving_avg[cases$date > last_day] <- NA

  ret <- tests %>%
    filter(testType == type) %>%
    left_join(tmp, by = "date") %>%
    filter(date >= start_date & date <= end_date)
  
  if(cumm){
    ret <- ret %>% 
      mutate(people_positives = cumsum(replace_na(people_positives, 0)),
             people_total = cumsum(replace_na(people_total, 0)),
             rate = people_positives/people_total,
             cases = cumsum(replace_na(cases, 0)),
             IncMueSalud = cumsum(replace_na(IncMueSalud, 0))
      )
  }
  make_pct <- function(x, digit = 1) ifelse(is.na(x), "", paste0(format(round(100*x, digit = digit), nsmall = digit), "%"))
  
  ret <- ret  %>%
    mutate(rate = format(round(rate, 2), nsmall = 2),
           fit = ifelse(is.na(fit), "", 
                        paste0(format(round(100*fit, 1), nsmall = 1), "% ",
                               "(", trimws(format(round(100*lower, 1), nsmall = 1)),"%" , ", ",
                               format(round(100*upper, 1), nsmall=1),"%", ")")),
           n = people_total_week - people_positives_week + cases_week_avg * 7,
           cases_rate = cases_week_avg * 7 / n,
           cases_lower = qbinom(0.025, n, cases_rate) / n,
           cases_upper = qbinom(0.975, n, cases_rate) / n,
           cases_rate = ifelse(is.na(cases_rate), "", 
                               paste0(format(round(100*cases_rate, 1), nsmall = 1), "% ",
                                      "(", trimws(format(round(100*cases_lower, 1), nsmall = 1)),"%" , ", ",
                                      format(round(100*cases_upper, 1), nsmall=1),"%", ")")),
           cases_week_avg = round(cases_week_avg),
           dummy = date) %>%
    mutate(positives = prettyNum(replace_na(people_positives, " "), big.mark = ","),
           tests = prettyNum(replace_na(people_total, " "), big.mark = ","),
           cases = prettyNum(replace_na(cases, " "), big.mark = ","),
           IncMueSalud = prettyNum(replace_na(IncMueSalud, " "), big.mark = ",")) %>% 
    select(date, fit, cases, cases_week_avg, IncMueSalud, CamasICU, HospitCOV19, 
           positives, tests, rate, cases_rate, dummy) %>%
    arrange(desc(date)) %>%
    mutate(date = format(date, "%B %d")) %>%
    setNames(c("Fecha", "% pruebas positivas", "Casos únicos", "Promedio de 7 días",
               "Muertes", "ICU", "Hospital", "Positivos", "Pruebas", 
               "Positivos/ Pruebas", "% casos nuevos", "dateorder"))
  
  ret <- DT::datatable(ret, #class = 'white-space: nowrap',
                       caption = htmltools::tags$caption(
                         style = 'caption-side: top; text-align: Left;',
                         htmltools::withTags(
                           div(HTML(paste0("Datos basados en pruebas ", 
                                           case_when(type == "Molecular" ~ "moleculares.", 
                                                     type == "Serological" ~ "serológicas.",
                                                     type == "Antigens" ~ "de antígenos.",
                                                     type == "Molecular+Antigens" ~ "moleculares y de antígenos."),
                                           "La métrica en la columna <b>% pruebas positiva</b> se calcula para la semana acabando en la fecha de la primera columna. ",
                                           "Esta métrica es parecida a la tasa de positividad que usa el CDC excepto que se remueven pruebas duplicadas dentro de la semana. ",
                                           "La diferencia es que el CDC usa todas pruebas aunque una persona se haga varias. ",
                                           "En paréntesis vemos intervalo de confianza del ", (1-alpha)*100,"%. ", 
                                           "Los <b>casos único</b> son el número de personas con su primera prueba positiva ese día. ",
                                           "El <b>promedio de 7 días</b> es el número de casos únicos por día durante la semana acabando ese día. ",
                                           "Noten que los datos de las pruebas toman", lag_to_complete, "días en estar aproximadamente completos, por lo tanto, ",
                                           "los casos están incompletos para días después de ", format(last_day, "%B %d. "),
                                           "La columna de <b>positivos</b> muestra el número de personas que tuvieron una prueba positiva ese día (no necesariamente son casos únicos).",
                                           "La columna de <b>pruebas</b> es el número de personas que se hicieron una prueba ese día.",
                                           "La métrica <b>% casos nuevos</b> se calcula para la semana acabando ese día y ",
                                           "se define como el por ciento de personas que salieron positivo por primera vez entre los que se hicieron la prueba esa semana, luego de remover los que han salid positivo antes. ",
                                           "Se define como el por ciento de pruebas positivas (incluyendo duplicados) para la semana acabando ese día. ",
                                           "Tengan en cuenta que los fines de semana se hacen menos pruebas y por lo tanto se reportan menos casos. ",
                                           "Las muertes, casos en el ICU y hospitalizaciones vienen del informe oficial del Departamento de Salud y toman un día en ser reportados."
                           ))))),
                       rownames = FALSE,
                       options = list(dom = 't', pageLength = -1,
                                      columnDefs = list(
                                        list(targets = 0, orderData = ncol(ret)-1),
                                        list(targets = ncol(ret)-1, visible = FALSE),
                                        list(className = 'dt-right', targets = 2:(ncol(ret)-2))))) %>%
    DT::formatStyle(c(1, 2, ncol(ret) - 1), "white-space"="nowrap")
  return(ret)
}

make_positivity_table <- function(tests, hosp_mort, 
                       start_date = first_day, 
                       end_date = today(), 
                       type = "Molecular"){

  tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU)
  
  ret <- tests %>%
    filter(testType == type) %>%
    left_join(tmp, by = "date") %>%
    filter(date >= start_date & date <= end_date)
  
  make_pct <- function(x, digit = 1) ifelse(is.na(x), "", paste0(format(round(100*x, digit = digit), nsmall = digit), "%"))
  make_pretty <- function(x) prettyNum(replace_na(x, " "), big.mark = ",")
  
  ret <- ret %>%
    mutate(fit = make_pct(fit),
           cases_rate = make_pct(cases_week_avg * 7 / (people_total_week - people_positives_week + cases_week_avg * 7)),
           cdc_rate = make_pct(tests_positives_week / tests_total_week), 
           people_positives_week = make_pretty(people_positives_week),
           people_total_week = make_pretty(people_total_week),
           cases = make_pretty(round(cases_week_avg*7)),
           tests_positives_week = make_pretty(tests_positives_week),
           tests_total_week = make_pretty(tests_total_week),
           dummy = date) %>%
    select(date, tests_total_week, people_total_week, tests_positives_week, 
           people_positives_week, cases, cdc_rate, fit, cases_rate, dummy) %>%
    arrange(desc(date)) %>%
    mutate(date = format(date, "%b %d")) %>%
    setNames(c("Fecha", "Pruebas", "Personas", "Pruebas+", "Personas+", "Casos",  "Pruebas+/ Pruebas", "Personas+/ Personas", "Casos/ (Casos+Neg)"))
  
  ret <- DT::datatable(ret, #class = 'white-space: nowrap',
                       caption = htmltools::tags$caption(
                         style = 'caption-side: top; text-align: Left;',
                         htmltools::withTags(
                           div(HTML(paste0(
                             "<p> Las primeras columnas 2-5 en la tabla abajo representan los totales de la semana que acaba en el día bajo la primera columna (<b>Fecha</b>):</b>",
                             "<UL>",
                             "<LI><b>Pruebas</b> = total de pruebas hechas, <b>Pruebas+</b> = pruebas positivas.</LI>",
                             "<LI><b>Personas</b> = personas que se hicieron pruebas, <b>Personas+</b> personas que salieron positivo.</LI>",
                             "<LI><b>Casos</b> = casos nuevos únicos, personas que salieron positivo por primera vez.</LI>",
                             "</UL><p>Las dos definiciones son las siguientes:</p>",
                             "<UL>",
                             "<LI><b>Prueba sobre prueba</b> = <b>Personas+</b> / <b>Personas</b></LI>",
                             "<LI><b>Casos sobre personas</b> = <b>Casos</b> / (<b>Casos</b> + <b>Neg</b>), <b>Neg</b> =  <b>Personas</b> - <b>Personas+</b> </LI>",
                             "</UL>",
                             "<p> Estas son basadas en <em>Approach 3</em> y <em>Approch 1</em>, respectivamente, que <a href=\"https://coronavirus.jhu.edu/testing/differences-in-positivity-rates\">esta explicación</a> recomiendo monitorearde ser posible.",
                             "Una pequeña diferencia es que <em>Approch 1</em> usa <b>Pruebas+</b> / <b>Pruebas</b></LI> y aqui removemos duplicados dentro de las semanas. ",
                             "El <a href = \"https://covid.cdc.gov/covid-data-tracker/#testing_positivity7day\">CDC</a> usa <em>Approch 1</em>.</p>",
                             "<p>Noten que hay más <b>Pruebas</b> que <b>Personas</b> porque algunas personas se hacen más de una prueba a la semana y hay errores de duplicación. ",
                             "Noten también que hay más <b>Personas+</b> que <b>Casos</b> únicos nuevos porque algunas personas salen positivo en múltiples semanas.</p>",
                             "<p>La tasa de positividad <b>no es un estimado del por ciento de la población que está infectada</b> ",
                             "ya que las personas que se hacen pruebas no son para nada representativas de la población. ",
                             "Son útiles y se monitorean porque suben cuando suben los casos o cuando no se hacen suficientes pruebas. </b>"
                           ))))),
                       rownames = FALSE,
                       options = list(dom = 't', pageLength = -1,
                                      columnDefs = list(
                                        list(targets = 0, orderData = ncol(ret)-1),
                                        list(targets = ncol(ret)-1, visible = FALSE),
                                        list(className = 'dt-right', targets = 1:(ncol(ret)-1))))) %>%
    DT::formatStyle(1, "white-space"="nowrap")
  return(ret)
}

make_municipio_table <- function(tests_by_strata, 
                                 start_date = first_day, 
                                 end_date = today(), 
                                 type = "Molecular"){
  
    tmp <- tests_by_strata %>%
      filter(date >= start_date &  date <= end_date & testType == type) %>%
      mutate(patientCity = as.character(patientCity)) %>%
      filter(patientCity %in% c("No reportado",poblacion_municipios$patientCity))
 
    children <- tmp %>%
      filter(as.numeric(ageRange) <= 2) %>%
      group_by(patientCity, ageRange) %>%
      summarize(value = sum(positives), .groups = "drop") %>%
      ungroup() %>%
      spread(ageRange, value)
 
    ndays <- as.numeric(end_date - start_date) + 1
 
    ret <- tmp %>%
      group_by(patientCity) %>%
      summarize(positives = sum(positives), tests = sum(tests),
                rate =  positives/tests, .groups = "drop") %>%
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
      setNames(c("Municipio", "% Pruebas positivas (IC)", "Positivos", "Pruebas",  
                 "Positivos por\n100,000 por día", "Población", "Positiovs 0 a 9 años", "Positivos 10 a 19 años", "dummy"))
 
    ret <- DT::datatable(ret, #class = 'white-space: nowrap',
                         caption = paste0("Por razones de privacidad, no tenemos accesoso a identificadors por lo cual estos estimados de positividad están basado en el porciento de pruebas positivas, sin remover duplicados.",
                                          " El porciento de pruebas positivas se calcula en periodo ",
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
                         end_date = today(), 
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
    ylab("por ciento") +
    ggtitle(paste("Distribución de pruebas",  
                  case_when(type == "Molecular" ~ "moleculares", 
                            type == "Serological" ~ "serológicas",
                            type == "Antigens" ~ "de antígenos",
                            type == "Molecular+Antigens" ~ "moleculares y de antígenos"),
                  "positivas por edad",
                  format(start_date, "%B %d"),
                  "a",
                  format(end_date, "%B %d."))) +
    theme_bw()
  
  if(yscale) ret <- ret+ coord_cartesian(ylim = c(0, 0.23))
  
  return(ret)
}

compute_summary <- function(tests, hosp_mort, type = "Molecular", day = today() - days(1)){
  
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
  
  change_casespos <- sapply(1:(nrow(pos)-1), function(i){
    p1 <- casespos$cases_rate[i] 
    p0 <- casespos$cases_rate[i+1]
    d <- p1 - p0
    se <- sqrt(p1*(1-p1) / casespos$n[i] + p0*(1-p0) / casespos$n[i+1])
    signif <- abs(d/se) > qnorm(0.975)
    sign(pos$fit[i] - pos$fit[i+1]) * signif 
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
        arrows[change_mor[i]+2]),
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
      round(mor$mort_week_avg[i]))
  }
  
  ## These are the positivity and hospitalizations for today
  ## we remove the first row to have them match the others
  positividad <- paste(make_pct(pos$fit[1]),  arrows[change_pos[1] + 2])
  pos <- pos[-1,]
  change_pos <- change_pos[-1]
  
  casos_positividad <- paste(make_pct(casespos$cases_rate[1]),  arrows[change_casespos[1] + 2])
  casespos <- casespos[-1,]
  change_casespos <- change_casespos[-1]
  
  hosp <- paste(prettyNum(hos$HospitCOV19[1], big.mark = ","), arrows[change_hos[1]+2])
  hos <- hos[-1,]
  change_hos <- change_hos[-1]
  
  ## make the table
  tab <- tibble(metrica = c("% pruebas positivas", 
                            "% casos nuevos",
                            "Casos nuevos por día", 
                            "Pruebas por día", 
                            "Hospitalizaciones",
                            "Muertes por día"),
                
                meta = c("< 3.0%",
                         "< 3.0%", 
                         "< 30", 
                         "> 4,500", 
                         "< 300",
                         "< 1"),
                
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
              positividad = positividad, casos_positividad = casos_positividad, hosp = hosp))
  
}

plot_rezago <- function(rezago,
                        start_date = first_day, 
                        end_date = today(), 
                        type = "Molecular"){
  
  if(type == "Molecular+Antigens") return(NULL) else{
    rezago %>%
      filter(date >= start_date &  date <= end_date & testType == type) %>%
      filter(diff<20 & diff>=0) %>%
      ggplot(aes(x=diff, color = Resultado)) +
      stat_ecdf(alpha = 0.75) + 
      xlab("Días") + 
      ylab("por ciento de pruebas") +
      labs(title = paste("Rezago entre toma de muestra y día en que se reporta prueba",  
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos")),
           subtitle = paste("Fechas:", format(start_date, "%B %d"), "a", format(end_date, "%B %d."))) +
      scale_y_continuous(labels=scales::percent) +
      xlim(0, 21) +
      theme_bw()
  }
}
  
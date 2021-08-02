# helpers
make_pct <- function(x, digit = 1) ifelse(is.na(x), "", paste0(format(round(100*x, digit = digit), nsmall = digit), "%"))
make_pretty <- function(x) prettyNum(replace_na(x, " "), big.mark = ",")
get_ci_lower <- function(n, p, alpha = 0.05) qbinom(alpha/2, n, p) / n
get_ci_upper <- function(n, p, alpha = 0.05) qbinom(1-alpha/2, n, p) / n
make_pretty_ci <- function(p, lower, upper, nsmall = 1, bounds_nsmall = 1){
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
ifelse(is.na(p), "", 
       paste0(format(round(100*p, nsmall), nsmall = nsmall), " ",
              "(", trimws(format(floor_dec(100*lower, bounds_nsmall), nsmall = bounds_nsmall)),"" , ", ",
              format(ceiling_dec(100*upper, bounds_nsmall), nsmall = bounds_nsmall),"", 
              ")"
       ))
}

dynamic_round <- function(x, min_rounded = 10){
  ifelse(round(x) >= min_rounded, 
         prettyNum(round(x), big.mark = ","),
         prettyNum(round(x, digits = 1), nsmall = 1, big.mark = ",")) %>%
    replace_na("")
}

# positivity plot ---------------------------------------------------------
plot_positivity <- function(tests, 
                            start_date = first_day, 
                            end_date = last_complete_day, 
                            type = "Molecular", 
                            yscale = FALSE,
                            version = c("pruebas", "casos")){
  
  version <- match.arg(version)
  
  dat <- tests %>%
    filter(testType == type &
           date >= start_date & date <= end_date) 
  
  if(version == "casos"){
    dat <- dat %>%
      mutate(n = cases_plus_negatives,
             fit = cases_rate,
             rate = cases_rate_daily,
             lower = get_ci_lower(n, fit),
             upper = get_ci_upper(n, fit))
    the_title <- "Tasa de positividad (casos)"
    bajo <- 0.02
  } else{
    the_title <- "Tasa de positividad (pruebas)"
    bajo <- 0.03
  }
  the_subtitle <- paste("Basado en pruebas", 
                        case_when(type == "Molecular" ~ "moleculares", 
                                  type == "Serological" ~ "serológicas",
                                  type == "Antigens" ~ "de antígeno",
                                  type == "Molecular+Antigens" ~ "moleculares y de antígenos"))
  
  add <- days(ceiling(as.numeric(end_date - start_date) * 0.04))
  
  ret <- dat %>%
    ggplot(aes(date, rate))
  
  the_label <- filter(dat, date == end_date) %>% pull(fit)
  
  ret <- ret + 
    geom_point(alpha = 0.65, show.legend = FALSE) +
    ylab("Tasa de positividad") +
    xlab("Fecha") +
    theme_bw() +
    geom_line(aes(date, fit, lty = date > last_day), color = "blue", size = 0.80, show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.35, show.legend = FALSE) +
    annotate("label", x = end_date + add, y = the_label, label = make_pct(the_label)) +
    labs(title = the_title, subtitle = the_subtitle,
         caption = "Los puntos son el por ciento diarios, la curva el porciento semanal.") +
    scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))
    
     
  the_ylim <- dat %>%
    summarize(lower = min(lower, na.rm = TRUE), upper = max(upper, na.rm = TRUE))
  
  the_ylim <- c(the_ylim$lower, the_ylim$upper)
  
  if(yscale){
    ret <- ret + coord_cartesian(ylim = c(0, 0.15)) +
      scale_y_continuous(labels = scales::percent) 

  } else{
    ret <- ret  +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(the_ylim))
  }
  
  if(type == "Molecular"){
    ret <- ret + 
      annotate("label", start_date, bajo - 0.01, label = paste("Meta =", make_pct(bajo)), hjust = 0) + #, color = "#01D474") +
      geom_hline(yintercept = bajo, lty=2, color = "gray") 
    ##geom_hline(yintercept = 0.10, lty=2, color = "gray") +
    ##geom_hline(yintercept = 0.20, lty=2, color = "gray") +
    ##annotate("text", end_date + days(2), 0.065, label = "Medio") + #, color = "#FFC900") +
    ##annotate("text", end_date + days(2), 0.15, label = "Alto") + #, color = "#FF9600") +
    ##annotate("text", end_date + days(2), 0.225, label = "Crítico") + #, color = "#FF0034") +
  }
  
  return(ret)
}


# ICU usage ---------------------------------------------------------------
plot_icu <- function(hosp_mort,  
                      start_date = first_day, 
                      end_date = last_complete_day, 
                      yscale = FALSE){
  
  tmp <- hosp_mort %>% 
    filter(!is.na(CamasICU)) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(icu = CamasICU / (CamasICU + CamasICU_disp))
  
  if(yscale){
    lim  <- c(0, 0.4)
  } else
  {
    lim <- c(min(tmp$icu, na.rm = TRUE),
             pmax(max(tmp$icu, na.rm = TRUE), 
                  1/max(tmp$CamasICU + tmp$CamasICU_disp, na.rm=TRUE)))
  }
  min_date <- min(tmp$date)
  
  ret <- tmp %>% 
    ggplot(aes(date, icu)) +
    #geom_hline(yintercept = 0.50, lty=2, color = "gray") +
    #geom_hline(yintercept = 0.60, lty=2, color = "gray") +
    #geom_hline(yintercept = 0.70, lty=2, color = "gray") +
    #geom_hline(yintercept = 1.00, lty=2, color = "gray") +
    #annotate("text", end_date + days(2), 0.25, label = "Bajo") + #, color = "#01D474") +
    #annotate("text", end_date + days(2), 0.55, label = "Medio") + #, color = "#FFC900") +
    #annotate("text", end_date + days(2), 0.65, label = "Alto") + #, color = "#FF9600") +
    #annotate("text", end_date + days(2), 0.75, label = "Crítico") + #, color = "#FF0034") +
    geom_line(lwd = 1.5, color = "darkblue") +
    xlab("Fecha") +
    ylab("Por ciento") +
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
                        end_date = last_complete_day, 
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
      #scale_y_continuous(breaks = seq(0, max(hosp_mort$IncMueSalud, na.rm=TRUE), 1)) +
      theme_bw()
    if(yscale){
      ret <- ret +  
        geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
        geom_line(aes(y = mort_week_avg), color="black", size = 1.25)
    } else{
      ret <- ret +  
        geom_point(aes(y = IncMueSalud), width = 0.75, alpha = 0.65) +
        geom_line(aes(y = mort_week_avg), color="black", size = 1.25)
    }
  }
  return(ret)
}
  
plot_hosp <- function(hosp_mort,  
                      start_date = first_day, 
                      end_date = last_complete_day,
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
                       end_date = last_complete_day, 
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
      labs(title = "Casos acumulados",
           subtitle= paste("Basado en pruebas", 
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
    
    #cases$cases_week_avg[cases$date > last_day] <- NA
    
    ret <- cases %>%
      filter(testType == type & date >= start_date & date <= end_date) %>%
      ggplot(aes(date, cases, lty = date > last_day)) +
      ylab("Casos únicos") +
      xlab("Fecha") +
      labs(title = "Casos únicos", 
           subtitle = paste("Basado en pruebas",
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
        geom_point(color = "#FBBCB2") +
        geom_line(aes(y = cases_week_avg), color = "#CC523A", size = 1.25) 
    }
    ret <- ret + theme(legend.position = "none")
  }
  return(ret)
}

plot_test <- function(tests, 
                      start_date = first_day, 
                      end_date = last_complete_day, 
                      type = "Molecular", 
                      cumm = FALSE){
  if(cumm){
    tests %>%
      filter(testType == type) %>%
      mutate(tests = cumsum(people_total)) %>%
      filter(date >= start_date & date <= end_date) %>%
      ggplot(aes(date, tests)) +
      geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
      ylab("Pruebas acumuladas") +
      xlab("Fecha") +
      labs(title = paste("Total de pruebas", 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos",
                                   type == "Molecular+Antigens" ~ "moleculares y de antígenos"))) + 
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() 
  } else{
    
    ## last_day is a global variable
    #tests$people_total_week[tests$date > last_day] <- NA
    
    tests %>%
      filter(testType == type & date >= start_date & date <= end_date) %>%
      ggplot(aes(date, people_total)) +
      geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
      geom_line(aes(y = people_total_week / 7, lty = date > last_day), color = "#31347A", size = 1.25, show.legend = FALSE) +
      ylab("Pruebas") +
      xlab("Fecha") +
      labs(title = paste("Pruebas", 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos",
                                   type == "Molecular+Antigens" ~ "moleculares y de antígenos"), 
           "por día")) + 
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month"))  +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
  }    
}

plot_positivity_by_lab <- function(labs, 
                            start_date = first_day, 
                            end_date = last_complete_day, 
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
      geom_line(aes(lty = date > last_day), col = "blue", show.legend = FALSE) +
      scale_x_date(date_labels = "%b", breaks = scales::breaks_width("1 month"))  +
      xlab("Fecha") +
      ylab("Tasa") +
      theme_bw() 
      
      labs(title = paste("Por ciento de pruebas positivas por laboratorio basada en pruebas" , 
                         case_when(type == "Molecular" ~ "moleculares", 
                                   type == "Serological" ~ "serológicas",
                                   type == "Antigens" ~ "de antígenos")))
    if(yscale){
      ret <- ret + coord_cartesian(ylim = c(0, 0.5)) + facet_wrap(~Laboratorio)
    } else{
      ret <- ret + facet_wrap(~Laboratorio, scales = "free_y")
    }
    return(ret)
  }
}

plot_tests_by_lab <- function(labs, 
                                   start_date = first_day, 
                                   end_date = last_complete_day, 
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
                     end_date = last_complete_day, 
                     type = "Molecular",
                     min_rate = 0.03,
                     max_rate = 0.12){
    
  load("data/map.rda")
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
    mutate(rate = 100*pmin(pmax(rate, min_rate), max_rate)) %>%
    na.omit() %>%
    rename(municipio = patientCity) %>%
    left_join(map, by = "municipio") %>%
    ggplot() + 
    geom_polygon(aes(x = X, y = Y, group = paste(municipio, part), fill = rate), color = "black", size = 0.15)  + 
    geom_text(mapping = aes(x = X, y = Y, label = municipio), data = map_centers,
              size  = 2.2,
              color = "black",
              fontface = "bold") +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                         name = "Por ciento de pruebas positivas:",
                         limits= c(100*min_rate, 100*max_rate)) +
    coord_map() +
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
                       end_date = last_complete_day, 
                       type = "Molecular",
                       cumm = FALSE){
  
  tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU,
                total_distributed, total_vaccinations, 
                people_vaccinated, people_fully_vaccinated)

  ## last_day is a global variable 
  #cases$moving_avg[cases$date > last_day] <- NA

  ret <- tests %>%
    filter(testType == type) %>%
    left_join(tmp, by = "date") %>%
    filter(date >= start_date & date <= end_date) %>%
    rename(tests_rate = fit,
           tests_rate_lower = lower,
           tests_rate_upper = upper,
           tests_rate_daily = rate,
           mort = IncMueSalud,
           icu = CamasICU,
           hosp = HospitCOV19,
           tests = people_total,
           positives = people_positives) %>%
    mutate(tests_rate_daily_lower = get_ci_lower(tests, tests_rate_daily),
            tests_rate_daily_upper = get_ci_upper(tests, tests_rate_daily),
            cases_rate_lower = get_ci_lower(cases_plus_negatives, cases_rate),
            cases_rate_upper = get_ci_upper(cases_plus_negatives, cases_rate),
            cases_rate_daily_lower = get_ci_lower(cases_plus_negatives_daily, cases_rate_daily),
            cases_rate_daily_upper = get_ci_upper(cases_plus_negatives_daily, cases_rate_daily)) %>%
  select(date, 
         tests_rate, 
         tests_rate_lower, 
         tests_rate_upper, 
         tests_rate_daily,
         tests_rate_daily_lower,
         tests_rate_daily_upper,
         cases_rate,
         cases_rate_lower, 
         cases_rate_upper, 
         cases_rate_daily,
         cases_rate_daily_lower,
         cases_rate_daily_upper,
         mort, icu, hosp, 
         cases, cases_week_avg,
         positives, tests, negative_cases,
         people_vaccinated, people_fully_vaccinated, 
         total_vaccinations, total_distributed)
          
  if(cumm){
    ret <- ret %>% 
      mutate(positives = cumsum(replace_na(positives, 0)),
             tests = cumsum(replace_na(tests, 0)),
             negative_cases = cumsum(replace_na(negative_cases, 0)),
             tests_rate = positives / tests, 
             cases = cumsum(replace_na(cases, 0)),
             mort = cumsum(replace_na(mort, 0)),
             cases_rate = cases / (cases + negative_cases),
             tests_rate_lower = get_ci_lower(tests, tests_rate),
             tests_rate_upper = get_ci_upper(tests, tests_rate),
             tests_rate_daily = NA,
             tests_rate_daily_lower = NA,
             tests_rate_daily_upper = NA,
             cases_rate_lower = get_ci_lower(negative_cases + cases, cases_rate),
             cases_rate_upper = get_ci_upper(negative_cases + cases, cases_rate),
             cases_rate_daily = NA,
             cases_rate_daily_lower = NA,
             cases_rate_daily_upper = NA,
             icu = NA, hosp = NA, 
             cases_week_avg = NA)
  }
  
  ret <- select(ret, -negative_cases)
  
  return(ret)
}
  
make_pretty_table <- function(tab, the_caption = ""){
  ret <- tab %>%
  arrange(desc(date)) %>%
    mutate(dummy = date,
           date = format(date, "%B %d"),
           tests_rate = make_pretty_ci(tests_rate, tests_rate_lower, tests_rate_upper),
           tests_rate_daily = make_pretty_ci(tests_rate_daily, tests_rate_daily_lower, tests_rate_daily_upper),
           cases_rate = make_pretty_ci(cases_rate, cases_rate_lower, cases_rate_upper),
           cases_rate_daily = make_pretty_ci(cases_rate_daily, cases_rate_daily_lower, cases_rate_daily_upper),
           cases = make_pretty(cases),
           cases_week_avg = round(cases_week_avg),
           positives = make_pretty(positives),
           tests = make_pretty(tests),
           mort = make_pretty(mort),
           total_distributed = make_pretty(total_distributed), 
           total_vaccinations = make_pretty(total_vaccinations),
           people_vaccinated = make_pretty(people_vaccinated),
           people_fully_vaccinated = make_pretty(people_fully_vaccinated)) %>%
    select(date, tests_rate, cases_rate, 
           cases, cases_week_avg, 
           mort, icu, hosp,  tests,  
           tests_rate_daily, cases_rate_daily,
           people_vaccinated, people_fully_vaccinated, total_vaccinations, total_distributed, 
           dummy) 
  
  col_names <- c("Fecha", 
                 "pruebas", 
                 "casos",
                 "diarios",
                 "promedio de 7 dias",
                 "Muertes",
                 "ICU",
                 "total",
                 "Pruebas", 
                 "pruebas", 
                 "casos",
                 "Vacunados", "Dosis completa", 
                 "Vacunas", "Distribuidas", 
                 "dateorder")
  
  the_header <- htmltools::withTags(table(
    class = 'display',
    thead(style = "border-collapse: collapse;",
          tr(
            th('', colspan = 1, style = "border-bottom: none;"),
            th('Tasas de positividad', colspan = 2, style = "border-bottom: none;text-align:center;"),
            th( 'Casos únicos', colspan = 2, style = "border-bottom: none;text-align:center;"),
            th('', rowspan = 1, style = "border-bottom: none;"),
            th('Hospitalizaciones', colspan = 2, style = "border-bottom: none;text-align:center;"),
            th('', colspan = 1, style = "border-bottom: none;"),#,  style = "vertical-align: bottom;"),
            th('Tasas diarias', colspan = 2, style = "border-bottom: none;text-align:center;"),
             th('Vacunas', colspan = 4, style = "text-align:center; border-bottom: none;"),
            th('', colspan = 1, style = "border-bottom: none;")),
          tr(
            lapply(col_names, th)
          )))
    )
  
  
  ret <- DT::datatable(ret, container = the_header,
                       caption = htmltools::tags$caption(
                         style = 'caption-side: top; text-align: Left;',
                         htmltools::withTags(htmltools::div(htmltools::HTML(the_caption)))),
                       rownames = FALSE,
                       options = list(dom = 't', pageLength = -1,
                                      columnDefs = list(
                                        list(targets = 0, orderData = ncol(ret)-1),
                                        list(targets = ncol(ret)-1, visible = FALSE),
                                        list(className = 'dt-center', targets = c(1:2, 9:10)),
                                        list(className = 'dt-right', targets = c(3:8,11:(ncol(ret)-2)))))) %>%
    DT::formatStyle(c(1:3, 10,11), "white-space"="nowrap")

  return(ret)
}

make_positivity_table <- function(tests, hosp_mort, 
                                  start_date = first_day, 
                                  end_date = last_complete_day, 
                                  type = "Molecular"){

  tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU)
  
  ret <- tests %>%
    filter(testType == type) %>%
    left_join(tmp, by = "date") %>%
    filter(date >= start_date & date <= end_date)
  
  ret <- ret %>%
    mutate(fit = make_pct(fit),
           cases_rate = make_pct(cases_rate),
           neg = make_pretty(people_total_week - people_positives_week),
           people_positives_week = make_pretty(people_positives_week),
           people_total_week = make_pretty(people_total_week),
           cases = make_pretty(round(cases_week_avg*7)),
           dummy = date) %>%
    select(date,  people_total_week,  
           people_positives_week, neg, cases, fit, cases_rate, dummy) %>%
    arrange(desc(date)) %>%
    mutate(date = format(date, "%b %d")) %>%
    setNames(c("Fecha", "Pruebas", "Positivos", "Negativos", "Casos", "Positivos / Pruebas", "Casos / (Casos+Neg)"))
  
   return(ret)
}

make_municipio_table <- function(tests_by_strata, 
                                 start_date = first_day, 
                                 end_date = last_complete_day, 
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
      mutate(lower = get_ci_lower(tests, rate),
             upper = get_ci_upper(tests, rate),
             ppc = round(positives/poblacion * 100000 / ndays, 1)) %>%
      arrange(desc(rate)) %>%
      mutate(rate = make_pretty_ci(rate, lower, upper),
             poblacion_text = make_pretty(poblacion))%>%
      select(patientCity, rate, positives, tests, ppc, poblacion_text, `0 to 9`, `10 to 19`, poblacion) %>%
      setNames(c("Municipio", "% Pruebas positivas (IC)", "Positivos", "Pruebas",  
                 "Positivos por\n100,000 por día", "Población", "Positiovs 0 a 9 años", "Positivos 10 a 19 años", "dummy"))
 
    return(ret)
}

# plot_agedist <- function(tests_by_strata,
#                          start_date = first_day, 
#                          end_date = last_complete_day, 
#                          type = "Molecular",
#                          yscale = FALSE,
#                          version = c("hist", "tendencia")){
#   
#   version <- match.arg(version)
#   
#   age_levels <- levels(tests_by_strata$ageRange)
#   dat <- tests_by_strata %>%
#     filter(testType == type & date >= start_date & date <= end_date) %>%
#     filter(ageRange != "No reportado") %>%
#     mutate(ageRange = age_levels[ifelse(as.numeric(ageRange) >= 9, 9, as.numeric(ageRange))]) %>%
#     mutate(ageRange = ifelse(ageRange == "80 to 89", "80+", ageRange)) %>%
#     mutate(ageRange = str_replace(ageRange, "to", "a"))
#   
#   if(version == "hist"){
#     ret <- dat %>%
#       group_by(ageRange) %>%
#       summarize(positives = sum(positives), .groups = "drop") %>%
#       ungroup() %>%
#       mutate(percent = positives/sum(positives)) %>%
#       ggplot(aes(ageRange, percent)) +
#       geom_bar(stat = "identity") +
#       scale_y_continuous(labels = scales::percent) +
#       xlab("Edad") +
#       ylab("por ciento") +
#       ggtitle(paste("Distribución de pruebas",  
#                     case_when(type == "Molecular" ~ "moleculares", 
#                               type == "Serological" ~ "serológicas",
#                               type == "Antigens" ~ "de antígenos",
#                               type == "Molecular+Antigens" ~ "moleculares y de antígenos"),
#                     "positivas por edad",
#                     format(start_date, "%B %d"),
#                     "a",
#                     format(end_date, "%B %d."))) +
#       theme_bw() 
#     
#     if(yscale) ret <- ret + coord_cartesian(ylim = c(0, 0.23))
#   } else{
#     age_levels <- levels(tests_by_strata$ageRange)
#     ret <- dat %>%
#       group_by(ageRange, week = round_date(date, "week")) %>%
#       summarize(positives = sum(positives), .groups = "drop") %>%
#       ungroup() %>%
#       group_by(week) %>%
#       mutate(percent = positives/sum(positives)) %>%
#       ungroup() %>%
#       ggplot(aes(week, percent)) +
#       geom_line(stat = "identity") +
#       scale_x_date(date_labels = "%b %d") +
#       scale_y_continuous(labels = scales::percent) +
#       xlab("Fecha") +
#       ylab("por ciento") +
#       ggtitle(paste("Distribución de pruebas",  
#                     case_when(type == "Molecular" ~ "moleculares", 
#                               type == "Serological" ~ "serológicas",
#                               type == "Antigens" ~ "de antígenos",
#                               type == "Molecular+Antigens" ~ "moleculares y de antígenos"),
#                     "positivas cada semana por edad.")) +
#       theme_bw()
#     
#     if(yscale){
#       ret <- ret + facet_wrap(~ageRange, ncol = 3) + coord_cartesian(ylim = c(0, 0.23))
#     } else{
#       ret <- ret + facet_wrap(~ageRange, ncol = 3, scale = "free") 
#     }
#   }
#   return(ret)
# }

plot_rezago <- function(rezago, rezago_mort,
                        start_date = first_day, 
                        end_date = last_complete_day, 
                        type = "Molecular", 
                        n_points =  600,
                        max_days = 30){
  
  diff_mort <- rezago_mort %>%
    filter(date >= start_date &  date <= end_date) %>%
    pull(diff)
  
  dias <- seq(0, max_days, length.out = n_points)
  
  dat <- data.frame(Resultado = "Muertes", dias = dias, props = ecdf(diff_mort)(dias))
  the_title <- "Rezago entre día de muerte y día en que se reporta en informe oficial"
  
  if(type != "Molecular+Antigens"){
    dat_tests <- rezago %>%
      filter(date >= start_date &  date <= end_date & testType == type) %>%
      filter(diff >= 0) %>%
      select(diff, Resultado) 
    
    diff_pos <- filter(dat_tests, Resultado == "Positivos") %>% pull(diff)
    dat_pos <- data.frame(Resultado = "Positivos", dias = dias, props = ecdf(diff_pos)(dias))
    
    diff_neg <- filter(dat_tests, Resultado == "Negativos") %>% pull(diff)
    dat_neg <- data.frame(Resultado = "Negativos", dias = dias, props = ecdf(diff_neg)(dias))
    
    dat <- bind_rows(dat, bind_rows(dat_neg, dat_pos))
    the_title <- paste(the_title, "\ny entre toma de muestra y día en que se reporta prueba",  
                       case_when(type == "Molecular" ~ "moleculares", 
                                 type == "Serological" ~ "serológicas",
                                 type == "Antigens" ~ "de antígenos"))
  }
    
    
  dat %>%
    ggplot(aes(x = dias, y = props, color = Resultado)) +
    geom_step(alpha = 0.75) + 
    xlab("Días de rezago") + 
    ylab("Por ciento reportados") +
    labs(title = the_title,
         subtitle = paste("Fechas:", format(start_date, "%B %d"), "a", format(end_date, "%B %d."))) +
    scale_y_continuous(labels=scales::percent, limits = c(0,1)) + 
    scale_color_manual(values=c("#000000", "#00BFC4", "#F8766D")) + 
    xlim(0, max_days) +
    theme_bw()
}


make_lab_tab <- function(lab_tab,
                         start_date = first_day, 
                         end_date = last_complete_day, 
                         type = "Molecular"){
  ret <- filter(lab_tab,  
                date >= start_date, 
                date <= end_date, 
                testType == type) %>%
    
    arrange(desc(date)) %>%
    select(-testType) 
  
  col_total <- ret %>% group_by(Laboratorio) %>% 
    summarize(Total = sum(tests, na.rm = TRUE),  .groups= "drop")
  row_total <- ret %>% group_by(date) %>% 
    summarize(tests = sum(tests, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(date)) 
  
  col_names <- as.character(row_total$date)
  
  row_total <- 
    spread(row_total, date, tests, fill = 0) %>% mutate(Laboratorio = "Total") 
  
  ret <- left_join(col_total, by="Laboratorio", spread(ret, date, tests, fill = 0)) %>%
    arrange(desc(Total)) 
  
  row_total <- mutate(row_total, Total = sum(ret$Total, na.rm = TRUE))
  
  ret <- bind_rows(row_total, ret) %>% 
    select("Laboratorio",  "Total", all_of(col_names))
  
  ret <- mutate_if(ret, is.numeric, function(x) prettyNum(x, big.mark=",")) %>%
    rename(Entidad = Laboratorio) ## hay hospitales también
  
  return(ret)
}

plot_vaccines <- function(hosp_mort,  
                                  start_date = first_day, 
                                  end_date = last_complete_day){
  
  tmp <- hosp_mort %>% 
    filter(date >= start_date & date <= end_date & !is.na(people_fully_vaccinated)) %>% 
    select(date, total_distributed, total_vaccinations, people_vaccinated,
    people_fully_vaccinated) %>% 
    rename("Dosis distribuidas" = total_distributed,
           "Vacunaciones totales" = total_vaccinations,
           "Personas vacunadas" = people_vaccinated,
           "Personas vacunadas (Dosis completa)" =  people_fully_vaccinated) %>%
    pivot_longer(-date) %>%
    mutate(name = factor(name, 
                         levels = c("Personas vacunadas (Dosis completa)",
                                    "Personas vacunadas",
                                    "Vacunaciones totales",
                                    "Dosis distribuidas")))
      
  
  tmp %>% ggplot(aes(date, value/1000, color = name)) +
    geom_line() +
    geom_point(show.legend = FALSE) +
    scale_x_date(date_labels = "%b %d") +#, breaks = breaks_width("days")) +
    ggtitle("Personas vacunadas, vacunaciones totales y dosis distribuidas") +
    ylim(c(0, max(tmp$value/1000))) + 
    ylab("Total en miles") +
    xlab("Fecha") +
    theme_bw() +   
    theme(legend.position="bottom", legend.title=element_blank()) 
}

 
table_vaccines <- function(hosp_mort,  
                          start_date = first_day, 
                          end_date = last_complete_day){
  
  tab <- hosp_mort %>% 
    filter(date >= start_date & date <= end_date & !is.na(people_fully_vaccinated)) %>% 
    select(date, people_vaccinated,
           people_fully_vaccinated, total_vaccinations, total_distributed) %>% 
    mutate(people_vaccinated = make_pretty(people_vaccinated),
           people_fully_vaccinated =  make_pretty(people_fully_vaccinated), 
           total_vaccinations =  make_pretty(total_vaccinations), 
           total_distributed =  make_pretty(total_distributed),
      dummy = date) %>%
    arrange(desc(dummy)) %>%
    rename("Personas vacunadas" = people_vaccinated,
           "Dosis completa" =  people_fully_vaccinated,
           "Dosis distribuidas" = total_distributed,
           "Vacunaciones totales" = total_vaccinations,
           Fecha = date) %>%
    mutate(Fecha = format(Fecha, "%B %d"))
    
  tab <- DT::datatable(tab, 
                       rownames = FALSE,
                       class = "display nowrap",
                       options = list(dom = 't', pageLength = -1,
                                      columnDefs = list(
                                        list(targets = 0, orderData = ncol(tab)-1),
                                        list(targets = ncol(tab)-1, visible = FALSE),
                                        list(className = 'dt-center', targets = c(1:(ncol(tab)-1))))))
  
  return(tab)
}

plot_fully_vaccinated <- function(hosp_mort,  
              start_date = first_day, 
              end_date = last_complete_day,
              yscale = FALSE){


    tmp <- hosp_mort %>% 
      filter(!is.na(people_fully_vaccinated) & 
               date >= start_date & date <= end_date) %>% 
      select(date, people_fully_vaccinated) 

    ret <- tmp %>% 
      ggplot(aes(date, people_fully_vaccinated / pr_pop)) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels = "%b %d") +#, breaks = breaks_width("days")) +
      scale_y_continuous(labels = scales::percent) +
      ggtitle("Por ciento de la población vacunada con dosis completa") +
      ylab("Por ciento") +
      xlab("Fecha") +
      theme_bw() 
    
    if(yscale){
      ret + 
      geom_hline(yintercept = 0.7) + 
      annotate("text", min(tmp$date) + days(2), 0.72, label = "Meta = 70% de la población") + 
      ylim(c(0, max(0.7, max(tmp$people_fully_vaccinated / pr_pop))))
    }
    return(ret)
}


plot_travelers <- function(travelers,  
                          start_date = first_day, 
                          end_date = last_complete_day,
                          yscale = FALSE,
                          version = c("totals", "percent")){
  
  version <- match.arg(version)
  
  
  tmp <- travelers %>% 
    filter(date >= start_date & date <= end_date) 
  
  if(version == "totals"){
    tmp <- select(tmp, -starts_with("perc")) 
    ylab <- "Viajeros por día"
    the_title <- "Viajeros llegando a Puerto Rico"
    if(yscale){
      ylim <- with(travelers, range(c(residents, short, long), na.rm=TRUE))
    } else{
      ylim <- with(tmp, range(c(residents, short, long), na.rm=TRUE))
    }
    } else{
      tmp <- select(tmp, date, starts_with("perc")) 
      ylab <- "Por ciento"
      the_title <- "Viajeros llegando a Puerto Rico"
      if(yscale){
        ylim <- c(0,1)
      } else{
        ylim <- with(tmp, range(c(perc_residents, perc_short, perc_long), na.rm=TRUE))
      }
    }
  
  values <- select(tmp, -contains("week_avg")) %>% 
    pivot_longer(-date) %>%
    mutate(name = str_remove_all(name,"perc_"))
  avgs <- select(tmp, date, contains("week_avg")) %>% 
    pivot_longer(-date, values_to = "avg") %>%
    mutate(name = str_remove_all(name,"perc_|_week_avg"))
  
  tab <- left_join(values, avgs, by = c("date", "name")) %>%
    mutate(name = case_when(name == "residents" ~ "Residente",
                            name == "short" ~ "Menos de 5 días",
                            name == "long" ~ "5 días o más")) %>%
    mutate(name = factor(name, 
                         levels = c("Residente",
                                    "Menos de 5 días",
                                    "5 días o más")))
  
  
  tab  %>% ggplot(aes(date, value, color = name)) +
    geom_point(alpha = 0.25) +
    geom_line(aes(y = avg), size = 1.5) +
    scale_x_date(date_labels = "%b %d") +
    ggtitle("Viajeros llegando a Puerto Rico") +
    ylab(ylab) +
    xlab("Fecha") +
    ggtitle(the_title) +
    ylim(ylim) + 
    theme_bw() 
}


table_travelers <- function(travelers,  
                           start_date = first_day, 
                           end_date = last_complete_day){
  
  tab <- travelers %>% 
    #filter(date >= start_date & date <= end_date) %>% 
    select(date, 
           residents, residents_week_avg, perc_residents,
           short, short_week_avg, perc_short,
           long, long_week_avg, perc_long) %>%
    mutate(residents = make_pretty(residents),
           residents_week_avg = make_pretty(round(residents_week_avg)),
           perc_residents = make_pct(perc_residents),
           short = make_pretty(short), 
           short_week_avg = make_pretty(round(short_week_avg)), 
           perc_short = make_pct(perc_short),
           long = make_pretty(long), 
           long_week_avg = make_pretty(round(long_week_avg)), 
           perc_long = make_pct(perc_long), 
           dummy = date) %>%
    arrange(desc(dummy)) %>%
    mutate(date = format(date, "%B %d")) %>%
    setNames(c("Fecha", 
               "Total", "Media móvil", "% con prueba",
               "Total", "Media móvil", "% con prueba",
               "Total", "Media móvil", "% con prueba", 
               "dummy")) 
  
  the_header <- htmltools::withTags(table(
    class = 'display',
    thead(style = "border-collapse: collapse;",
          tr(
            th('', colspan = 1, style = "border-bottom: none;"),
            th('Residentes', colspan = 3, style = "border-bottom: none;text-align:center;"),
            th('Menos de 5 días', colspan = 3, style = "border-bottom: none;text-align:center;"),
            th('5 días o más', colspan = 3, style = "border-bottom: none;text-align:center;")),
          tr(
            lapply(names(tab)[-ncol(tab)], th)
          )))
  )
  tab <- DT::datatable(tab, 
                       container = the_header,
                       rownames = FALSE,
                       class = "display nowrap",
                       options = list(dom = 't', pageLength = -1,
                                      columnDefs = list(
                                        list(targets = 0, orderData = ncol(tab)-1),
                                        list(targets = ncol(tab)-1, visible = FALSE),
                                        list(className = 'dt-center', targets = c(1:(ncol(tab)-1))))))
  
  return(tab)
}


summary_by_region <- function(tests_by_region, 
                              pop_by_region,
                              start_date = first_day, 
                              end_date = last_complete_day, 
                              type = "Molecular", 
                              cumm = FALSE,
                              yscale = FALSE,
                              version = c("tp_pruebas", "tp_casos", "casos", "pruebas", "prop")){
  
  version <- match.arg(version)
  
  dat <- tests_by_region %>%
    filter(testType == type &
             date >= start_date & date <= end_date) %>%
    mutate(cases_rate_lower = get_ci_lower(cases_plus_negatives, cases_rate),
           cases_rate_upper = get_ci_upper(cases_plus_negatives, cases_rate)) %>%
    left_join(pop_by_region, by = "region") 
  
  if(cumm){
    dat <- dat %>% 
      group_by(region) %>%
      mutate(people_positives = cumsum(replace_na(people_positives, 0)),
             people_total = cumsum(replace_na(people_total, 0)),
             negative_cases = cumsum(replace_na(negative_cases, 0)),
             fit = people_positives / people_total, 
             cases = cumsum(replace_na(cases, 0)),
             cases_rate = cases / (cases + negative_cases),
             lower = get_ci_lower(people_total, fit),
             upper = get_ci_upper(people_total, fit),
             cases_rate_lower = get_ci_lower(negative_cases + cases, cases_rate),
             cases_rate_upper = get_ci_upper(negative_cases + cases, cases_rate),
             cases_week_avg = cases,
             people_total_week = people_total) %>%
      ungroup()
    
    
    yscale = FALSE
  }
  
  type_char <- case_when(type == "Molecular" ~ "molecular", 
                         type == "Serological" ~ "serológica",
                         type == "Antigens" ~ "de antígeno",
                         type == "Molecular+Antigens" ~ "moleculares o de antígenos")
  
  pct <- FALSE
  if(version == "tp_pruebas"){
    tab <- dat %>% 
      mutate(the_stat = make_pretty_ci(fit, lower, upper)) %>%
      select(date, region, the_stat)
    dat <- dat %>% rename(the_stat = fit)
    var_title <- "Tasa de positividad (pruebas)"
    the_ylim <- c(0, 0.2)
    pct <- TRUE
  }
  if(version == "tp_casos"){
    tab <- dat %>% 
      mutate(the_stat = make_pretty_ci(cases_rate, cases_rate_lower, cases_rate_upper)) %>%
      select(date, region, the_stat)
    dat <- dat %>% rename(the_stat = cases_rate)
    var_title <- "Tasa de positividad (casos)"
    the_ylim <- c(0, 0.15)
    pct <- TRUE
  }
  if(version == "casos"){
    tab <- dat %>% 
      mutate(the_stat = cases_week_avg/poblacion*10^5) %>%
      mutate(the_stat = ifelse(is.na(the_stat), "", format(round(the_stat, 1), nsmall = 1))) %>%
      select(date, region, the_stat)
    dat <- dat %>% 
      mutate(cases_week_avg = cases_week_avg/poblacion*10^5) %>%
      rename(the_stat = cases_week_avg) 
    var_title <- "Casos únicos por día por 100,000 habitantes"
    the_ylim <- c(0, 40)
  }
  if(version ==  "pruebas"){
    tab <- dat %>% 
      mutate(the_stat = make_pretty(round(people_total_week/poblacion*10^5))) %>%
      select(date, region, the_stat)
    dat <- dat %>% 
      mutate(people_total_week = people_total_week/poblacion*10^5) %>%
      rename(the_stat = people_total_week)
    var_title <- "Pruebas por día por 100,000 habitantes"
    the_ylim <- c(0, 3000)
  }
  if(version ==  "prop"){
    dat <- dat %>% group_by(date) %>%
      mutate(the_stat = people_total_week/sum(people_total_week)) %>%
      ungroup() 
    tab <- dat %>% 
      mutate(the_stat = make_pct(the_stat)) %>%
      select(date, region, the_stat)
   
    var_title <- "Por ciento de pruebas"
    the_ylim <- c(0, .35)
    pct <- TRUE
    }
  
  tab <- tab %>%
    pivot_wider(names_from = region, values_from = the_stat) %>% 
    arrange(desc(date)) %>%
    select(date, all_of(levels(dat$region))) %>%
    rename(Fecha = date)
  
  the_title <- var_title
  the_subtitle <- paste("Basado en pruebas", type_char)
  
  pretty_tab <- tab %>% 
    mutate(dummy = Fecha,
           Fecha = format(Fecha, "%B %d")) 
  
  pretty_tab <- 
    DT::datatable(pretty_tab, 
      rownames = FALSE,
      class = "display nowrap",
      options = list(dom = 't', pageLength = -1,
                     columnDefs = list(
                       list(targets = 0, orderData = ncol(pretty_tab)-1),
                       list(targets = ncol(pretty_tab)-1, visible = FALSE),
                       list(className = 'dt-center', targets = c(1:(ncol(pretty_tab)-1))))))
  
  p <- dat %>% 
    ggplot(aes(date, the_stat, color = region,  lty = date > last_day)) + 
    geom_line() +
    guides(linetype = FALSE) +
    xlab("Fecha") +
    ylab(var_title) +
    labs(color = "Región") +
    labs(title = the_title, subtitle = the_subtitle) +
    scale_x_date(date_labels = "%b", breaks = breaks_width("1 month")) +
    theme_bw()

  if(pct) the_labels <- scales::percent else the_labels <- waiver()
  
  if(yscale){
    p <- p + scale_y_continuous(limit = the_ylim, labels = the_labels)
  } else{
    p <- p + scale_y_continuous(labels = the_labels) 
  }
  
return(list(p = p, tab = tab, pretty_tab = pretty_tab))
}

summary_by_age <- function(tests_by_age,
                           deaths_by_age,
                           pop_by_age,
                           start_date = first_day, 
                           end_date = last_complete_day, 
                           type = "Molecular", 
                           cumm = FALSE,
                           yscale = FALSE,
                           version = c("tp_pruebas", "tp_casos", "casos_per", "casos", "deaths_per", "deaths"),
                           facet = TRUE){
  
  version <- match.arg(version)
  
  dat <- tests_by_age %>%
    filter(testType == type & !is.na(ageRange) &
             date >= start_date & date <= end_date) %>%
    mutate(cases_rate_lower = get_ci_lower(cases_plus_negatives, cases_rate),
           cases_rate_upper = get_ci_upper(cases_plus_negatives, cases_rate)) %>%
    left_join(filter(deaths_by_age, date >= start_date & date <= end_date), by = c("date", "ageRange")) %>%
    left_join(pop_by_age, by = "ageRange")
    
  if(cumm){
    dat <- dat %>% 
      group_by(ageRange) %>%
      mutate(people_positives = cumsum(replace_na(people_positives, 0)),
             people_total = cumsum(replace_na(people_total, 0)),
             negative_cases = cumsum(replace_na(negative_cases, 0)),
             fit = people_positives / people_total, 
             cases = cumsum(replace_na(cases, 0)),
             cases_rate = cases / (cases + negative_cases),
             lower = get_ci_lower(people_total, fit),
             upper = get_ci_upper(people_total, fit),
             cases_rate_lower = get_ci_lower(negative_cases + cases, cases_rate),
             cases_rate_upper = get_ci_upper(negative_cases + cases, cases_rate),
             cases_week_avg = cases,
             people_total_week = people_total,
             deaths = cumsum(replace_na(deaths, 0)),
             deaths_week_avg = deaths) %>%
      ungroup()
  }
  
  type_char <- case_when(type == "Molecular" ~ "molecular", 
                         type == "Serological" ~ "serológica",
                         type == "Antigens" ~ "de antígeno",
                         type == "Molecular+Antigens" ~ "moleculares o de antígenos")
  
  pct <- FALSE
  if(version == "tp_pruebas"){
    tab <- dat %>% 
      mutate(the_stat = make_pretty_ci(fit, lower, upper)) %>%
      select(date, ageRange, the_stat)
    dat <- dat %>% rename(the_stat = fit, daily_stat = rate)
    var_title <- "Tasa de positividad (pruebas)"
    the_ylim <- c(0, 0.25)
    pct <- TRUE
  }
  if(version == "tp_casos"){
    tab <- dat %>% 
      mutate(the_stat = make_pretty_ci(cases_rate, cases_rate_lower, cases_rate_upper)) %>%
      select(date, ageRange, the_stat)
    dat <- dat %>% rename(the_stat = cases_rate, daily_stat = cases_rate_daily)
    var_title <- "Tasa de positividad (casos)"
    the_ylim <- c(0, 0.2)
    pct <- TRUE
  }
  if(version == "casos"){
    tab <- dat %>% 
      mutate(the_stat = dynamic_round(cases_week_avg, min_round = 100)) %>%
      select(date, ageRange, the_stat)
    dat <- dat %>% 
      rename(the_stat = cases_week_avg, daily_stat = cases) 
    var_title <- "Casos únicos por día"
    the_ylim <- c(0, 500)
  }
  
  if(version == "casos_per"){
    tab <- dat %>% 
      mutate(the_stat = cases_week_avg/poblacion*10^5) %>%
      mutate(the_stat = ifelse(is.na(the_stat), "", format(round(the_stat, 1), nsmall = 1))) %>%
      select(date, ageRange, the_stat)
    dat <- dat %>% 
      mutate(cases_week_avg = cases_week_avg/poblacion*10^5, cases = cases/poblacion*10^5) %>%
      rename(the_stat = cases_week_avg, daily_stat = cases) 
    var_title <- "Casos únicos por día por 100,000 habitantes"
    the_ylim <- c(0, 80)
  }
  if(version == "deaths"){
    if(cumm){
      tab <- dat %>% 
        mutate(the_stat = make_pretty(deaths)) %>%
        select(date, ageRange, the_stat)
    } else{
      tab <- dat %>% 
        mutate(the_stat = dynamic_round(deaths_week_avg, min_round = 100)) %>%
        select(date, ageRange, the_stat)
    }
    dat <- dat %>% 
      rename(the_stat = deaths_week_avg, daily_stat = deaths) 
    var_title <- "Muertes por día"
    the_ylim <- c(0, 10)
  }
  
  if(version == "deaths_per"){
    tab <- dat %>% 
      mutate(the_stat = deaths_week_avg/poblacion*10^5) %>%
      mutate(the_stat = ifelse(is.na(the_stat), "", format(round(the_stat, 1), nsmall = 1))) %>%
      select(date, ageRange, the_stat)
    dat <- dat %>% 
      mutate(deaths_week_avg = deaths_week_avg/poblacion*10^5, deaths = deaths/poblacion*10^5) %>%
      rename(the_stat = deaths_week_avg, daily_stat = deaths) 
    var_title <- "Muertes por día por 100,000 habitantes"
    the_ylim <- c(0, 3.25)
  }
  
  # if(version ==  "prop"){
  #   dat <- dat %>% group_by(date) %>%
  #     mutate(the_stat = cases_week_avg/sum(cases_week_avg),
  #            daily_stat = cases/sum(cases)) %>%
  #     ungroup() 
  #   tab <- dat %>% 
  #     mutate(the_stat = make_pct(the_stat)) %>%
  #     select(date, ageRange, the_stat)
  #   var_title <- "Por ciento de casos"
  #   the_ylim <- c(0, .5)
  #   pct <- TRUE
  # }
  # 
  if(cumm){
    the_ylim <- range(dat$the_stat, na.rm = TRUE)
    if(version %in% c("casos", "casos_per", "deaths", "deaths_per")) the_ylim[1] <- 0
  }
  
  tab <- tab %>%
    pivot_wider(names_from = ageRange, values_from = the_stat) %>% 
    arrange(desc(date)) %>%
    select(date, all_of(levels(dat$ageRange))) %>%
    rename(Fecha = date)
  
  the_title <- paste(var_title, "por grupo de edad")
  the_subtitle <- paste("Basado en pruebas", type_char)
  
  pretty_tab <- tab %>% 
    mutate(dummy = Fecha,
           Fecha = format(Fecha, "%B %d")) 
  
  pretty_tab <- 
    DT::datatable(pretty_tab, 
                  rownames = FALSE,
                  class = "display nowrap",
                  options = list(dom = 't', pageLength = -1,
                                 columnDefs = list(
                                   list(targets = 0, orderData = ncol(pretty_tab)-1),
                                   list(targets = ncol(pretty_tab)-1, visible = FALSE),
                                   list(className = 'dt-center', targets = c(1:(ncol(pretty_tab)-1))))))
  
  if(pct) the_labels <- scales::percent else the_labels <- waiver()
  
  if(facet){
    p <- dat %>% 
      ggplot(aes(date, the_stat)) + 
      xlab("Fecha") +
      ylab(var_title) +
      labs(color = "Edad") +
      labs(title = the_title, subtitle = the_subtitle) +
      scale_x_date(date_labels = "%b %d") +
      theme_bw() +
      theme(text = element_text(size = 15))  
    
    if(yscale){
      p <- p + facet_wrap(~ageRange, nrow = 2) + 
        scale_y_continuous(labels = the_labels, limits = the_ylim) 
    } else{
      p <- p + facet_wrap(~ageRange, nrow = 2,  scales = "free_y") + 
        scale_y_continuous(labels = the_labels)
    }
    
    if(version %in% c("casos", "casos_per")){ the_color_1 <- "#FBBCB2"; the_color_2 <- "#CC523A"}
    if(version %in% c("deaths", "deaths_per")){ the_color_1 <- "grey"; the_color_2 <- "black"}
    
    if(version %in% c("casos", "casos_per", "deaths", "deaths_per")){
      if(cumm){
        p <- p + 
          geom_bar(stat = "identity", color = the_color_1, fill = the_color_1, width= 0.2)
       } else{
        p <- p + 
          geom_bar(aes(y = daily_stat), stat = "identity", color = the_color_1, fill = the_color_1, width= 0.2, alpha =0.5, show.legend = FALSE) +
          geom_line(aes(lty = date > last_day), color = the_color_2, show.legend = FALSE, size = 1.25) 
      }
    } else{
      p <- p + 
        geom_line(aes(lty = date > last_day), show.legend = FALSE) 
    }
  } else{
    p <- dat %>% 
      ggplot(aes(date, the_stat, color = ageRange,  lty = date > last_day)) + 
      geom_line() +
      guides(linetype = FALSE) +
      xlab("Fecha") +
      ylab(var_title) +
      labs(color = "Edad") +
      labs(title = the_title, subtitle = the_subtitle) +
      scale_x_date(date_labels = "%b %d") +
      theme_bw()
    
    if(yscale){
      p <- p + scale_y_continuous(labels = the_labels, limits = the_ylim)
    } else{
      p <- p + scale_y_continuous(labels = the_labels)
    }
  }
  return(list(p = p, tab = tab, pretty_tab = pretty_tab))
}

###
### this is used to make the table in the front page
compute_summary <- function(tests, hosp_mort, day = last_complete_day){
  
  ## dates that we will put in the table
  ## they are 4 entries, 1 week apart
  ## lag_to_complete is a global var
  the_dates <- day - days(lag_to_complete) - weeks(0:3)
  the_latest_dates <- day - weeks(0:1)
  
  ## positivity
  ## we include the latest day because we have a usable value
  ## we also keep a week earlier 
  ## we take them out later to keep the table dimensions the same
  pos <- filter(tests, testType == "Molecular" & date %in% the_dates) %>%
    arrange(desc(date))
  
  latest <- filter(tests, testType == "Molecular" & date %in% the_latest_dates) %>%
    arrange(desc(date))
  
  if(nrow(latest) > 0) pos <- bind_rows(latest, pos) else bind_rows(slice(pos, c(1,2), pos))
  
  ## this computes the difference in positivity between weeks
  ##determines if they are significant
  ## and returns -1 (decrease), 0 (no change), 1 (increase)
  change_pos <- sapply(c(1,3:(nrow(pos)-1)), function(i){
    p1 <- pos$fit[i] 
    p0 <- pos$fit[i+1]
    d <- p1 - p0
    se <- sqrt(p1*(1-p1) / pos$people_total_week[i] + p0*(1-p0) / pos$people_total_week[i+1])
    signif <- abs(d/se) > qnorm(0.975)
    sign(pos$fit[i] - pos$fit[i+1]) * signif 
  })
  
  casespos <- pos 
  
  change_casespos <- sapply(c(1,3:(nrow(casespos)-1)), function(i){
    p1 <- casespos$cases_rate[i] 
    p0 <- casespos$cases_rate[i+1]
    d <- p1 - p0
    se <- sqrt(p1*(1-p1) / casespos$cases_plus_negatives[i] + 
                 p0*(1-p0) / casespos$cases_plus_negatives[i+1])
    signif <- abs(d/se) > qnorm(0.975)
    sign(casespos$cases_rate[i] - casespos$cases_rate[i+1]) * signif 
  })
  
  pos <- pos[-2,] ##remove second entry which was included only to compare
  casespos <- casespos[-2,]
  
  ## cases 
  ## same a pos but for cases. here we use diagnostic tests: molecular + antigens
  cas <- filter(tests, testType == "Molecular+Antigens" & 
                  date %in% the_dates)  %>%
    arrange(desc(date))
  
  cas_day <- filter(tests, testType == "Molecular+Antigens" & date >= max(the_dates))  %>%
    arrange(desc(cases_week_avg)) %>%
    slice(1) %>% pull(date)
  
  the_latest_cas_dates <- cas_day - weeks(0:1)
  
  latest <- tests %>% 
    filter(testType == "Molecular+Antigens" &date %in% the_latest_cas_dates) %>%
    arrange(desc(date))
  
  if(nrow(latest) > 0) cas <- bind_rows(latest, cas) else hos <- bind_rows(slice(cas, c(1,2)), cas)
  
  ## get overdisepersion, last day is a global variable defined in init
  ## we assume cases are Possion with the precalculated trend an offset.
  phi <- tests %>% filter(date >= make_date(2020, 7, 1) & 
                            date <= make_date(2020, 11, 2) & ## avoid election thanksgiving and xmas
                            date <= last_day &
                            testType == "Molecular+Antigens") %>%
    mutate(wd = factor(wday(date)), week = factor(round_date(date, "week"))) %>%
    glm(cases ~ wd + week, data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  phi <- pmax(phi, 1)
  
  change_cas <- sapply(c(1, 3:(nrow(cas)-1)), function(i){
    d <- cas$cases_week_avg[i] - cas$cases_week_avg[i+1]
    se <- sqrt((phi*cas$cases_week_avg[i] + phi*cas$cases_week_avg[i+1])/7)
    signif <- abs(d/se) > qnorm(0.975)
    sign(d) * signif 
  })
  
  cas <- cas[-2,]
  
  ## tests
  ##as pos but for number of tests, algo diagnostic tests
  tes <- filter(tests, testType == "Molecular+Antigens" & 
                  date %in% the_dates) %>%
    arrange(desc(date))
  
  phi <- tests %>% filter(date >= make_date(2020, 7, 1) & 
                            date <= make_date(2020, 11, 2) & ## avoid elections, thanksgiving and xmas
                            date <= last_day &
                            testType == "Molecular+Antigens") %>%
    mutate(wd = factor(wday(date)), week = factor(round_date(date, "week"))) %>%
    glm(people_total_week ~ wd + week, data = ., family = quasipoisson) %>%
    summary()  %>%
    .$dispersion
  
  phi <- pmax(phi, 1)
  
  
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
  
  hosp_day <- hosp_mort %>% 
    filter(!is.na(HospitCOV19) & date <= day & date >= max(the_dates)) %>%
    arrange(desc(date)) %>% slice(1) %>% pull(date)
  
  the_latest_hosp_dates <- hosp_day - weeks(0:1)
  
  latest <- hosp_mort %>% select(date, HospitCOV19, hosp_week_avg) %>% 
    filter(date %in% the_latest_hosp_dates) %>%
    arrange(desc(date))
  
  if(nrow(latest) > 0) hos <- bind_rows(latest, hos) else hos <- bind_rows(slice(hos, c(1,2)), hos)
  
  hosp_fit <- hosp_mort %>% filter(date >= make_date(2020, 7, 1) & date <= last_day) %>%
    filter(!is.na(HospitCOV19)) %>%
    mutate(wd = factor(wday(date))) %>%
    glm(HospitCOV19 ~ wd, offset = log(hosp_week_avg), data = ., family = quasipoisson) 
  
  phi <- hosp_fit %>%
    summary()  %>%
    .$dispersion
  
  phi <- pmax(phi, 1)
  
  ##becuase data is so correlated we have to compute correlation matrix to compute SE
  hosp_corrs <- acf(hosp_fit$resid, plot = FALSE)$acf[1:14, 1, 1]
  ## this is the transformation needed to compute difference of averages
  X <- matrix(rep(c(1/7, -1/7), each = 7))
  
  change_hos <- sapply(c(1, 3:(nrow(hos)-1)), function(i){
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
  
  hos <- hos[-2,]
  
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
  vac <- hosp_mort %>% 
    select(date,  people_fully_vaccinated, people_vaccinated) %>% 
    filter(!is.na(people_fully_vaccinated)) %>%
    filter(date %in% the_dates) %>%
    arrange(desc(date)) 
  
  latest <- hosp_mort %>% 
    select(date,  people_fully_vaccinated, people_vaccinated) %>% 
    filter(!is.na(people_fully_vaccinated) & date <= day & date > max(the_dates)) %>%
    arrange(desc(date)) %>% 
    slice(1)
  
  if(nrow(latest) > 0) vac <- bind_rows(latest, vac) else vac <- bind_rows(slice(vac, 1), vac)
  
  vac <- vac %>%
    mutate(pct_fully_vaccinated = people_fully_vaccinated/pr_pop,
           pct_one_dose = people_vaccinated/pr_pop)
  
  ## this is the htlm to make colored arrows:  down is green, sideways is yelloww, up is red (bad)
  arrows <- c( "<span style=\"color:#01D474;font-weight: bold;\">&#8595;</span>",
               "<span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span>", 
               "<span style=\"color:#FF0034;font-weight: bold;\">&#8593;</span>")
  
  ## for test since up is good
  arrows_2 <- c("<span style=\"color:#FF0034;font-weight: bold;\">&#8595;</span>",
                "<span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span>",
                "<span style=\"color:#01D474;font-weight: bold;\">&#8593;</span>")
  
  #no_arrow <- "<span style=\"color:#ffffff00;font-weight: bold;\">&#8596;</span>"
  no_arrow <- ""
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
      prettyNum(round(tes$people_total_week[i] / 7), big.mark = ","),
      prettyNum(round(hos$HospitCOV19[i]), big.mark = ","),
      dynamic_round(mor$mort_week_avg[i]),
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
  
  ## pick recent weekly average for caseswith highest value
  ## we do this because of incomplete data, the highest is likely the most accurate
  
  casos <- paste(round(cas$cases_week_avg[1]),  arrows[change_cas[1] + 2]) ## keep same arrow as latest
  cas <- slice(cas, -1)
  change_cas <- change_cas[-1]
  
  hosp <- paste(prettyNum(hos$HospitCOV19[1], big.mark = ","), arrows[change_hos[1]+2])
  hos <- slice(hos, -1)
  change_hos <- change_hos[-1]
  
  vacunas <- paste(make_pct(vac$pct_fully_vaccinated[1]),  no_arrow)
  una_dosis <- paste(make_pct(vac$pct_one_dose[1]),  no_arrow)
  
  vacs_per_day <- diff(vac$people_fully_vaccinated[c(1,3)])/diff(as.numeric(vac$date[c(1,3)]))
  
  tmp <- round((pr_pop*0.7 - vac$people_fully_vaccinated[1]) / vacs_per_day)
  dias_hasta_meta_vacunas <- paste0(
    prettyNum(tmp, big.mark = ","), format(today()+tmp, " (%b %d)"))
    
  vac <- slice(vac, -1)
  
  meta <- c("< 3.0%", "< 2.0%",  "< 30",  "> 4,500", "< 300", "< 1", "> 70%")
  
  ## make the table
  tab <- tibble(metrica = c("Tasa de positividad (pruebas)", 
                            "Tasa de positividad (casos)",
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
                     paste0(format(pos$date[1]-days(6), "%b%d-"), format(pos$date[1], "%b%d")),
                     paste0(format(pos$date[2]-days(6), "%b%d-"), format(pos$date[2], "%b%d")),
                     paste0(format(pos$date[3]-days(6), "%b%d-"), format(pos$date[3], "%b%d")))
  
  
  return(list(tab = tab, 
              positividad = positividad, casos_positividad = casos_positividad, 
              casos = casos, hosp = hosp, una_dosis = una_dosis, vacunas = vacunas,
              dias_hasta_meta_vacunas = dias_hasta_meta_vacunas))
  
}

  

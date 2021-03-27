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
                           limits= c(100*min_rate, 100*max_rate)) +
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
    filter(new > 0 & diff >=0 & date >= start_date &  date <= end_date) %>%
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
                              pop_by_age,
                              start_date = first_day, 
                              end_date = last_complete_day, 
                              type = "Molecular", 
                              cumm = FALSE,
                              yscale = FALSE,
                              version = c("tp_pruebas", "tp_casos", "casos_per", "casos", "prop")){
  
  version <- match.arg(version)
  
  dat <- tests_by_age %>%
    filter(testType == type & !is.na(ageRange) &
             date >= start_date & date <= end_date ) %>%
    mutate(cases_rate_lower = get_ci_lower(cases_plus_negatives, cases_rate),
           cases_rate_upper = get_ci_upper(cases_plus_negatives, cases_rate)) %>%
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
             people_total_week = people_total) %>%
      ungroup()
    
    yscale <- FALSE
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
      mutate(the_stat = cases) %>%
      mutate(the_stat = make_pretty(cases)) %>%
      select(date, ageRange, the_stat)
    dat <- dat %>% 
      rename(the_stat = cases_week_avg, daily_stat = cases) 
    var_title <- "Casos únicos por día"
    the_ylim <- c(0, 225)
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
    the_ylim <- c(0, 30)
  }
  if(version ==  "prop"){
    dat <- dat %>% group_by(date) %>%
      mutate(the_stat = cases_week_avg/sum(cases_week_avg),
             daily_stat = cases/sum(cases)) %>%
      ungroup() 
    tab <- dat %>% 
      mutate(the_stat = make_pct(the_stat)) %>%
      select(date, ageRange, the_stat)
    var_title <- "Por ciento de casos"
    the_ylim <- c(0, .4)
    pct <- TRUE
  }
  
  tab <- tab %>%
    pivot_wider(names_from = ageRange, values_from = the_stat) %>% 
    arrange(desc(date)) %>%
    select(date, all_of(levels(dat$ageRange))) %>%
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
  
  if(pct) the_labels <- scales::percent else the_labels <- waiver()
  
  if(!yscale & !cumm){
    p <- dat %>% 
      ggplot(aes(date, the_stat)) + 
      xlab("Fecha") +
      ylab(var_title) +
      labs(color = "Edad") +
      labs(title = the_title, subtitle = the_subtitle) +
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month")) +
      scale_y_continuous(labels = the_labels) +
      facet_wrap(~ageRange, nrow = 2,  scales = "free_y") + 
      theme_bw() +
      theme(text = element_text(size = 15))  
    
    if(version %in% c("casos", "casos_per")){
      p <- p + 
        geom_bar(aes(y = daily_stat), stat = "identity", color = "#FBBCB2", fill = "#FBBCB2", width= 0.2, alpha =0.5, show.legend = FALSE) +
        geom_line(aes(lty = date > last_day), color = "#CC523A", show.legend = FALSE, size = 1.25) 
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
      scale_x_date(date_labels = "%b", breaks = breaks_width("1 month")) +
      theme_bw()
    
    if(!cumm){
      p <- p + scale_y_continuous(labels = the_labels, limits = the_ylim)
    } else{
      p <- p + scale_y_continuous(labels = the_labels)
    }
  }
  return(list(p = p, tab = tab, pretty_tab = pretty_tab))
}


### this is used to make the table in the front page
source("https://raw.githubusercontent.com/rafalab/pr-covid/master/coalicion/dashboard/functions.R")

  
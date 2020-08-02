# -- Set up
source("init.R")

shinyServer(function(input, output, session) {
    
  ## get date and time of latest update
  output$stamp = renderUI({
    load(file.path(rda_path,"data.rda"))
    HTML(paste("Actualización:<br>", the_stamp)) 
  })
  
  # -- This creates the positivity rate figure
  output$tasa_positividad <- renderPlot({
    load(file.path(rda_path,"data.rda"))
    
    xlim <- c(input$range[1]-days(1), input$range[2]+days(1))
    ylim <- with(filter(tests, date >= input$range[1], date <= input$range[2]),
                 c(min(expit(fit-z*se), rate), max(expit(fit+z*se), rate)))
    ret <- tests %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      ggplot(aes(date, rate)) +
      geom_hline(yintercept = 0.05, lty=2, color = "gray") +
      geom_point(aes(date, rate), size=2, alpha = 0.65) +
      geom_ribbon(aes(ymin= expit(fit - z*se), ymax = expit(fit + z*se)), alpha = 0.35) +
      geom_line(aes(y = expit(fit)), color="blue2", size = 0.80) +
      annotate("rect", xmin=pmax(xlim[1], today() - 8), xmax = pmin(today()+1, xlim[2]), ymin=-1, ymax=2, 
                fill="#ffcccb", alpha = 0.5) + 
      ylab("Tasa de positividad") +
      xlab("Fecha") +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(date_labels = "%B %d", expand = c(0, 0), limits = xlim)  +
      labs(title = "Tasa de Positividad en Puerto Rico", caption = str_wrap("Ojo: Interpreten los resultados de la última semana (en rojo) con cautela. Los resultados tardan en llegar lo cual resulta en más variabilidad dado a que hay pocas pruebas reportadas para los últimos 5-6 días.También es posible que haya un sesgo si los positivos se reportan más temprano que los negativos o si las pruebas se comienzan a restringir a asintomáticos.")) +
      theme_bw() +
      theme(plot.caption=element_text(hjust = 0))
    
    if(input$yscale){
      ret <- ret+ coord_cartesian(xlim = xlim, ylim = c(0, 0.25))
    } else{
      ret <- ret+ coord_cartesian(xlim = xlim, ylim = ylim)
    }
    return(ret)
    
  })
  
  # -- This creates the hospitalization figure
  output$hospitalizaciones <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    tmp <- hosp_mort %>% 
      filter(!is.na(HospitCOV19)) %>%
      filter(date >= input$range[1] & date <= input$range[2]) 
    
    if(input$yscale){
      max_y <- pmax(max(tmp$HospitCOV19, na.rm = TRUE), 7010)
    } else
    {
      max_y <- max(tmp$HospitCOV19, na.rm = TRUE)
    }
    min_date <- min(tmp$date)
    
    ret <- tmp %>% 
      ggplot(aes(date, HospitCOV19)) +
      geom_hline(yintercept = 6981, lty = 2, color="red") + 
      geom_bar(stat = "identity", width = 0.75, fill = "#8CC8F4") +
      ## geom_point(size = 2, alpha = 0.65) +
      ## geom_smooth(formula = y~x, method = "loess", span = 14/nrow(hosp_mort), size = 0.8, alpha = 0.35, 
      ##            level = 1 - alpha, method.args = list(degree = 1, family = "symmetric")) +
      xlab("Fecha") +
      ylab("Hospitalizaciones") +
      ggtitle("Hospitalizaciones actuales por COVID-19 en Puerto Rico") +
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(limits = c(0, max_y)) +
      theme_bw() 
    
    if(input$yscale) 
      ret <- ret +  annotate("text", x = min_date, y = 7010,  
                             label = "Total de camas en Hospitales = 6,981", 
                             vjust = 0, hjust = 0) 
    
    return(ret)
    
  })
  
  # -- This creates the ICU figure
  output$icu <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    tmp <- hosp_mort %>% 
      filter(!is.na(CamasICU)) %>%
      filter(date >= input$range[1] & date <= input$range[2]) 
    
    if(input$yscale){
      max_y <- pmax(max(tmp$CamasICU, na.rm = TRUE), 710)
    } else
    {
      max_y <- max(tmp$CamasICU, na.rm = TRUE)
    }
    min_date <- min(tmp$date)
    
    ret <- tmp %>% 
      ggplot(aes(date, CamasICU)) +
      geom_hline(yintercept = 691, lty = 2, color="red") + 
      geom_bar(stat = "identity", width = 0.75, fill = "#8CC8F4") +
      # geom_point(size = 2, alpha = 0.65) +
      # geom_smooth(formula = y~x, method = "loess", span = 14/nrow(hosp_mort), size = 0.8, alpha = 0.35, 
      #            level = 1 - alpha, method.args = list(degree = 1, family = "symmetric")) +
      xlab("Fecha") +
      ylab("Pacientes en ICU") +
      ggtitle("Pacientes en ICU actuales por COVID-19 en Puerto Rico") +
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(limits = c(0, max_y)) +
      theme_bw() 
    
    if(input$yscale) ret <- ret + annotate("text", x = min_date, y = 700, 
                                           label = "Total de camas en los ICU = 691",
                                           vjust = 0, hjust = 0) 
    
    return(ret)
    
  })
  
  # -- This resets the range
  observeEvent(input$reset, {
    updateDateRangeInput(session, "range",
                         start = "2020-03-21",
                         end   = today() - 1)
  })
  
  # -- This sets range to last two weeks
  observeEvent(input$weeks, {
    updateDateRangeInput(session, "range",
                         start = today() -1 - weeks(2),
                         end   = today() - 1)
  })
  
  # -- This creates the hospitalization figure
  output$muertes <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    hosp_mort %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      ggplot(aes(date)) +
      #geom_point(aes(y = IncMueSalud), size = 2, alpha = 0.65) +
      geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
      # geom_ribbon(aes(ymin= exp(fit - z*se), ymax = exp(fit + z*se)), alpha = 0.35) +
      geom_line(aes(y = exp(fit)), color="black", size = 1.25) +
      ylab("Muertes") +
      xlab("Fecha") +
      ggtitle("Muertes por COVID-19 en Puerto Rico") +
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(breaks = seq(0, 15, 1)) +
      theme_bw()
  })
  
  # -- This creates the daily number of tests figure
  output$numero_pruebas <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    tests %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      ggplot(aes(date, tests)) +
      geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
      geom_line(aes(y = fit_test), color = "#31347A", size = 1.25) +
      ylab("Pruebas") +
      xlab("Fecha") +
      ggtitle("Pruebas moleculares por día en Puerto Rico") +
      scale_x_date(date_labels = "%B %d") +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
  })
  
  # -- This creates the daily number of tests figure
  output$positivos_acumulados <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    tests %>% 
      mutate(positives = cumsum(positives)) %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      ggplot(aes(date, positives)) +
      geom_bar(stat = "identity", fill = "#FBBCB2", width= 0.75) +
      ggtitle("Pruebas positivas acumuladas en Puerto Rico") +
      ylab("Pruebas Positivas") +
      xlab("Fecha") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%B %d") +
      theme_bw()

  })
  
  #-- This creates the positivity rate map by municipio
  output$mapa_positividad <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    MAX <- 0.15 ## maximum positivity rate
    municipio_tests <- tests_by_strata %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      group_by(date, patientCity) %>%
      dplyr::summarize(positives = sum(positives),
                       tests     = sum(tests)) %>%
      ungroup() %>%
      group_by(patientCity) %>%
      dplyr::summarize(positives  = sum(positives),
                       tests      = sum(tests),
                       rate       = positives / tests) %>%
      ungroup() %>%
      mutate(rate = pmin(MAX, rate)) %>%
      na.omit() %>%
      mutate(lwr  = 100 * qbinom(alpha/2, tests, rate) / tests,
             upr  = 100 * qbinom(1 - alpha/2, tests, rate) / tests, 
             rate = 100 * rate)
    
    municipio_tests %>%
      {merge(map, .,by.x = "ADM1_ES", by.y = "patientCity", all.y = T)} %>%
      ggplot() +
      geom_sf(data = map, fill="gray", size=0.15) +
      geom_sf(aes(fill = rate), color="black", size=0.15) +
      geom_text(data = map, aes(X, Y, label = ADM1_ES),
                size  = 2.2,
                color = "black",
                fontface = "bold") +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                           name = "Tasa de Positividad:",
                           limits= c(0, MAX*100)) +
      theme_void() +
      theme(legend.position = "bottom") +
      ggtitle(paste("Tasa de positividad por municipio para el period de",   
                    format(input$range[1], "%B %d"),
                    "a",
                    format(input$range[2], "%B %d.")))
  })
  
  # -- This is used to print table in app
  output$tabla <- DT::renderDataTable({
    
    load(file.path(rda_path,"data.rda"))
    
    tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU)
    
    ret <- tests %>% left_join(tmp, by = "date") %>%
     filter(date >= input$range[1], date <= input$range[2]) %>%
      mutate(rate = format(round(rate, 2), nsmall=2),
             avg_7_day = paste0(format(round(100*expit(fit), 1), nsmall=1),
                                "% ",
                                "(", trimws(format(round(100*expit(fit - z*se), 1), nsmall=1)),"%", 
                                ", ",
                                format(round(100*expit(fit + z*se), 1), nsmall=1),"%", ")"),
             dummy = date,
             warning = as.numeric(date >= today()- days(7))) %>%
      select(date, avg_7_day, positives, tests, rate, IncMueSalud, CamasICU, HospitCOV19, dummy, warning) %>%
      arrange(desc(date)) %>%
      mutate(date = format(date, "%B %d")) %>%
      setNames(c("Fecha", "Tasa de positividad (IC)", "Positivos", "Pruebas", "Positivos/Pruebas",  
                 "Muertes", "ICU", "Hospitalizaciones", "dateorder", "warning"))
    
    ret <- DT::datatable(ret, class = 'white-space: nowrap',
                  caption = paste0("La columna con fechas contiene el día en que se hizo la prueba. Tasa de positividad es un estimado basado en la tendencia usando un método estadístico, llamado regresión por splines, parecido a tomar el promedio de los siete días alrededor de cada fecha. IC = Intervalo de confianza del ", (1-alpha)*100,"%. Ojo: Interpreten los resultados de la última semana (en rojo) con cautela. Los resultados tardan en llegar lo cual resulta en más variabilidad dado a que hay pocas pruebas reportadas para los últimos 5-6 días. También es posible que haya un sesgo si los positivos se reportan más temprano que los negativos o si las pruebas se comienzan a restringir a asintomáticos."),
    rownames = FALSE,
    options = list(dom = 't', pageLength = -1,
                   columnDefs = list(
                     list(targets = 0, orderData = 8),
                     list(targets = c(8,9), visible = FALSE),
                     list(className = 'dt-right', targets = 2:7)))) %>%
      DT::formatStyle(columns = 2:5,
                      valueColumns = "warning", 
                      backgroundColor = DT::styleEqual(c(0,1), c("#ffffff00", "#ffcccb")))
    return(ret)
  }, server = FALSE)
  
  
  output$municipios <- DT::renderDataTable({
    
    load(file.path(rda_path,"data.rda"))
    
    tmp <- tests_by_strata %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      mutate(patientCity = as.character(patientCity))
    
    children <- tmp %>%
      filter(as.numeric(ageRange) <= 2) %>%
      group_by(patientCity, ageRange) %>%
      summarize(value = sum(positives)) %>%
      ungroup() %>%
      spread(ageRange, value)
    
    ndays <- as.numeric(diff(input$range))+1
    
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
             poblacion = prettyNum(poblacion, big.mark=",")) %>%
      select(patientCity, rate, positives, tests, ppc, poblacion, `0 to 9`, `10 to 19`) %>%
      setNames(c("Municipio", "Tasa de positividad (IC)", "Positivos", "Pruebas",  "Positivos por\n100,000 por día", "Población", "Positiovs 0 a 9 años", "Positivos 10 a 19 años"))
    
    ret <- DT::datatable(ret, class = 'white-space: nowrap',
                         caption = paste0("Tasa de positividad es un estimado basado en periodo ", 
                                          format(input$range[1], "%B %d"),
                                          " a ",
                                          format(input$range[2], "%B %d"),
                                          ". IC = Intervalo de confianza del ", (1-alpha)*100,"%."),
                         rownames = FALSE,
                         options = list(dom = 't', pageLength = -1,
                                        columnDefs = list(
                                          list(className = 'dt-right', targets = 2:7)))) 
   
    return(ret)
  }, server = FALSE)
  
  output$age <- renderPlot({
    
    load(file.path(rda_path,"data.rda"))
    
    age_levels <- levels(tests_by_strata$ageRange)
    ret <- tests_by_strata %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      filter(ageRange != "No reportado") %>%
      mutate(ageRange = age_levels[ifelse(as.numeric(ageRange) >=9, 9, as.numeric(ageRange))]) %>%
      mutate(ageRange = ifelse(ageRange == "80 to 89", "80+", ageRange)) %>%
      group_by(ageRange) %>%
      summarize(positivos = sum(positives)) %>%
      ungroup() %>%
      mutate(percent = positivos/sum(positivos)) %>%
      ggplot(aes(ageRange, percent)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent) +
      xlab("Edad") +
      ylab("Porciento") +
      ggtitle(paste("Distribución de pruebas positivas por edad en Puerto Rico de", 
                     format(input$range[1], "%B %d"),
                     "a",
                     format(input$range[2], "%B %d."))) +
      theme_bw()
    
    if(input$yscale) ret <- ret+ coord_cartesian(ylim = c(0, 0.23))
    
    return(ret)
  })    
    
  # -- This allows users to download data
  output$downloadData <- downloadHandler(
    filename = function() {
      load(file.path(rda_path,"data.rda"))
      paste0("pruebas-",format(the_stamp, "%Y-%m-%d_%H:%M:%S"),".csv")
    },
    content = function(file) {
      all_tests <- readRDS(file.path(rda_path,"all_tests.rds"))
      write.csv(all_tests, file = file, row.names = FALSE)  
    },
    contentType = "txt/csv"
  )
})




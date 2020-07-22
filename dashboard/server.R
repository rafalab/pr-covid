# -- Set up
source("init.R")

shinyServer(function(input, output, session) {
    
    # -- This creates the positivity rate figure
    output$tasa_positividad <- renderPlot({
       ret <- tests %>%
         filter(date >= input$range[1], date <= input$range[2]) %>%
         ggplot(aes(date, rate)) +
         geom_hline(yintercept = 0.05, lty=2, color = "gray") +
         geom_point(aes(date, rate), size=2, alpha = 0.65) +
         geom_ribbon(aes(ymin= expit(fit - z*se), ymax = expit(fit + z*se)), alpha = 0.35) +
         geom_line(aes(y = expit(fit)), color="blue2", size = 0.80) +
         ylab("Tasa de positividad") +
         xlab("Fecha") +
         ggtitle("Tasa de Positividad en Puerto Rico") +
         scale_y_continuous(labels = scales::percent) +
         scale_x_date(date_labels = "%B %d") +
         theme_bw()
       
       if(input$yscale) ret <- ret+ coord_cartesian(ylim = c(0, 0.25))
       
       return(ret)
         
    })
    
    # -- This creates the hospitalization figure
    output$hospitalizaciones <- renderPlot({
      
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
        geom_bar(stat = "identity") +
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
                               label = "Total de camas disponibles en Hospitales  = 6,981", 
                               vjust = 0, hjust = 0) 
        
      return(ret)
      
    })
   
    # -- This creates the ICU figure
    
    output$icu <- renderPlot({
      
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
        geom_bar(stat = "identity") +
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
                                            label = "Total de camas disponibles en los ICU  = 691",
                                            vjust = 0, hjust = 0) 
        
      return(ret)
      
    })
    
    
    # -- This creates the hospitalization figure
    output$muertes <- renderPlot({
      hosp_mort %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        ggplot(aes(date)) +
        geom_point(aes(y = IncMueSalud), size = 2, alpha = 0.65) +
        geom_ribbon(aes(ymin= exp(fit - z*se), ymax = exp(fit + z*se)), alpha = 0.35) +
        geom_line(aes(y = exp(fit)), color="blue2", size = 0.80) +
        ylab("Muertes") +
        xlab("Fecha") +
        ggtitle("Muertes por COVID-19 en Puerto Rico") +
        scale_x_date(date_labels = "%B %d") +
        scale_y_continuous(breaks = seq(0, 15, 1)) +
        theme_bw()
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas <- renderPlot({
        
        tests %>% 
        filter(date >= input$range[1], date <= input$range[2]) %>%
        group_by(date = ceiling_date(date, unit = "week", 
                                     week_start = wday(max(date)))) %>%
        dplyr::summarize(tests = sum(tests)) %>%
        ggplot(aes(date, tests)) +
        geom_bar(size=0.20, stat = "identity") +
        ggtitle("Número de Pruebas Semanales en Puerto Rico") +
        ylab("Número de pruebas") +
        xlab("Semana acabando en esta fecha") +
        scale_y_continuous(labels = scales::comma,
                           breaks = seq(0, 30000, by = 5000)) +
        scale_x_date(date_labels = "%B %d") +
        theme_bw()
        # ggplotly(displayModeBar = FALSE)
    
    })
    
     output$titulo_mapa <- renderUI({
       tagList(
         h4(paste("Tasa de positividad por municipio para el periodo de",   
               format(input$range[1], "%B %d"),
               "a",
               format(input$range[2], "%B %d.")))
       )
     })
     
     #-- This creates the positivity rate map by municipio
     output$mapa_positividad <- renderLeaflet({
       
       MAX <- 0.25 ## maximum positivity rate
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
       
       spatial_df <- municipio_tests %>%
         {merge(map, .,by.x = "ADM1_ES", by.y = "patientCity", all.y = T)} %>%
         filter(ADM1_ES != 'No reportado') %>%
         sf::st_as_sf() %>%
         mutate(rate = round(rate, 3),
                lwr = round(lwr, 3),
                upr = round(upr, 3))
       
       
       pal <- leaflet::colorQuantile("Reds", domain = spatial_df$rate, n = 5)
       labels <- glue::glue(
         "<strong>{spatial_df$ADM1_ES}</strong><br/>Tasa de Positividad: {spatial_df$rate} [{spatial_df$lwr}, {spatial_df$upr}] <br/>Casos Reportados: {spatial_df$positives} <br/> Pruebas Realizadas: {spatial_df$tests}"
       ) %>% lapply(htmltools::HTML)
       
       spatial_df %>% 
         leaflet::leaflet() %>%
         leaflet::setView(-66.3, 18.1, 9) %>%
         leaflet::addProviderTiles("Esri.WorldTerrain") %>%
         leaflet::addPolygons(
           fillColor = ~pal(spatial_df$rate),
           weight = 1,
           opacity = 1,
           color = "white",
           dashArray = "1",
           fillOpacity = 0.90,
           highlight = leaflet::highlightOptions(
             weight = 3,
             color = "#666",
             dashArray = "5",
             fillOpacity = 0.7,
             bringToFront = TRUE),
           label = labels,
           labelOptions = leaflet::labelOptions(
             style = list("font-weight" = "normal", 
                          "padding" = "3px 8px"),
             textsize = "15px",
             direction = "auto")) %>%
         leaflet::addLegend(pal = pal, 
                            values = ~spatial_df$rate, 
                            opacity = 0.7, 
                            title = "Percentilas de <br/>Tasas de Positividad",
                            position = "bottomright")
     })
     
     # -- This is used to print table in app
    output$tabla <- DT::renderDataTable(DT::datatable({
      tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU)
      
      ret <- tests %>% left_join(tmp, by = "date") %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        mutate(rate = format(round(rate, 2), nsmall=2),
               avg_7_day = paste0(format(round(100*expit(fit), 1), nsmall=1),
                                  "% ",
                                  "(", trimws(format(round(100*expit(fit - z*se), 1), nsmall=1)),"%", 
                                  ", ",
                                  format(round(100*expit(fit + z*se), 1), nsmall=1),"%", ")")) %>%
        select(date, avg_7_day, positives, tests, rate, IncMueSalud, CamasICU, HospitCOV19) %>%
        arrange(desc(date)) %>%
        mutate(date = format(date, "%B %d")) %>%
        setNames(c("Fecha",  "Tasa de positividad (IC)", "Positivos", "Pruebas", "Pos/\nPruebas",  
                   "Muertes", "ICU", "Hospitalizaciones"))
      return(ret)
    }), 
    class = 'white-space: nowrap',
    caption = paste0("La columna con fechas contiene el día en que se hizo la prueba. Tasa de positividad es un estimado basado en la tendencia. IC = Intervalo de confianza del ", (1-alpha)*100,"%."),
    rownames= FALSE,
    options = list(dom = 't', pageLength = -1,
                   columnDefs = list(list(className = 'dt-right', targets = 2:7)))
    )

    
    output$municipios <- DT::renderDataTable(DT::datatable({
      
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
        select(patientCity, positives, tests, rate, ppc, poblacion, `0 to 9`, `10 to 19`) %>%
        setNames(c("Municipio", "Positivos", "Pruebas", "Tasa de positividad (IC)", "Positivos por 100,000 por día", "Población", "Casos 0 a 9 años", "Casos 10 a 19 años"))
      
        return(ret)
    }), 
    caption = paste0("Tasa de positividad es un estimado basado en periodo ", 
                     format(input$range[1], "%B %d"),
                     " a ",
                     format(input$range[2], "%B %d"),
                     ". IC = Intervalo de confianza del ", (1-alpha)*100,"%."),
    rownames= FALSE,
    options = list(dom = 't', pageLength = -1))
    
    
    # -- This allows users to download data
    output$downloadData <- downloadHandler(
        filename = function() {
            load("rdas/all_tests.rda")
            paste0("pruebas-",format(attr(all_tests, "date"), "%Y-%m-%d_%H:%M:%S"),".csv")
        },
        content = function(file) {
            load("rdas/all_tests.rda")
            write.csv(all_tests, file, row.names = FALSE)  
        }
    )
})

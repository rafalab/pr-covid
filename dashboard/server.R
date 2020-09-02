# -- Set up
source("init.R")
load(file.path(rda_path,"data.rda"))

shinyServer(function(input, output, session) {
   
  # -- This resets the range
  observeEvent(input$reset, {
    updateDateRangeInput(session, "range",
                         start = "2020-03-21",
                         end   = last_day)
  })
  
  # -- This sets range to last two weeks
  observeEvent(input$weeks, {
    updateDateRangeInput(session, "range",
                         start = last_day - weeks(1),
                         end   = last_day)
  })
  
  # -- This sets range to last two weeks
  observeEvent(input$alldates, {
    updateDateRangeInput(session, "range",
                         start = first_day,
                         end   = today())
  })
  
  ## get date and time of latest update
  output$stamp = renderUI({
    HTML(paste("Actualización:<br>", the_stamp)) 
  })
  
  # -- This is used to print table in app
  output$tabla <- DT::renderDataTable({
    make_table(tests, cases, hosp_mort, 
               start_date = input$range[1], 
               end_date = input$range[2], 
               type = input$testType)
    }, server = FALSE)


  # -- This creates the positivity rate figure
  output$tasa_positividad <- renderPlot({
    plot_positivity(tests, start_date = input$range[1], 
                    end_date = input$range[2], 
                    type = input$testType, 
                    yscale = input$yscale)
  })
  
  # -- This creates the hospitalization figure
  output$hospitalizaciones <- renderPlot(
    plot_hosp(hosp_mort, 
              start_date = input$range[1],
              end_date = input$range[2])
  )
  
  # -- This creates the ICU figure
  output$icu <- renderPlot(
    plot_icu(hosp_mort, 
             start_date = input$range[1],
             end_date = input$range[2],
             yscale = input$yscale)
  )
  
  # -- This creates the hospitalization figure
  output$muertes <- renderPlot(
    plot_deaths(hosp_mort, 
                start_date = input$range[1],
                end_date = input$range[2],
                cumm = input$acumulativo)
  )
  
  # -- This creates the daily number of tests figure
  output$numero_pruebas <- renderPlot(
    plot_test(tests, 
              start_date =input$range[1], 
              end_date =input$range[2], 
              type = input$testType, 
              cumm = input$acumulativo)
  )
  # -- This creates the daily number of tests figure
  output$casos <- renderPlot(
    plot_cases(cases, 
               start_date = input$range[1], 
               end_date = input$range[2], 
               type =  input$testType,
               cumm = input$acumulativo)
  )
  
  # -- This creates a geographical table of positivity rate
  output$municipios <- DT::renderDataTable({
   make_municipio_table(test_by_strata,  
                        start_date =input$range[1], 
                        end_date =input$range[2], 
                        type = input$testType)
    }, server = FALSE)
      
  # -- This creates a geographical map of positivity rate
  output$mapa_positividad <- renderPlot(
    plot_map(test_by_strata,  
             start_date = input$range[1], 
             end_date = input$range[2],
             type =  input$testType)
  )
  
  output$age <- renderPlot(
    plot_agedist(tests_by_strata, start_date =input$range[1], 
                 end_date =input$range[2], 
                 type = input$testType,
                 yscale = input$yscale)
  )
})

#   # -- Resumen -----------------------------------------------------------------
#   output$resumen <- renderPlot({
#     load(file.path(rda_path,"data.rda"))
#     
#     p1 <-   tests %>%
#       filter(testType == input$testType &
#              date >= input$range[1] & date <= input$range[2]) %>%
#       ggplot(aes(date, tests)) +
#       geom_bar(stat = "identity", width = 0.75, fill = "#D1D1E8") +
#       geom_step(aes(y = tests_week_avg), color = "#31347A", size = 1.25) +
#       ylab("Pruebas") +
#       xlab("Fecha") +
#       ggtitle(paste("Pruebas", ifelse(input$testType=="Molecular", "moleculares", "serológicas"))) +
#       scale_x_date(date_labels = "%B %d") +
#       scale_y_continuous(labels = scales::comma) +
#       theme_bw()
#     
#     p2 <-  tests %>%
#       filter(testType == input$testType &
#              date >= input$range[1] & date <= input$range[2]) %>%
#       ggplot(aes(date, cases)) +
#       geom_bar(stat = "identity", fill = "#FBBCB2", width= 0.75) +
#       geom_line(aes(y = exp(cases_fit)), color = "#CC523A", size = 1.25) +
#       ylab("Casos únicos") +
#       xlab("Fecha") +
#       ggtitle("Casos") +
#       scale_x_date(date_labels = "%B %d") +
#       theme_bw()
#     
#     p3 <- hosp_mort %>% 
#       replace_na(list(HospitCOV19 = 0, CamasICU = 0)) %>%
#       filter(date >= input$range[1] & date <= input$range[2]) %>%
#       ggplot(aes(date, HospitCOV19)) +
#       geom_bar(stat = "identity", width = 0.75, fill = "#8CC8F4") +
#       geom_bar(aes(date, CamasICU), stat = "identity", width = 0.75, alpha = 0.5, fill = "darkblue") +
#       xlab("Fecha") +
#       ylab("Pacientes") +
#       ggtitle("Hospitalizaciones y ICU") +
#       scale_x_date(date_labels = "%B %d") +
#       theme_bw() 
#     
#   p4 <- hosp_mort %>%
#     filter(date >= input$range[1] & date <= input$range[2]) %>%
#     ggplot(aes(date)) +
#     geom_bar(aes(y = IncMueSalud), stat = "identity", width = 0.75, alpha = 0.65) +
#     geom_line(aes(y = exp(fit)), color="black", size = 1.25) +
#     ylab("Muertes") +
#     xlab("Fecha") +
#     ggtitle("Muertes") +
#     scale_x_date(date_labels = "%B %d") +
#     scale_y_continuous(breaks = seq(0, 15, 1)) +
#     theme_bw()
#   
#   gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)  
#   
# })
#   
#   

#   # -- This allows users to download data
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       load(file.path(rda_path,"data.rda"))
#       paste0("pruebas-",format(the_stamp, "%Y-%m-%d_%H:%M:%S"),".csv")
#     },
#     content = function(file) {
#       load(file.path(rda_path,"data.rda"))
#       write.csv(tests_by_strata, file = file, row.names = FALSE)  
#     },
#     contentType = "txt/csv"
#   )
# })
# 



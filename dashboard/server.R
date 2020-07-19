# -- Set up
source("init.R")

shinyServer(function(input, output, session) {
    
    
    
    # -- This creates the positivity rate figure
    output$tasa_positividad <- renderPlot({
       tests %>%
         filter(date >= input$range[1], date <= input$range[2]) %>%
         ggplot(aes(date, rate)) +
         geom_hline(yintercept = 0.05, lty=2, color="gray") +
         geom_point(aes(date, rate), size=2, alpha=0.40) +
         geom_ribbon(aes(ymin= expit(fit - z*se), ymax = expit(fit + z*se)), alpha=0.20) +
         geom_line(aes(y = expit(fit)), color="blue2", size=0.80) +
         ylab("Tasa de positividad") +
         xlab("Fecha") +
         ggtitle("Tasa de Positividad en Puerto Rico") +
         scale_y_continuous(labels = scales::percent) +
         coord_cartesian(ylim = c(0, 0.25)) +
         scale_x_date(date_labels = "%B %d") +
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
        geom_bar(color="black", fill="#252525", size=0.20, stat = "identity") +
        ggtitle("Número de Pruebas Semanales en Puerto Rico") +
        ylab("Número de pruebas") +
        xlab("Semana acabando en esta fecha") +
        scale_y_continuous(labels = scales::comma,
                           breaks = seq(0, 30000, by = 5000)) +
        scale_x_date(date_labels = "%B %d") +
        theme_bw()
        # ggplotly(displayModeBar = FALSE)
    
    })
    
    # -- This is used to print table in app
    output$tabla <- DT::renderDataTable(DT::datatable({
      ret <- tests %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        mutate(rate = paste0(format(round(100*rate,1), nsmall=1),"%"),
               avg_7_day = paste0(format(round(100*expit(fit), 1), nsmall=1),"%"),
               lower_ci = paste0(format(round(100*expit(fit - z*se), 1), nsmall=1),"%"),
               upper_ci = paste0(format(round(100*expit(fit + z*se), 1), nsmall=1),"%"))%>%
        select(date, positives, tests, rate, avg_7_day, lower_ci, upper_ci) %>%
        arrange(desc(date)) %>%
        mutate(date = format(date, "%B %d")) %>%
        setNames(c("Fecha", "Positivos", "Pruebas", "Tasa", "Promedio 7 dias", "LCI", "UCI"))
      return(ret)
    }), 
    rownames= FALSE,
    options = list(dom = 't', pageLength = -1))
    
    output$municipios <- DT::renderDataTable(DT::datatable({
      ret <- tests_by_strata %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        group_by(patientCity) %>%
        summarize(`0 to 9` = sum(positives[as.character(ageRange) == "0 to 9"]),
                  `10 to 19` = sum(positives[as.character(ageRange) == "10 to 19"]),
                  positives = sum(positives), tests = sum(tests),
                  rate =  positives/tests) %>%
        mutate(lower = qbinom(alpha/2, tests, rate) / tests,
               upper = qbinom(1 - alpha/2, tests, rate) / tests) %>%
        select(patientCity, positives, tests, rate, lower, upper, `0 to 9`, `10 to 19`) %>%
        arrange(desc(rate)) %>%
        mutate(rate = paste0(format(round(100*rate,1), nsmall=1),"%"),
               lower = paste0(format(round(100*lower, 1), nsmall=1),"%"),
               upper = paste0(format(round(100*upper, 1), nsmall=1),"%")) %>%
        setNames(c("Municipio", "Positivos", "Pruebas", "Tasa", "LCI", "UCI", "Casos 0 a 9 años", "Casos 10 a 19 años"))
      
        return(ret)
    }), 
    rownames= FALSE,
    options = list(dom = 't', pageLength = -1))
    
    
    # -- This allows users to download data
    output$downloadData <- downloadHandler(
        filename = function() {
            load("rdas/all_tests.rda")
            paste0("pruebas-",attr(all_tests,"date"),".csv")
        },
        content = function(file) {
            load("rdas/all_tests.rda")
            write.csv(all_tests, file, row.names = FALSE)  
        }
    )
})

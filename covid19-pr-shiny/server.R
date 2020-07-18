# -- Set up
source("init.R")

shinyServer(function(input, output, session) {
    
    ntext <- eventReactive(input$do, {
        "Hello World"
    })
    output$nText <- renderText({
        ntext()
    })
    
    # -- This is used to print table in app
    output$tabla <- DT::renderDataTable(DT::datatable({
        Sys.setlocale("LC_TIME", "es_ES")
        z <- qnorm(0.995)
        ret <- tests %>%
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
    options = list(pageLength = 30))
    
    # -- This creates the positivity rate figure
    output$tasa_positividad <- renderPlotly({
        z <- qnorm(0.995)
        tests %>%
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
        
        ggplotly()
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas <- renderPlotly({
        
        tests %>%
            group_by(date = ceiling_date(date, 
                                         unit = "week", 
                                         week_start = wday(max(date)))) %>%
            summarize(tests = sum(tests)) %>%
            ggplot(aes(date, tests)) +
            geom_bar(color="black", fill="#252525", size=0.20, stat = "identity") +
            ggtitle("Número de Pruebas Semanales en Puerto Rico") +
            ylab("Número de pruebas") +
            xlab("Semana acabando en esta fecha") +
            scale_y_continuous(labels = scales::comma,
                               breaks = seq(0, 30000, by = 5000)) +
            scale_x_date(date_labels = "%B %d") +
            theme_bw()
        
        ggplotly()
    
    })
    
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

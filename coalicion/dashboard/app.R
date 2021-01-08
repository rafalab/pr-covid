source("init.R")
library(shiny)

ui <- fixedPage(
    theme = "bootstrap.css",
    fixedRow(
        column(2, img(src = "coalicion.png", width = 75)),
        column(10, h4("Coalición Científica de Puerto Rico"), 
               h5("Informe Diario Sobre COVID19")
    )),

    br(),
    
    htmlOutput("fecha"),
    
    htmlOutput("rec"),
    
    br(),
    
    h5("Niveles actuales:"),
    
    htmlOutput("positividad"),
    
    br(),
    
    htmlOutput("tab_title"),
    
    DT::dataTableOutput("resumen_table"),
    
    br(),
    
    uiOutput("stamp"),
    
    HTML("<p>Ver datos completos <a href=\"https://rconnect.dfci.harvard.edu/covidpr/\"> aquí</a></p>"),
    
    dateInput(
        "the_day",
        "Cambiar fecha:",
        value = today() - days(1),
        min = make_date(2020, 5, 1),
        max = today() - days(1),
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 0,
        language = "es",
        width = "125px"
    )
    
)
 
server <- function(input, output, session) {
    
    load(file.path(rda_path,"data.rda"))
    
    ## adjust last week of positivity rate
    tests <- tests %>%
        mutate(fit = ifelse(date <= last_day, fit, estimate),
               lower = ifelse(date <= last_day, lower, estimate_lower),
               upper = ifelse(date <= last_day, upper, estimate_upper))
    
    
    output$fecha <- renderText({
        paste0("<h5> Fecha: ", format(input$the_day, "%Y, %B, %e"), "</h5>")
    })
    
    res <- reactive(compute_summary(tests, hosp_mort, cases, day = input$the_day))
    
    output$rec <- renderText({
        riesgo <- res()$riesgo
        paste0("<h5> Recomendación: <b>", 
               c("Apertura", "Mantener restricciones", "Más restricciones", "Lockdown")[riesgo], 
               "</b></h5>")
    })
    
    output$positividad <-  renderText({
        paste0("Tasa de Positividad: ", res()$positividad, "&emsp;", "Hospitalizaciones: ", res()$hosp, "\n")
    })
    
    output$tab_title <- renderText({
        paste0("<h5>Resumen basado en datos hasta ", 
               format(input$the_day - days(lag_to_complete), "%B %d, "), 
               "cuando estaban más completos:")
    })
    
    output$resumen_table <- DT::renderDataTable({
        res()$tab %>%
            DT::datatable(rownames = FALSE, escape = FALSE, 
                          options = list(dom = 't',
                                         ordering = FALSE,
                                         columnDefs = list(
                                             list(className = 'dt-center', targets = c(1,3,4,5)),
                                             list(className = 'dt-left', targets = c(0,2))
                                         )
                          )
            )
    }, server = FALSE)
    
    output$stamp = renderUI({
        HTML(paste("Actualización:", the_stamp)) 
    })
    
}
    
shinyApp(ui = ui, server = server)

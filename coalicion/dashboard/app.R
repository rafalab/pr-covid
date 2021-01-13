source("init.R")
library(shiny)

ui <- fixedPage(
    theme = "bootstrap.css",
    
    titlePanel("", windowTitle = "Informe Diario Sobre COVID19 de la Coalición Científica de Puerto Rico"),
    
    fixedRow(
        column(2, img(src = "coalicion.png", width = 75)),
        column(10, h4("Coalición Científica de Puerto Rico"), 
               h5("Informe Diario Sobre COVID19")
    )),

    br(),
    
    htmlOutput("fecha"),
    
    #htmlOutput("rec"),
    
    br(),
    
    h5("Niveles actuales:"),
    
    htmlOutput("positividad"),
    
    br(),
    
    htmlOutput("tab_title"),
    
    DT::dataTableOutput("resumen_table"),
    
    br(),
    
    HTML(paste("<p> Este informe es generado automaticamente usando los datos más recientes.",
               "Pueden ver el código <a href=\"https://github.com/rafalab/pr-covid/tree/master/coalicion/dashboard\" style=\"color:darkblue;\">aquí</a> y ",
               "datos completos <a href=\"https://rconnect.dfci.harvard.edu/covidpr/\" style=\"color:darkblue;\">aquí</a>.</p>")),
    
    uiOutput("stamp"),
    
    br(),
    
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
    ## currently not needed. need to monitor.
    # tests <- tests %>%
    #     mutate(fit = ifelse(date <= last_day, fit, estimate),
    #            lower = ifelse(date <= last_day, lower, estimate_lower),
    #            upper = ifelse(date <= last_day, upper, estimate_upper))
    # 
    
    output$fecha <- renderText({
        paste0("<h5> Fecha: ", format(input$the_day, "%Y, %B, %e"), "</h5>")
    })
    
    res <- reactive(compute_summary(tests, hosp_mort, cases, day = input$the_day))
    
    output$rec <- renderText({
        paste0("<h5> Recomendación: <b>", 
               c("Apertura", "Mantener restricciones", "Más restricciones", "Lockdown")[res()$riesgo], 
               "</b></h5>",
               "<h5> Razón: Nivel de casos <b>", c("bajo", "medio", "alto", "crítico")[res()$nivel],"</b>",
               ifelse(res()$riesgo == 4, 
                      ".",
                      ifelse(res()$nivel == 1,
                             c(" y están bajando.", " pero no están bajando.", " pero están subiendo.")[res()$tendencia+2],
                             c(" y están bajando.", " y no están bajando.", " y están subiendo.")[res()$tendencia+2])),
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
                                             list(className = 'dt-left', targets = c(0,2)),
                                             #list(className = 'dt-center', targets = c(1)),
                                             list(className = 'dt-right', targets = c(1,3,4))
                                         )
                          )
            )
    }, server = FALSE)
    
    output$stamp = renderUI({
        HTML(paste("Actualización:", the_stamp)) 
    })
    
}
    
shinyApp(ui = ui, server = server)

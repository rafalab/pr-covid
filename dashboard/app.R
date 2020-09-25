# -- Set up
source("init.R")

library(shiny)
library(shinythemes)

button_style <- "color: black; background-color: rgb(230, 220, 205); position: relative; 
                     text-align:center;border-radius: 6px; border-width: 2px"

# -- Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # -- Meta data
                tags$head(
                  tags$meta(name="description", content="Harvard COVID-19 Dashboard used to monitor Puerto Rico data"),
                  tags$meta(name="keywords", content="Puerto Rico, Dashboard, COVID-19"),
                  tags$meta(name="author", content="Rafael A. Irizarry")
                ),
                
                # -- Google analytics add on
                tags$head(includeHTML(("google-analytics.html"))),
                
                # Application title
                titlePanel("Monitoreo de COVID-19 en Puerto Rico"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput("range", "Periodo", 
                                   start = make_date(2020, 3, 21), 
                                   end = last_day,
                                   min = first_day,
                                   format = "M-dd",
                                   max = today()),
                    
                    actionButton("weeks", "Última Semana", 
                                 style = button_style),
                    br(),br(),
                    actionButton("alldates", "Todas las fechas", 
                                 style = button_style),
                    br(), br(),
                    radioButtons("testType", 
                                 label = "Tipo de prueba",
                                 choices = list("Molecular" = "Molecular",
                                                "Serológica" = "Serological"),
                                 selected = "Molecular"),
                    radioButtons("acumulativo", 
                                 label = "Tipo de gráfico",
                                 choices = list("Diario" = FALSE,
                                                "Acumulativo" = TRUE),
                                 selected = FALSE),
                    
                    radioButtons("yscale", 
                                 label = "Rango del gráfico:",
                                 choices = list("Preescogido" = TRUE,
                                                "Determinado por datos" = FALSE),
                                 selected = TRUE),
                    div("Datos depurados:"),
                    downloadButton("downloadData", "Download",
                                   style = button_style),
                    
                    br(), 
                    br(), 
                    
                    uiOutput("stamp"),
                    width = 3),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    
                    tabsetPanel(
                      
                      tabPanel("Resumen",
                               htmlOutput("riesgo"),
                               h3("Estimados actuales"),
                               DT::dataTableOutput("resumen_table"),
                               HTML(paste0("<h5>Estimados de la tasa de positividad y casos por día se calculan para <b>",
                                          format(last_day, "%B %d</b>."), " Noten que los datos de las pruebas toman 6 días en estar aproximadamente completos. ",
                                          "La <b>tasa de positividad</b> se define como el número de personas con prueba positiva divido entre el total de personas que se han hecho pruebas. ",
                                          "<b>Casos</b> y <b>pruebas</b> están basados en un <b>promedio de siete días</b> para contrarrestar el efecto que tiene el día de la semana en los totales. ",
                                          "El <b>uso de camas ICU</b> es el porcentaje de camas disponibles, no usadas por otras causas, usadas por pacientes de COVID-19. Hay alrededor de 700 camas ICU en Puerto Rico. Típicamente, sin COVID, hay alrededor de 275 displonibles.",
                                          "La <b>tendencia</b> es el cambio porcentual cuando comparamos estos estimados a los de la semana anterior. Si dividimos la tendencia para casos por 100 y añadimos uno, nos da un estimado del <b>Número de Transmisión Rt</b>. ",
                                          "<b>Noten que todos estos son estimados con variabilidad estadística</b>. Los gráficos nos dan una idea de esta variabilidad.</h5>")),
                               HTML(paste0("<h5> Los niveles de riesgo los dividimos como crítico, alto, medio, y bajo. ",
                                          "Entramos en <b>nivel crítico</b> cuando el uso de camas ICU sobrepasa 70%, lo cual indica que los hospitales pronto no podrán recibir más pacientes en condiciones críticas. ",
                                          "Entramos en <b>nivel alto</b> si la tasa de positividad o casos por días sobrepasan los niveles metas, lo cual indica que la situación no mejorará sin intervenciones o cambio de comportamiento. ",
                                          "Si se alcanzan estas metas, entramo en <b>nivel medio</b> lo cual indica poco riesgo actual pero, como todavía hay casos, continuamos monitoreando. ",
                                          "Entramos en <b>nivel bajo</b> cuando prácticamente desaparece la enfermedad. Estás definiciones pueden cambiar mientras sigamos aprendiendo.")),
                               h3("Resumen gráfico"),
                               plotOutput("resumen_plots")),
                      
                      tabPanel("Datos diarios",
                               DT::dataTableOutput("tabla")),
                      
                      tabPanel("Positividad",
                               plotOutput("tasa_positividad")),
                      
                      tabPanel("Hospitalizaciones",
                               plotOutput("hospitalizaciones")),
                      
                      tabPanel("ICU",
                               plotOutput("icu")),
                      
                      tabPanel("Muertes",
                               plotOutput("muertes")),
                      
                      tabPanel("Pruebas",
                               plotOutput("numero_pruebas")),
                      
                      tabPanel("Casos",
                               plotOutput("casos")),
                      
                      tabPanel("Municipios",
                               DT::dataTableOutput("municipios")),
                      
                      tabPanel("Mapa",
                               plotOutput("mapa_positividad")),
                      
                      tabPanel("Por Edad",
                               plotOutput("age")),
                      
                      tabPanel("Rezago",
                              plotOutput("rezago"))
                      
                    )
                  )),
                hr(),
                div(class = "footer", includeHTML("footer.html")
                )
)

server <- function(input, output, session) {
  
  load(file.path(rda_path,"data.rda"))
  
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
  

# Nivel de riesgo ---------------------------------------------------------

  output$riesgo <-  renderText({
    riesgo <- compute_summary(tests, hosp_mort, cases)$riesgo
    paste0("<h3> Nivel de riesgo: ", c("Bajo","Medio", "Alto", "Crítico")[riesgo],
           "&nbsp<div style = \"height: 25px; width: 25px; background-color: ",
          c("#01D474", "#FFC900", "#FF9600", "#FF0034")[riesgo],
          "; border-radius: 50%; display: inline-block; position:absolute\"></div></h3>") 
          
  })
  
  # -- This shows a summary
  output$resumen_table <- DT::renderDataTable({
    compute_summary(tests, hosp_mort, cases)$tab %>%
      DT::datatable(class = 'white-space: nowrap',
                    rownames = FALSE,
                    options = list(dom = 't', ordering = FALSE, pageLength = -1, 
                                   columnDefs = list(
                                     list(className = 'dt-center', targets = 0:3)))) %>%
      DT::formatStyle(columns = 1:4, fontSize = '125%')
  }, server = FALSE)
  
  
  output$resumen_plots <- renderPlot({
    p1 <- plot_positivity(tests, 
                          start_date = input$range[1],  end_date = input$range[2], 
                          type = input$testType, yscale = input$yscale)
    p2 <- plot_icu(hosp_mort, 
                    start_date = input$range[1], end_date = input$range[2],
                    yscale = input$yscale)
    p3 <-  plot_cases(cases, 
                      start_date = input$range[1], end_date = input$range[2], 
                      type =  input$testType, cumm = input$acumulativo)
    p4 <- plot_test(tests, 
                    start_date = input$range[1], end_date = input$range[2], 
                    type =  input$testType, cumm = input$acumulativo)
    p <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    return(p)
  })    
  
  # -- This is used to print table in app
  output$tabla <- DT::renderDataTable({
    make_table(tests, cases, hosp_mort, 
               start_date = input$range[1], 
               end_date = input$range[2], 
               type = input$testType)
  }, server = FALSE)
  
  
  # -- This creates the positivity rate figure
  output$tasa_positividad <- renderPlot(
    plot_positivity(tests, start_date = input$range[1], 
                    end_date = input$range[2], 
                    type = input$testType, 
                    yscale = input$yscale)
  )
  
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
    make_municipio_table(tests_by_strata,  
                         start_date =input$range[1], 
                         end_date =input$range[2], 
                         type = input$testType)
  }, server = FALSE)
  
  # -- This creates a geographical map of positivity rate
  output$mapa_positividad <- renderPlot(
    plot_map(tests_by_strata,  
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
  
  output$rezago <- renderPlot({
    load(file.path(rda_path,"rezago.rda"))
    plot_rezago(rezago, 
                start_date = input$range[1], 
                 end_date =input$range[2], 
                 type = input$testType)
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
}



shinyApp(ui = ui, server = server)




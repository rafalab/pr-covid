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
    
    tabsetPanel(
        
        tabPanel("Datos",
    
                 #htmlOutput("rec"),
    
                 br(),
    
                 h5("Niveles actuales: "),
                 htmlOutput("positividad"),
                 htmlOutput("fecha"),
                 
                 br(),
                 
                 htmlOutput("tab_title"),
                 
                 DT::dataTableOutput("resumen_table"),
                 
                 br(),
                 
                 fixedRow(
                     column(6, selectInput("testType", 
                                           label = "Tipo de prueba",
                                           choices = list("Molecular" = "Molecular",
                                                          "Molecular + Antígeno" = "Molecular+Antigens",
                                                          "Antígeno" = "Antigens",
                                                          "Serológica" = "Serological"),
                                           selected = "Molecular")),
                     column(6, dateInput(
                         "the_day",
                         "Fecha:",
                         value = today() - days(1),
                         min = make_date(2020, 5, 1),
                         max = today() - days(1),
                         format = "yyyy-mm-dd",
                         startview = "month",
                         weekstart = 0,
                         language = "es",
                         width = "125px"
                     ))
                 ),
                 HTML(paste("<p> Este informe es generado automaticamente usando los datos más recientes.",
                            "Pueden ver el código <a href=\"https://github.com/rafalab/pr-covid/tree/master/coalicion/dashboard\" style=\"color:darkblue;\">aquí</a> y ",
                            "datos completos <a href=\"https://rconnect.dfci.harvard.edu/covidpr/\" style=\"color:darkblue;\">aquí</a>.</p>")),
                 
                 uiOutput("stamp")
        ),
        tabPanel("Explicación", 
                 div(HTML(paste0(
                     "<br>",
                     "<h4> Tasas </h4>",
                     "<p> Las tasas se calculan usando los siguientes totales semanales:</b>",
                     "<UL>",
                     "<LI><b>Personas</b> = personas que se hicieron pruebas.</LI>
                     <LI><b>Positivos</b> = personas que salieron positivo.</LI>
                     <LI><b>Negativos</b> = personas que salieron negativo.</LI>",
                     "<LI><b>Casos</b> = casos nuevos únicos, o sea personas que salieron positivo por primera vez esa semana.</LI>",
                     "</UL><p>Se calculan en semanas para evitar el efecto que tiene el día de la semana. ",
                     "Las tasas entonces se definen así:</p>",
                     "<UL>",
                     "<LI><b>% pruebas positivas</b> = <b>Positivos</b> / <b>Personas</b>. </LI>",
                     "<LI><b>% casos nuevos:</b> <b>Casos</b> / (<b>Casos</b> + <b>Negativos</b>) </LI>",
                     "</UL>",
                     "<p>La primera es parecida a la tasa de positividad que usa la CDC. La diferencia es que removemos duplicados dentro de cada semana. ",
                     "Noten que la primera tasa es más alta que la segunda debido a que muchos se hacen pruebas en múltiples semanas y por lo tanto ",
                     "no todos los <b>Positivos</b> son <b>Casos</b> nuevos.</p>",
                     "<p>Importante notar que estas tasas <b>no son estimados del por ciento de la población que está infectada</b> ",
                     "ya que las personas que se hacen pruebas no son para nada representativas de la población. ",
                     "Son útiles y se monitorean porque suben cuando suben los casos o cuando no se hacen suficientes pruebas.</p>",
                     "<h4>Tendencias</h4>",
                     "<p>Las flechas de colores muestran la tendencia de cada métrica. Comparamos cada semana con la semana anterior ",
                     "y llevamos a cabo una prueba de significancia estadística.</p>",
                     "<UL>",
                     "<LI> <span style=\"color:#01D474;font-weight: bold;\">&#8595;</span> = Disminución estadísticamente significativa.</LI>
                     <LI><span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span> = No hay cambio estadísticamente significativo.</LI>
                     <LI><span style=\"color:#FF0034;font-weight: bold;\">&#8593;</span> = Aumento estadísticamente significativo.</LI>
                     </UL>",
                     "<p> Los colores indican la tendencia que deseamos ver (verde) y la que no (rojo). El total de pruebes es la única métrica que queremos ver subir.",
                     "<h4> Otros resúmenes semanales:</h4>",
                     "<UL>",
                     "<LI><b>Casos nuevos por día</b> = Promedio diario de casos únicos esa semana.</LI>
                     <LI><b>Pruebas por día</b> = Promedio diario de personas que se hicieron la prueba esa semana.</LI>
                     <LI><b>Hospitalizaciones</b> = Número de hospitalizaciones reportados por salud el último día de la semana.</LI>
                     <LI><b>Muertes por día</b> = Promedio diario de muertes reportadas por salud esa semana.</LI></UL>"
                 )))
        ),
        tabPanel("Disclaimer",
                 br(),
                 HTML("<p> APIs provistos por Departamento de Salud: <a href=\"https://github.com/rafalab/pr-covid/blob/master/dashboard/apis.md\">https://github.com/rafalab/pr-covid/blob/master/dashboard/apis.md</a></p>"),
                 p("Los datos provistos por el Departamento de Salud por medio de estos APIs son en su mayoría recopilados de forma electrónica, y el resto de forma manual, por lo que es dinámico y continuamente sujeto a cambios. Además, dado a que en el Departamento de Salud se utilizan varios métodos de depuración de datos para una identificación óptima de casos únicos, los datos aquí provistos difieren ligeramente de los números oficiales. Estas diferencias se deben a que en el Bioportal puede haber algunos casos duplicados y/o con información incorrecta, que con el tiempo el equipo del Departamento de Salud va trabajando y limpiando.")
        )
    ),
    hr(),    
    br(),
    
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
        paste0("Fechas: ", 
               format(input$the_day, "%Y, %B, %e -"), 
               format(input$the_day, "%Y, %B, %e"))
    })
    
    res <- reactive(compute_summary(tests, hosp_mort, day = input$the_day, type = input$testType))
    
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
        paste0(
            "% pruebas positivas: ", res()$positividad, "&emsp;", 
            "% casos nuevos: ", res()$casos_positividad, "&emsp;", 
            "Hospitalizaciones: ", res()$hosp, "\n")
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
                                             list(className = 'dt-left', targets = c(0,1)),
                                             #list(className = 'dt-center', targets = c(1)),
                                             list(className = 'dt-right', targets = c(2,3,4))
                                         )
                          )
            )
    }, server = FALSE)
    
    output$stamp = renderUI({
        HTML(paste("Actualización:", the_stamp)) 
    })
    
}
    
shinyApp(ui = ui, server = server)

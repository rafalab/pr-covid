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
                 br(),
                 htmlOutput("fecha"),
                 
                 br(),
                 
                 htmlOutput("tab_title"),
                 
                 DT::dataTableOutput("resumen_table"),
                 
                 br(),
                 
                 HTML("<p>Ver pestaña <span style=\"color:#18bc9c;\">Explicación</span> para detalles sobre estos resúmenes.</p>"),
                 
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
                         value = last_complete_day,
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
                            "Pueden ver el código <a href=\"https://github.com/rafalab/pr-covid/tree/master/coalicion/dashboard\" style=\"color:darkblue;\">aquí</a>, ",
                            "datos completos <a href=\"https://rconnect.dfci.harvard.edu/covidpr/\" style=\"color:darkblue;\">aquí</a>, e",
                            "informes previos <a href=\"https://rafalab.github.io/pr-covid/\" style=\"color:darkblue;\">aquí</a>.</p>")),
                 
                 uiOutput("stamp")
        ),
        tabPanel("Explicación", 
                 div(HTML(paste0(
                     "<br>",
                     "<h4> Tasas de Positividad</h4>",
                     "<p> Las tasas se calculan usando los siguientes totales semanales:</b>",
                     "<UL>",
                     "<LI><b>Personas</b> = personas que se hicieron pruebas.</LI>
                     <LI><b>Positivos</b> = personas que salieron positivo.</LI>
                     <LI><b>Negativos</b> = personas que salieron negativo.</LI>",
                     "<LI><b>Casos</b> = casos nuevos únicos, o sea personas que salieron positivo por primera vez esa semana.</LI>",
                     "</UL><p>Usamos totales semanales en vez de diarios, para evitar el efecto que tiene el día de la semana. Por ejemplo, los domingos se hacen muchas menos pruebas. ",
                     "Las tasas entonces se definen así:</p>",
                     "<UL>",
                     "<LI><b>% pruebas positivas</b> = <b>Positivos</b> / <b>Personas</b></LI>",
                     "<LI><b>% casos nuevos = </b> <b>Casos</b> / (<b>Casos</b> + <b>Negativos</b>)</LI>",
                     "</UL>",
                     "<p>La primera es parecida a la tasa de positividad que usa la CDC. La diferencia es que removemos duplicados dentro de cada semana para evitar el posible efecto de duplicados causados por errores de entrada de datos. ",
                     "Noten que la primera tasa es más alta que la segunda debido a que muchos se hacen pruebas en múltiples semanas y por lo tanto ",
                     "no todos los <b>Positivos</b> son <b>Casos</b> nuevos.</p>",
                     "<p>Importante notar que estas tasas <b>no son estimados del por ciento de la población que está infectada</b> ",
                     "ya que las personas que se hacen pruebas no son para nada representativas de la población. ",
                     "Son útiles y se monitorean porque suben cuando suben los casos o cuando no se hacen suficientes pruebas. ",
                     "Por ejemplo, desde abril, hemos observados menos de 1 muerte por día solo durante periodos con tasa de positividad < 3%.</p>",
                     "<h4>Tendencias</h4>",
                     "<p>Las flechas de colores muestran la tendencia de cada métrica. Comparamos cada semana con la semana anterior ",
                     "y llevamos a cabo una prueba de significancia estadística.</p>",
                     "<UL>",
                     "<LI><span style=\"color:#01D474;font-weight: bold;\">&#8595;</span> = Disminución estadísticamente significativa.</LI>
                     <LI><span style=\"color:#FFC900;font-weight: bold;\">&#8596;</span> = No hay cambio estadísticamente significativo.</LI>
                     <LI><span style=\"color:#FF0034;font-weight: bold;\">&#8593;</span> = Aumento estadísticamente significativo.</LI>
                     </UL>",
                     "<p> Los colores indican la tendencia que deseamos ver (verde) y la que no (rojo). El total de pruebes es la única métrica que queremos ver subir.",
                     "<h4> Otros resúmenes semanales:</h4>",
                     "<UL>",
                     "<LI><b>Casos nuevos por día</b> = Promedio diario de casos únicos <b>detectados</b> esa semana.</LI>
                     <LI><b>Pruebas por día</b> = Promedio diario de personas que se hicieron la prueba esa semana.</LI>
                     <LI><b>Hospitalizaciones</b> = Número de hospitalizaciones reportados por salud el último día de la semana. La tendencia se calcula para una media móvil de 7 días que no mostramos en la tabla.</LI>
                     <LI><b>Muertes por día</b> = Promedio diario de muertes reportadas por salud esa semana. </LI>
                     <LI><b>% población vacunada</b> = Por ciento de la población de Puerto Rico que ha recibido ambas dosis de la vacuna. </LI>
                     <LI><b>Días para alcanzar 70%</b> = Basado en la tasa de crecimiento de la última semana, el número de días que tardará llegar a la meta de 70% de la población vacunada con amabas dosis.</LI>
                     <LI><b>% por lo menos 1 dosis</b> = Por ciento de la población de Puerto Rico que ha recibido por lo menos una dosis de la vacuna.</LI>
                     </UL>",
                     "<p> Importante notar que no se detectan todos los casos y que cuántos detectamos depende de cuántas pruebas se hacen."                 )))
        ),
        tabPanel("Disclaimer",
                 br(),
                 p("Con la excepción de los datos de las vacunas, los datos incluidos en esta página son recopilados por el Departamento de Salud. Los mismos son generados automáticamente usando los datos más recientes recopilados. Sin embargo, los mismos pueden diferir de los datos oficiales publicados por otros medios oficiales del Departamento de Salud.  Los datos incluidos deben solo ser utilizados para propósitos informativos e ilustrativos."),
                 p("Ni el Departamento de Salud, ni la Coalición Científica de Puerto Rico, ni el Gobierno de Puerto Rico son responsables de cualquier daño causado por la información publicada en esta página."),
                 HTML("<p>Los datos de las vacunas los obtenemos de <a href=\"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv\">Our World in Data</a></p>.")
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
        paste0("<p>Fechas: ", 
               format(input$the_day-days(6), " %B/%d/%Y - "), 
               format(input$the_day, "%B/%d/%Y"),"</p>")
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
            "<table cellpadding=\"100\" cellspacing=\"100\">",
            "<tr><td>% pruebas positivas:</td><td align=\"right\">&emsp;", res()$positividad, "</td></tr>", 
            "<tr><td>% casos nuevos:</td><td align=\"right\">&emsp;", res()$casos_positividad, "</td></tr>", #, "&emsp;", 
            "<tr><td>Hospitalizaciones:</td><td align=\"right\">&emsp;", res()$hosp, "</td></tr>",
            "<tr><td>% población vacunada:</td><td align=\"right\">&emsp;", res()$vacunas, "</td></tr>",
            "<tr><td>Días para alcanzar 70%:</td><td align=\"right\">&emsp;", res()$dias_hasta_meta_vacunas, "</td></tr>",
            "<tr><td>% por lo menos 1 dosis:</td><td align=\"right\">&emsp;", res()$una_dosis, "</td></tr></table>")
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

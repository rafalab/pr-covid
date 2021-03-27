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
                                   start = last_complete_day - days(89), 
                                   end = last_complete_day,
                                   min = first_day,
                                   format = "M-dd",
                                   max = today(),
                                   language = "es",
                                   width = "100%"),
                    
                    actionButton("weeks", "Última semana", 
                                 style = button_style),
                    br(),br(),
                    
                    actionButton("months", "Últimos 90 días", 
                                 style = button_style),
                    br(),br(),
                    
                    actionButton("alldates", "Todas las fechas", 
                                 style = button_style),
                    br(), br(),
                    radioButtons("testType", 
                                 label = "Tipo de prueba",
                                 choices = list("Molecular" = "Molecular",
                                                "Molecular + Antígeno" = "Molecular+Antigens",
                                                "Antígeno" = "Antigens",
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
                    
                    selectInput("dataset", "Datos depurados:",
                                choices = c("Todas las pruebas" = "pruebas",
                                            "Casos por día" = "casos", 
                                            "Muertes y hospitalizaciones" = "hosp-mort",
                                            "Casos, positivos y pruebas por día" = "positivos",
                                            "Positivos y pruebas por municipio por día" = "municipios",
                                            "Positivos y pruebas por edad por día" = "edad",
                                            "Positivos y pruebas por municipio/edad por día" = "municipios-edad",
                                            "Positivos por laboratorio" = "labs",
                                            "Pruebas por laboratorio" = "labs_pruebas",
                                            "Rezago" = "rezago")),
                    
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
                               
                               h4("Niveles actuales: "),
                               htmlOutput("positividad"),
                               htmlOutput("fecha"),
                               br(),
                               htmlOutput("table_title"),
                               DT::dataTableOutput("resumen_table"),
                               htmlOutput("table_caption"),
                               hr(),
                               h4("Resumen gráfico:"),
                               plotOutput("resumen_plots")),
                    
                
                      tabPanel("Datos diarios",
                               DT::dataTableOutput("tabla"),
                               br(),
                               downloadButton("downloadTable", "Download",
                                              style = button_style)),
                      
                      tabPanel("Positividad",
                               h4("Tasa de positividad"),
                               radioButtons("pos_version", 
                                            label = "",
                                            choices = list("Pruebas sobre pruebas" = "pruebas",
                                                           "Casos sobre personas" = "casos"),
                                            selected = "pruebas",
                                            inline = TRUE),
                               plotOutput("tasa_positividad"),
                               hr(),
                               h5("Explicación sobre las diferentes definiciones de Tasa de Positividad"),
                               DT::dataTableOutput("tabla_positividad")),
                      
                      tabPanel("Hospitalizaciones",
                               plotOutput("hospitalizaciones")),
                      
                      tabPanel("ICU",
                               plotOutput("icu")),
                      
                      tabPanel("Muertes",
                               plotOutput("muertes")),
                      
                      tabPanel("Pruebas",
                               plotOutput("numero_pruebas"),
                               hr(),
                               plotOutput("positividad_por_lab"),
                               hr(),
                               plotOutput("numero_pruebas_por_lab")),
                      
                      
                      tabPanel("Casos",
                               plotOutput("casos")),
                      
                      tabPanel("Regiones",
                               radioButtons("by_region_version", 
                                            label = "",
                                            choices = list("Tasa de positividad (pruebas)" = "tp_pruebas",
                                                           "Tasa de positividad (casos)" = "tp_casos",
                                                           "Casos por 100,000" = "casos",
                                                           "Pruebas por 100,000" = "pruebas",
                                                           "Por ciento de pruebas" = "prop"),
                                            selected = "tp_pruebas",
                                            inline = TRUE),
                               plotOutput("plot_by_region"),
                               DT::dataTableOutput("table_by_region")),
                      
                      tabPanel("Municipios",
                               DT::dataTableOutput("municipios")),
                      
                      tabPanel("Mapa",
                               plotOutput("mapa_positividad")),
                      
                      tabPanel("Por Edad",
                               radioButtons("by_age_version", 
                                            label = "",
                                            choices = list("Tasa de positividad (pruebas)" = "tp_pruebas",
                                                           "Tasa de positividad (casos)" = "tp_casos",
                                                           "Casos por 100,000" = "casos_per",
                                                           "Casos" = "casos",
                                                           "Por ciento de casos" = "prop",
                                                           "Pruebas por 100,000" = "pruebas"),
                                            selected = "tp_pruebas",
                                            inline = TRUE),
                               plotOutput("plot_by_age"),
                               DT::dataTableOutput("table_by_age")),
                      
                      tabPanel("Rezago",
                              plotOutput("rezago")),
                      
                      tabPanel("Labs",
                               br(),
                               downloadButton("downloadLabData", "Download",
                                              style = button_style),
                               DT::dataTableOutput("labs")),
                      
                      tabPanel("Vacunas",
                               plotOutput("vaccines"),
                               DT::dataTableOutput("vaccines_table")),
                      
                      tabPanel("FAQ",
                               includeMarkdown("faq.md"))
                      
                    ), width = 9
                  ), position = "left"),
                hr(),
                div(class = "footer", includeHTML("footer.html"))
)

server <- function(input, output, session) {
  
  load(file.path(rda_path,"data.rda"))
  
  ## adjust last week of positivity rate
  ## Right now it is not necessary to adjust as + are coming at the same time as - 
  # tests <- tests %>%
  #   mutate(fit = ifelse(date <= last_day, fit, estimate),
  #          lower = ifelse(date <= last_day, lower, estimate_lower),
  #          upper = ifelse(date <= last_day, upper, estimate_upper))
  #          
  # -- This sets range to last two weeks
  observeEvent(input$weeks, {
    updateDateRangeInput(session, "range",
                         start = last_complete_day - days(6),
                         end   = last_complete_day)
  })
  
  # -- This sets range to last 90 days (default)
  observeEvent(input$months, {
    updateDateRangeInput(session, "range",
                         start = last_complete_day - days(89),
                         end   = last_complete_day)
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

#   output$riesgo <-  renderText({
#     riesgo <- compute_summary(tests, hosp_mort, cases, type = input$testType)$riesgo
#     paste0("<h3> Recomendación: ", c("Apertura","Flexibilización", "Más restricciones", "Lockdown")[riesgo],
#            "&nbsp<div style = \"height: 25px; width: 25px; background-color: ",
#            c("#01D474", "#FFC900", "#FF9600", "#FF0034")[riesgo],
#            "; border-radius: 50%; display: inline-block; position:absolute\"></div>", 
# "</h3>") 
#   })

  # Summary table title -----------------------------------------------------

  output$table_title <- renderText({
      paste0("<h4>Resumen basado en datos hasta ", 
             format(last_day, "%B %d:"), 
             "</h4>")
  })
    
  output$table_caption <- renderText({
    paste0("<h5> <b> Tasa de positividad</b>, ",
           "<b>casos</b> y <b>pruebas</b> están basados en pruebas ",
           case_when(input$testType == "Molecular" ~ "moleculares", 
                     input$testType == "Serological" ~ "serológicas",
                     input$testType == "Antigens" ~ "de antígenos",
                     input$testType == "Molecular+Antigens" ~ "moleculares y de antígenos"),
           
           ". Para estos indicadores y las muertes usamos <b>promedio de siete días</b> para contrarrestar el efecto que tiene el día de la semana. ",
           "Las flechas de colores no dicen si hubo cambio estadísticamente singificative cuando comparamos a la semana anterior. ",
           "Noten que los datos de las pruebas toman ", lag_to_complete, " días en estar aproximadamente completos. Ver pestaña <em>FAQ</em> para explicaciones mas detalladas.")
  })
  # -- This shows a summary
  res <- reactive(compute_summary(tests, hosp_mort, type = input$testType))
  
  output$positividad <-  renderText({
    paste0(
      "<table cellpadding=\"100\" cellspacing=\"100\">",
      "<tr><td>Tasa de positividad (pruebas):</td><td align=\"right\">&emsp;", res()$positividad, "</td></tr>", 
      "<tr><td>Tasa de positividad (casos):</td><td align=\"right\">&emsp;", res()$casos_positividad, "</td></tr>", #, "&emsp;", 
      "<tr><td>Hospitalizaciones:</td><td align=\"right\">&emsp;", res()$hosp, "</td></tr>",
      "<tr><td>% población vacunada:</td><td align=\"right\">&emsp;", res()$vacunas, "</td></tr>",
      "<tr><td>Días para alcanzar 70%:</td><td align=\"right\">&emsp;", res()$dias_hasta_meta_vacunas, "</td></tr>",
      "<tr><td>% por lo menos 1 dosis:</td><td align=\"right\">&emsp;", res()$una_dosis, "</td></tr></table>")
  })
  
  output$resumen_table <- DT::renderDataTable({
    compute_summary(tests, hosp_mort, type = input$testType)$tab %>%
      DT::datatable(class = 'white-space: nowrap',
                    rownames = FALSE,
                    escape = FALSE, 
                    options = list(dom = 't', ordering = FALSE, pageLength = -1, 
                                   columnDefs = list(
                                     list(className = 'dt-center', targets = 0:3)))) %>%
      DT::formatStyle(columns = 1:4, fontSize = '100%')
  }, server = FALSE)
  
  
  output$resumen_plots <- renderPlot({
    p1 <- plot_positivity(tests, 
                          start_date = input$range[1],  end_date = input$range[2], 
                          type = input$testType, yscale = input$yscale, version = "pruebas")
    p2 <- plot_deaths(hosp_mort, 
                    start_date = input$range[1], end_date = input$range[2],
                    cumm = input$acumulativo, yscale = input$yscale)
    p3 <-  plot_cases(cases, 
                      start_date = input$range[1], end_date = input$range[2], 
                      type =  input$testType, cumm = input$acumulativo,
                      yscale = input$yscale)
    p4 <-  plot_hosp(hosp_mort, 
                     start_date = input$range[1], end_date = input$range[2], 
                     yscale = input$yscale)
                     
    p <- gridExtra::grid.arrange(p1, p2, p3, p4)
    return(p)
  })    
  
  # -- This is used to print table in app
  output$tabla <- DT::renderDataTable({
    ret <- make_table(tests, hosp_mort, 
               start_date = input$range[1], 
               end_date = input$range[2], 
               type = input$testType,
               cumm = input$acumulativo)
    
    type <- case_when(input$testType == "Molecular" ~ "moleculares.", 
                      input$testType == "Serological" ~ "serológicas.",
                      input$testType == "Antigens" ~ "de antígenos.",
                      input$testType == "Molecular+Antigens" ~ "moleculares y de antígenos.")
    
  the_period <- ifelse(input$acumulativo, paste("calculadas desde ",format(input$range[1], "%B %d"), "hasta la fecha de la primera columna. "),
                                                "calculadas para la semana acabando en la fecha de la primera columna. ")
   the_caption <- paste0(
     "<p>Tasa de positividad y casos basados en pruebas ", type,
     " Mostramos dos versiones de la <b>tasa de positividad</b> ",
     "(explicación <a href=\"https://rafalab.github.io/pr-covid/tasa-de-positividad-faq.html\">aquí</a>) ",
     the_period,
     "En paréntesis mostramos intervalos de confianza del ", (1-alpha)*100,"%. ",
     "Los <b>casos únicos</b> son el número de personas con su primera prueba positiva el día ",
     "en la primera columna. Mostramos datos <b>diarios</b> y ",
     "el <b>promedio de 7 días</b>. ",
     "Las <b>muertes</b> y datos de <b>hospitalizaciones</b> vienen del informe oficial del Departamento de Salud. ",
      "La columna de <b>pruebas</b> es el número de personas que se hicieron una prueba ese día. ",
     "Las columnas bajo <b>tasas diarias</b> muestran las tasas de positividad calculadas usando datos de un día. ",
     "Tengan en cuenta que los fines de semana se hacen menos pruebas y por lo tanto se reportan menos casos y ",
     "que los datos de las pruebas toman ", lag_to_complete, " días en estar aproximadamente completos ",
     "(los casos están incompletos para días después de ", format(last_day, "%B %d). "),
     "Los datos de <b>vacunas</b> incluyen cuatro medidas: ",
     "el total de <b>vacunados</b> con por lo menos una dosis, ",
     "el total de personas que han recibido la <b>dosis completa</b>, ",
     "el total de <b>vacunas</b> admin<istradas y ",
     "el total de vacunas <b>distribuidas</b>.",
     "<p> Para <b>descargar los datos</b> mostrados en esta tabla, haga clic en el botón de <em>Download</em> al final de la página.")
  
   make_pretty_table(ret, the_caption)
  },
  server = FALSE
  )
  
  
  # -- This creates the positivity rate figure
  output$tasa_positividad <- renderPlot(
    plot_positivity(tests, start_date = input$range[1], 
                    end_date = input$range[2], 
                    type = input$testType, 
                    yscale = input$yscale,
                    version = input$pos_version)
  )
  
  output$tabla_positividad <- DT::renderDataTable({
    ret <- make_positivity_table(tests, hosp_mort, 
                                 start_date = input$range[1], 
                                 end_date = input$range[2], 
                                 type = input$testType)
    DT::datatable(ret, #class = 'white-space: nowrap',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: Left;',
                    htmltools::withTags(
                      div(HTML(paste0(
                        "<p> Las primeras columnas 2-5 en la tabla abajo representan los totales de la semana que acaba en el día bajo la primera columna (<b>Fecha</b>):</b>",
                        "<UL>",
                        "<LI><b>Pruebas</b> = personas que se hicieron pruebas.</LI>", 
                        "<LI><b>Positivos</b> personas que salieron positivo.</LI>",
                        "<LI><b>Negativos</b> =  <b>Pruebas</b> - <b>Positivos</b>.</LI>",
                        "<LI><b>Casos</b> = casos nuevos únicos, personas que salieron positivo por primera vez.</LI>",
                        "</UL><p>Las dos definiciones son las siguientes:</p>",
                        "<UL>",
                        "<LI><b>Prueba sobre prueba</b> = <b>Positivos</b> / <b>Pruebas</b></LI>",
                        "<LI><b>Casos sobre personas</b> = <b>Casos</b> / (<b>Casos</b> + <b>Negativos</b>)</LI>",
                        "</UL>",
                        "<p> Estas son basadas en <em>Approach 3</em> y <em>Approch 1</em>, respectivamente, que <a href=\"https://coronavirus.jhu.edu/testing/differences-in-positivity-rates\">esta explicación</a> recomienda monitorear, de ser posible. ",
                        "Una pequeña diferencia es que <em>Approch 3</em> usa todas las pruebas y aqui removemos duplicados dentro de las semanas para evitar el posible efecto de duplicados causados por errores de entrada de datos. ",
                        "El <a href = \"https://covid.cdc.gov/covid-data-tracker/#testing_positivity7day\">CDC</a> usa <em>Approch 3</em>.</p>",
                        "<p>Noten que hay más <b>Pruebas</b> que <b>Personas</b> porque algunas personas se hacen más de una prueba a la semana y hay errores de duplicación. ",
                        "Noten también que hay más <b>Positivos</b> que <b>Casos</b> únicos nuevos porque algunas personas salen positivo en múltiples semanas.</p>",
                        "<p>La tasa de positividad <b>no es un estimado del por ciento de la población que está infectada</b> ",
                        "ya que las personas que se hacen pruebas no son para nada representativas de la población. ",
                        "Son útiles y se monitorean porque suben cuando suben los casos o cuando no se hacen suficientes pruebas. ",
                        "Pueden leer más detalles <a href=\"https://rafalab.github.io/pr-covid/tasa-de-positividad-faq.html\">aquí</a>."
                      ))))),
                  rownames = FALSE,
                  options = list(dom = 't', pageLength = -1,
                                 columnDefs = list(
                                   list(targets = 0, orderData = ncol(ret)-1),
                                   list(targets = ncol(ret)-1, visible = FALSE),
                                   list(className = 'dt-right', targets = 1:(ncol(ret)-1))))) %>%
      DT::formatStyle(1, "white-space"="nowrap")
    }, 
    server = FALSE
  )
  
  # -- This creates the hospitalization figure
  output$hospitalizaciones <- renderPlot(
    plot_hosp(hosp_mort, 
              start_date = input$range[1],
              end_date = input$range[2],
              yscale = input$yscale)
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
                cumm = input$acumulativo,
                yscale = input$yscale)
  )
  
  # -- This creates the daily number of tests figure
  output$numero_pruebas <- renderPlot(
    plot_test(tests, 
              start_date =input$range[1], 
              end_date =input$range[2], 
              type = input$testType, 
              cumm = input$acumulativo)
  )
  
  # -- This creates positivity plot by lab
  output$positividad_por_lab <- renderPlot(
    plot_positivity_by_lab(labs, 
              start_date =input$range[1], 
              end_date =input$range[2], 
              type = input$testType, 
              yscale = input$yscale)
  )
  
  # -- This creates proportion of tests per labs
  output$numero_pruebas_por_lab <- renderPlot(
    plot_tests_by_lab(labs, 
                      start_date =input$range[1], 
                      end_date =input$range[2], 
                      type = input$testType)
  )
  
  # -- This creates the daily number of tests figure
  output$casos <- renderPlot(
    plot_cases(cases, 
               start_date = input$range[1], 
               end_date = input$range[2], 
               type =  input$testType,
               cumm = input$acumulativo, 
               yscale = input$yscale)
  )
  

  # by region stats ---------------------------------------------------------

  by_region <- reactive({load(file.path(rda_path,"regions.rda"));
                              summary_by_region(tests_by_region, 
                                                pop_by_region,
                                                start_date = input$range[1], 
                                                end_date = input$range[2], 
                                                type =  input$testType,
                                                cumm = input$acumulativo, 
                                                yscale = input$yscale,
                                                version = input$by_region_version)})
  
  output$plot_by_region <- renderPlot(by_region()$p)
  output$table_by_region <- DT::renderDataTable(by_region()$pretty_tab, server = FALSE)
  
  by_age<- reactive({load(file.path(rda_path,"by-age.rda"));
    summary_by_age(tests_by_age, 
                      pop_by_age,
                      start_date = input$range[1], 
                      end_date = input$range[2], 
                      type =  input$testType,
                      cumm = input$acumulativo, 
                      yscale = input$yscale,
                      version = input$by_age_version)})
  
  output$plot_by_age <- renderPlot(by_age()$p)
  output$table_by_age <- DT::renderDataTable(by_age()$pretty_tab, server = FALSE)
  
  # -- This creates a geographical table of positivity rate
  output$municipios <- DT::renderDataTable({
    ret <- make_municipio_table(tests_by_strata,  
                                start_date =input$range[1], 
                                end_date =input$range[2], 
                                type = input$testType)
    
    DT::datatable(ret, #class = 'white-space: nowrap',
                  caption = paste0("Por razones de privacidad, no tenemos accesoso a identificadors por lo cual estos estimados de positividad están basado en el porciento de pruebas positivas, sin remover duplicados.",
                                   " El porciento de pruebas positivas se calcula en periodo ",
                                   format(input$range[1], "%B %d"),
                                   " a ",
                                   format(input$range[2], "%B %d"),
                                   ". IC = Intervalo de confianza del ", (1-alpha)*100,"%."),
                  rownames = FALSE,
                  options = list(dom = 't', pageLength = -1,
                                 columnDefs = list(
                                   list(targets = 5, orderData = 8),
                                   list(targets = 8, visible = FALSE),
                                   list(className = 'dt-right', targets = 2:7)))) %>%
      DT::formatStyle(1:2,"white-space"="nowrap")
  }, 
  server = FALSE
  )
  
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
                 yscale = input$yscale,
                 version = input$age_plot_version)
  )
  
  output$rezago <- renderPlot({
    load(file.path(rda_path,"rezago.rda"))
    load(file.path(rda_path,"rezago_mort.rda"))
    plot_rezago(rezago, rezago_mort, 
                start_date = input$range[1], 
                 end_date = input$range[2], 
                 type = input$testType)
  })
  
  output$labs <- DT::renderDataTable({
   
    load(file.path(rda_path, "lab_tab.rda"))
    
    ret <- make_lab_tab(lab_tab,
                        input$range[1],
                        input$range[2],
                        input$testType)
                        
   DT::datatable(ret,   
                 caption =  paste0("Número de pruebas reportadas por los Laboratorios y Hospitales para los días: ",
                                   format(input$range[1], "%Y %B %d"),
                                   " a ",
                                   format(input$range[2],  "%Y %B %d.")),
                 rownames = FALSE,
                 options = list(dom = 't', pageLength = -1,
                                columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(ret)-1))))) %>%
     DT::formatStyle(1,"white-space"="nowrap")
   
  }, server = FALSE)
  
  output$vaccines <- renderPlot({
    plot_vaccines(hosp_mort, 
                start_date = input$range[1],
                end_date =input$range[2])
  })
  
  output$vaccines_table <- DT::renderDataTable({
    table_vaccines(hosp_mort, 
                  start_date = input$range[1],
                  end_date =input$range[2])
  }, server = FALSE)
  
  # -- This allows users to download data
  datasetInput <- reactive({
    switch(input$dataset,
           "pruebas" = readRDS(file.path(rda_path, "all_tests.rds")),
           "casos" = cases,
           "hosp-mort" = {
             hosp_mort %>%
               select(date, HospitCOV19, CamasICU, CamasICU_disp, IncMueSalud, 
                      hosp_week_avg, icu_week_avg, mort_week_avg) %>%
               setNames(c("dates", "hospitalizaciones", "camas_icu", "camas_icu_disp", "muertes",
                        "hospitalizaciones_week_avg", "camas_icu_week_avg", "muertes_week_avg"))
           },
           "positivos" = select(tests, -old_rate),
           "municipios" = {
             tests_by_strata %>%
               filter(testType %in% c("Antigens", "Molecular", "Serological")) %>%
               mutate(patientCity = as.character(patientCity)) %>%
               filter(patientCity %in% c("No reportado", poblacion_municipios$patientCity)) %>%
               group_by(testType, patientCity, date) %>%
               summarize(positives = sum(positives), tests = sum(tests),
                         rate =  positives/tests, .groups = "drop") %>%
               ungroup() %>%
               left_join(poblacion_municipios, by = "patientCity") 
           },
           "edad" = {
             tests_by_strata %>%
               filter(ageRange != "No reportado") %>%
               filter(testType %in% c("Antigens", "Molecular", "Serological")) %>%
               group_by(testType, ageRange, date) %>%
               summarize(positives = sum(positives), .groups = "drop") %>%
               ungroup() %>%
               mutate(percent = positives/sum(positives))
           },
           "municipios-edad" = {
             tests_by_strata %>%
               filter(ageRange != "No reportado") %>%
               filter(testType %in% c("Antigens", "Molecular", "Serological")) %>%
               mutate(patientCity = as.character(patientCity)) %>%
               filter(patientCity %in% c("No reportado", poblacion_municipios$patientCity)) 
           },
           "rezago" = {
             load(file.path(rda_path, "rezago.rda"))
             rezago
           }
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$dataset, "-", format(the_stamp, "%Y-%m-%d_%H:%M:%S"),".csv")
    },
    content = function(file) {
      write.csv(datasetInput(), file = file, row.names = FALSE)  
    },
    contentType = "txt/csv"
  )
  
  output$downloadLabData <- downloadHandler(
    filename = function() {
      paste0("lab_dat", "-", format(the_stamp, "%Y-%m-%d_%H:%M:%S"),".csv")
    },
    content = function(file) {
      load(file.path(rda_path, "lab_tab.rda"))
      
      ret <- make_lab_tab(lab_tab,
                          input$range[1],
                          input$range[2],
                          input$testType)
      write.csv(ret, file = file, row.names = FALSE)  
    },
    contentType = "txt/csv"
  )
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0("datos_diarios", "-", format(the_stamp, "%Y-%m-%d_%H:%M:%S"),".csv")
    },
    content = function(file) {
      
      ret <- make_table(tests, hosp_mort, 
                        start_date = input$range[1], 
                        end_date = input$range[2], 
                        type = input$testType,
                        cumm = input$acumulativo)

      write.csv(ret, file = file, row.names = FALSE)  
    },
    contentType = "txt/csv"
  )
  
}

shinyApp(ui = ui, server = server)




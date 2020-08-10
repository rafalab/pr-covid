# -- Set up
library(shiny)
library(shinythemes)
library(lubridate)

# -- First day
first_day <- make_date(2020, 3, 12)

# -- Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),
                  
    # -- Google analytics add on
    tags$head(includeHTML(("google-analytics.html"))),
    
    # Application title
    titlePanel("Monitoreo de COVID-19 en Puerto Rico"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("range", "Periodo", 
                           start = make_date(2020, 3, 21), end= today() - days(3),
                           min = first_day,
                           format = "M-dd",
                           max = today()),
            
            actionButton("weeks", "Últimas dos Semanas", 
                         style = "color: black; 
                     background-color: rgb(230, 220, 205); 
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px"),
            br(),
            br(),
            actionButton("reset", "Periodo original", 
            style = "color: black; 
                     background-color: rgb(230, 220, 205); 
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px"),
            br(),
            br(),
            actionButton("alldates", "Todas las fechas", 
                         style = "color: black; 
                     background-color: rgb(230, 220, 205); 
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px"),
            br(),
            br(),
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
              br(),
            div("Datos depurados:"),            
            downloadButton("downloadData", "Download", 
                           style = "color: black; 
                     background-color: rgb(230, 220, 205); 
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px"), 
           br(),
            br(),
           uiOutput("stamp"),
           #div("Actualización:", br(), the_stamp),
           width = 3),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                tabPanel("Totales diarios",
                         DT::dataTableOutput("tabla")),
                
                tabPanel("Resumen",
                         h3("Resumen de datos COVID-19 en Puerto Rico"),
                         plotOutput("resumen")),
                
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
                
                tabPanel("Tasas por región",
                         DT::dataTableOutput("regiones")),
                
                # tabPanel("Mapa",
                #          plotOutput("mapa_positividad")),
                # 
                tabPanel("Positivos por Edad",
                         plotOutput("age"))
                
            )
    )),
    hr(),
    div(class = "footer", includeHTML("footer.html"))))

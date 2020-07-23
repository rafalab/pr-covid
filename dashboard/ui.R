# -- Set up
source("init.R")

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
                           start = make_date(2020, 3, 21), end=today() -days(1),
                           min = make_date(2020,3,15),
                           format = "M-dd",
                           max = today()),
            actionButton("reset", "Todas las fechas", 
            style = "color: black; 
                     background-color: rgb(230, 220, 205); 
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px"),
            br(),
            br(),
            actionButton("weeks", "Últimas dos Semanas", 
                         style = "color: black; 
                     background-color: rgb(230, 220, 205); 
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px"),
            br(),
            br(),
            radioButtons("yscale", 
                         label = "Rango del gráfico:",
                         choices = list("Preescogido" = TRUE,
                                        "Determinado por datos" = FALSE),
                         selected = TRUE),
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
            div("Actualización:", br(), attr(tests, "date")),
            width = 3),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                tabPanel("Totales diarios",
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
                
                tabPanel("Positivos",
                         plotOutput("positivos_acumulados")),
                
                tabPanel("Tasas por municipio",
                         DT::dataTableOutput("municipios")),
                
                tabPanel("Mapa",
                         plotOutput("mapa_positividad"))
                
            )
    )),
    hr(),
    div(class = "footer", includeHTML("footer.html"))))

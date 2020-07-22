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
            br(),
            radioButtons("yscale", 
                         label = "Escala de valores",
                         choices = list("Predeterminada" = TRUE,
                                        "Libre" = FALSE),
                         selected = TRUE),
            br(),
            div("Datos depurados:"),
            downloadButton("downloadData", "Download"), 
            br(),
            br(),
            div("Aquí puede reiniciar el período:"),
            actionButton("reset", "Reiniciar período"),
            br(),
            br(),
            div("Aquí puede ver las últimas dos semanas:"),
            actionButton("weeks", "Últimas dos semanas"),
            br(),
            br(),
            div("Actualización:", attr(tests, "date")),
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
                
                tabPanel("Tasas por municipio",
                         DT::dataTableOutput("municipios")),
                
                tabPanel("Mapa",
                         plotOutput("mapa_positividad"))
                
            )
    )),
    hr(),
    div(class = "footer", includeHTML("footer.html"))))

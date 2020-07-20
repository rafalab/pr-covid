# -- Set up
source("init.R")

# -- Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Monitoreo de COVID-19 en Puerto Rico"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("range", "Periodo", 
                           start = make_date(2020, 3, 21), end=today(),
                           min = make_date(2020,3,15),
                           format = "M-dd",
                           max = today()),
            br(),
            div("Datos depurados:"),
            downloadButton("downloadData", "Download"), 
            width = 3),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                # -- Panel 1
                tabPanel("Positividad",
                         plotOutput("tasa_positividad")),

                
                # -- Panel 2
                tabPanel("Hospitalizaciones",
                         plotOutput("hospitalizaciones")),
                
                # -- Panel 3
                tabPanel("Muertes",
                         plotOutput("muertes")),
                
                # -- Panel 4
                tabPanel("Pruebas",
                         plotOutput("numero_pruebas")),
                
                # -- Panel 5
              ##  tabPanel("Mapa",
              ##          plotOutput("mapa_positividad")),

                # -- Panel 4
                tabPanel("Totales diarios",
                         DT::dataTableOutput("tabla")),
                
                # -- Panel 5
                tabPanel("Tasas por municipio",
                         DT::dataTableOutput("municipios"))
            )
    )),
    hr(),
    print("Datos actualizados de mortalidad y hospitalizaciones son provistos por María-Eglée Pérez  (Departamento de Matemáticas, UPR-RP)\n"),
    hr(),
    print("API: https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests\n"),
    hr(),
    print("Code used to create this site https://github.com/rafalab/pr-covid/tree/master/dashboard")
))

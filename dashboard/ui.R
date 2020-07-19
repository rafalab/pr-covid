# -- Set up
source("init.R")

# -- Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Monitoreo de COVID-19 en Puerto Rico"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            div("Datos depurados:"),
            downloadButton("downloadData", "Download"), 
            br(),
            br(),
            dateRangeInput("range", "Periodo", 
                           start = "2020-03-21", end=today(),
                           min = "2020-03-15", 
                           max = today(), 
                           separator = " - "),
            width = 3),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                # -- Panel 1
                tabPanel("Tasa de Positividad",
                         plotOutput("tasa_positividad")),
                         # plotOutput("tasa_positividad_edad")),
                
                # -- Panel 2
                tabPanel("Número de Pruebas",
                         plotOutput("numero_pruebas")),
                         # plotOutput("numero_pruebas_edad")),
                
                # -- Panel 3
                tabPanel("Tabla con totales diarios",
                         DT::dataTableOutput("tabla"))
            )
    )),
    hr(),
    print("API: https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests\n"),
    hr(),
    print("Code used to create this site https://github.com/rafalab/pr-covid/tree/master/covid19-pr-shiny")
))

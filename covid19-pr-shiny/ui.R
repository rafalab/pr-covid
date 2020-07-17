# -- Set up
source("init.R")

# -- Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Monitoreo de COVID-19 en Puerto Rico"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            div("Este botón actualiza automaticamente los datos usando el", a("API", href="https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"), "del Departamento de Salud. Esto tarda unos minutos."),
            actionButton("do", "Presiona para actualizar datos"),
            br(),
            br(),
            div("Este botón descarga los datos del", a("API", href="https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"), "ya limpios."),
            downloadButton("downloadData", "Download")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                
                # -- Panel 1
                # tabPanel("Datos",
                #          DT::dataTableOutput("table")),
                
                # -- Panel 2
                tabPanel("Tasa de Positividad",
                         plotlyOutput("tasa_positividad")), 
                
                # -- Panel 3
                tabPanel("Número de Pruebas",
                         plotlyOutput("numero_pruebas"),
                         br(),
                         br(),
                         plotlyOutput("numero_pruebas_resultados"))
                
                
            )
        )
    )
))

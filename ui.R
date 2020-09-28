library(shiny)
library(shinyFiles)
library(leaflet)

shinyUI(pageWithSidebar(
  headerPanel("Herramienta para verificar calidad de datos"),
  sidebarPanel(tags$strong("Panel de opciones"),
               br(),
               br(),
               conditionalPanel(condition="input.tabselected==1",
                                tags$p(" Seleccione la GDB a estructurar"),
                                shinyDirButton("directory", label = "Seleccione su GDB",
                                               "por favor selecione la GDB"),
               tags$p(),
               helpText("Descargue los datos unificados"),
               radioButtons("type", "",
                            choices = c("Excel (CSV)", "Texto (TSV)", "Texto (Separado por espacios)")),
               downloadButton("downloadData", "Guardar como")
               ),
               
               conditionalPanel(condition="input.tabselected==2",
                                #definicion columnas coordenadas
                                
                                #Datos divipola
                                tags$hr(),
                                tags$strong("trasformacion datos Divipola"),
                                radioButtons(inputId = "dividata",
                                             label = "",
                                             choices = c("Si" = 1, "No" = 2),
                                             selected = NULL),
                                
                                #Datos faltantes
                                tags$hr(),
                                tags$strong("Adicion de datos"),
                                tags$p("Las columnas 'ocurrenceID', 'type' y 'basisOfRecord' \n 
                             no vienen en la informacion suministrada en una  'GDB'.\n 
                             Desea que se incluyan con datos por defecto"),
                                radioButtons(inputId = "dataajust",
                                             label = "",
                                             choices = c("agregar" = 1, "omitir" = 2),
                                             selected = NULL),
                                actionButton("tranformBtn", "Agregar"),
                                
                                tags$hr(),
                                tags$strong("Definicion de la coordenadas X y Y"),
                                tags$p("Seleccione las columnas que tienen la longitud (x) y la latitud (y)"),
                                uiOutput("varx"),uiOutput("vary"),
                                
                                #origen datos
                                tags$strong("Origen de los datos"),
                                tags$p(" Seleccione un sistema de referencia"),
                                selectInput(inputId = "coordSrc",label = "",
                                            choices = c("Oeste - Oeste"= 1, "Oeste"= 2, "Bogota"= 3, "Este"= 4, "Este - Este" =5),
                                            selected = NULL),
                                actionButton("coordBtn", "Transformar"),
                                
                                tags$hr(),
                                helpText("Descargue los datos transformados"),
                                radioButtons("type2", "",
                                             choices = c("Excel (CSV)", "Texto (TSV)", "Texto (Separado por espacios)")),
                                downloadButton("downloadData2", "Guardar como")
                                 ),

               
               conditionalPanel(condition="input.tabselected==4",
                                fileInput("gvInput", "Seleccione el archivo CSV",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                          ),
                                actionButton("runGV", "Validar datos"),
                                p(),
                                downloadButton("downloadGVOutput", "Descargar datos validados")
                                ),

               conditionalPanel(condition="input.tabselected==5",
                                h1("Ingrese puntos:"),
                                numericInput("long", label = h3("Longitud:"), value = -73.839222),
                                numericInput("lat", label = h3("Latitud:"), value = 4.721806),
                                actionButton("locality", "Mapear"),
                                p(),
                                downloadButton("downloadDataBioMAD", "Descargar"),
                                p(),
                                p("Este modulo es una herramienta desarrollada 
                                  para generar listados preliminares de especies
                                  a partir de la consulta de los mapas validados 
                                  disponibles en BioModelos."),
                                p("Se sugiere usar con prudencia los listados generados.
                                  Si desea conocer las distribuciones de las especies listadas,
                                  por favor consulte",
                                  a("BioModelos.", href = "http://biomodelos.humboldt.org.co/")),
                                "Modulo creado por Elkin A. Noguera-Urbano usando",
                                a("Shiny.", href = "http://shiny.rstudio.com")
               )
               
  ),# sidebarPanel bracket
  
  mainPanel(
    tabsetPanel(
      tabPanel("Inicio", value=1,
               tags$h4("Pasar de informacion en GDB a plantilla DwC"),
               tags$p(),
               tags$p(),
               tags$p("En este apartado podrá seleccionar una GeoDataBase para que los archivos de las \n 
           colectas y los registros obtenidos, tanto de fauna, como de flora, puedan ser \n
           unificados en una plantilla que cumpla con el formato Darwin Core. Tenga en cuenta\n
           que para su correcta ejecucción, tanto los archivos, como las columnas en las tablas\n
           y los archivos geográficos deben mantener los nombres definidos por la Agencia \n
           Nacional de Licencias Ambientales (ANLA)"),
               tags$p(HTML("Datos identificados y migrados a la plantilla DwC, recuerde que dependiendo el volumen de los datos, este proceso puede tardar varios minutos\n")),
               div(dataTableOutput("initTable"), style = "font-size:80%")),
      
      tabPanel("Estructuracion de los datos", value=2,
               tags$p(HTML("Datos\n")),
               div(dataTableOutput("tbTranform"), style = "font-size:80%")
      ),
      
      tabPanel("Verificacion Taxonomica", value = 3,
               div(dataTableOutput("estrTable"), style = "font-size:80%")
      ),
      
      tabPanel('Verificacion Geografica', value = 4,
               div(dataTableOutput("gvOutput"), style = "font-size:80%")
      ),

      tabPanel("BioModelos", value = 5,
               leafletOutput("mymap"),
               p(),
               fluidRow(
                 column(4, DT::dataTableOutput("table"))
               )
      ),
      id = "tabselected"

)  # Tabset Panel bracket
) # MainPanel bracket
)) #pageWithSidebar braket  

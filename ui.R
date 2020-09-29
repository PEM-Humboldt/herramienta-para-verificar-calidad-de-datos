library(shiny)
library(shinyFiles)
library(leaflet)

shinyUI(pageWithSidebar(
  headerPanel("Herramienta para verificar la calidad de datos"),
  sidebarPanel(img(src = "logo.png",  style = "width:25%;"),
               p(),
               hr(),
               tags$strong("Panel de opciones"),
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
               downloadButton("downloadData", "Descargar")
               ),

               conditionalPanel(condition="input.tabselected==2",
                                #definicion columnas coordenadas

                                #Datos divipola
                                tags$strong("Trasformacion datos Divipola"),
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
                                downloadButton("downloadData2", "Descargar")
                                 ),

                conditionalPanel(condition="input.tabselected==3",
                                 helpText("Seleccione el archivo obtenido en el paso anterior, o uno nuevo que cuente con la columna 'scientificName'"),
                                 fileInput('verCsv', "Seleccione el archivo 'csv'",
                                  accept=c('text/csv',
                                  'text/comma-separated-values,text/plain',
                                  '.csv')
                                ),
                                tags$hr(),
                                checkboxInput('header', 'Encabezado', TRUE),
                                radioButtons(
                                  'sep',
                                  'Separator',
                                  c(Comma=',', Semicolon=';', Tab='\t'),
                                  ','
                                ),
                                actionButton("taxValBtn", "Validar Nombres"),
                                tags$hr(),
                                helpText("Mezcle los datos originales con la validación"),
                                actionButton("mergeBtn", "Mezclar resultados"),
                                tags$hr(),
                                helpText("Descargue los datos validados"),
                                downloadButton("downloadData3", "Descargar")
                ),

                conditionalPanel(condition="input.tabselected==4",
                                 helpText("Seleccione el archivo obtenido en cualquier de los pasos anteriores, o
                                 uno que cuente con los campos geograficos DwC ('country', 'stateProvince,'
                                          'country', 'decimalLongitude', 'decimalLatitude')."),
                                 fileInput("gvInput", "Seleccione el archivo CSV",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                          ),
                                actionButton("runGV", "Verificar datos"),
                                p(),
                                tags$hr(),
                                helpText("Descargue los datos verificados"),
                                downloadButton("downloadGVOutput", "Descargar"),
                                ),

                conditionalPanel(condition="input.tabselected==5",
                                 helpText("Este modulo es una herramienta desarrollada
                                  para generar listados preliminares de especies
                                  a partir de la consulta de los mapas validados
                                  disponibles en BioModelos."),
                                 hr(),
                                 p("Seleccione las coordenadas del punto del cual desea conocer las especies potenciales"),
                                numericInput("long", label = ("Longitud:"), value = -73.839222),
                                numericInput("lat", label = ("Latitud:"), value = 4.721806),
                                actionButton("locality", "Mapear"),
                                p(),
                                downloadButton("downloadDataBioMAD", "Descargar"),
                                p(),
                                p(),
                                p("Se sugiere usar con prudencia los listados generados.
                                  Si desea conocer las distribuciones de las especies listadas,
                                  por favor consulte",
                                  a("BioModelos.", href = "http://biomodelos.humboldt.org.co/")),
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
               tags$br(),
               tags$br(),
               tags$p(HTML("Cite este recurso de la siguiente forma:")),
               tags$blockquote("Bello C., Castro C., Cruz-Rodriguez C., Gonzalez I, Londono M. C., Lopez D., Noguera-Urbano E. A., Olaya-Rodriguez M. H., Rey J. C., Suarez E., Velasquez-Tibata J., Villa M. 2020. Herramienta para verificar la calidad de datos sobre biodiversidad. Instituto de Investigacion de Recussos Biologicos, Alexander von Humboldt. Esta aplicacion fue creada en el marco de la suborden de tarea 4 en el marco del Proyecto Riqueza Natural de USAID.", style = "font-size:90%"),
               
               div(dataTableOutput("initTable"), style = "font-size:80%")),

      tabPanel("Estructuracion de los datos", value=2,
               tags$br(),
               tags$em(HTML("Para continuar con el proceso de verificacion usando la informacion extraida de las GDB, seleccione 'si' y 'agregar' en el panel de opciones")),
               div(dataTableOutput("tbTranform"), style = "font-size:80%")
      ),

      tabPanel("Verificacion Taxonomica", value = 3,
               tags$br(),
                tags$em(HTML("En esta seccion podra verificar la coincidencia entre los nombres cientificos que provienen de la gdb u otra fuente, con validadores taxonomicos disponibles en la web, y los mezcla con la tabla de entrada")),
               div(tableOutput("estrTable"), style = "font-size:80%")
      ),

      tabPanel('Verificacion Geografica', value = 4,
               tags$br(),
               tags$em(HTML("Este modulo permite verificar que el municipio
                            y departamento asociado a cada registro corresponda
                            con sus coordenadas. El proceso agrega unas columnas
                            con municipios y departamentos recomendados para
                            aquellos registros donde no hay una congruencia. Para
                            ejecutar la verificacion asegurese de subir una tabla
                            con los datos estructurados en formato csv y haga click
                            en el boton de Verificar datos.")),
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

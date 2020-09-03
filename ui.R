library(shiny)
library(shinyFiles)

shinyUI(pageWithSidebar(
  headerPanel("Titulo por definir"),
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
      tabPanel('Verificacion Geografica', value = 4),
      id = "tabselected"

)  # Tabset Panel bracket
) # MainPanel bracket
)) #pageWithSidebar braket  

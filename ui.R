library(shiny)
library(shinyFiles)

shinyUI(pageWithSidebar(
  headerPanel(
    "Datos Biológicos"),
  sidebarPanel(

    tags$p(),
    tags$p("En este apartado podrá seleccionar una GeoDataBase para que los archivos de las \n 
           colectas y los registros obtenidos, tanto de fauna, como de flora, puedan ser \n
           unificados en una plantilla que cumpla con el formato Darwin Core. Tenga en cuenta\n
           que para su correcta ejecucción, tanto los archivos, como las columnas en las tablas\n
           y los archivos geográficos deben mantener los nombres definidos por la Agencia \n
           Nacional de Licencias Ambientales (ANLA)"),
    
    tags$hr(),
    tags$p(),
    helpText(" Seleccione la GDB a estructurar"),
    shinyDirButton("directory", "Seleccione su GDB", "por favor selecione la GDB"),
    
    tags$p(),  
    tags$p(), 
    tags$p(),
    helpText(" Seleccione el formato de descarga"),
    radioButtons("type", "Tipo de Formato:",
                choices = c("Excel (CSV)", "Texto (TSV)", "Texto (Separado por espacios)")),
    downloadButton("downloadData", "Guardar como")
),

  mainPanel(
    tags$h4("Datos en la pantilla DwC"),
    tags$p(HTML("Datos identificados y migrados a la plantilla DwC, recuerde que dependiendo el volumen de los datos, este proceso puede tardar varios minutos\n")),
    tableOutput("outTable"),
  )
))  



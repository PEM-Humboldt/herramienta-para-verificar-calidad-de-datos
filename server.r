library(shiny)
library(shinyFiles)
library(fs)
library(data.table)
library(rgdal)
library(sf)
library(dplyr)
library(stringi)

source("db_to_dwc_simp.r")
shinyServer(function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), filetypes=c('', 'gdb'))

  dsimput<- reactive({
    req(input$directory)
    if (is.integer(input$directory)) {
      cat("No se ha seleccionado ninguna GDB")
    } else {
      df<-parseDirPath(volumes, input$directory)
      suppressWarnings(db_to_dwc(df))
    }
 })
  fileext<- reactive({
    switch (input$type,
            "Excel (CSV)" = "csv", "Texto (TSV)" = "txt", "Text (Separado por espacios)" = "txt")

  })
  ## Tabla en pantalla
  output$outTable <- renderTable({
    dsimput()
  })

  # descargar los datos en el formato seleccionado ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("GDB_in_DwC", fileext(), sep = ".")
    },
    content = function(file) {
      sep<- switch (input$type, "Excel (CSV)" = ",", "Texto (TSV)" = "\t", "Text (Separado por espacios)" = " ")

      write.csv(dsimput(), file, row.names = FALSE)
    }
  )

})

library(shiny)
library(shinyFiles)
library(fs)
library(data.table)
library(rgdal)
library(sf)
library(dplyr)
library(stringi)
library(DT)

source("db_to_dwc_simp.r") #Pass from GDB to DWC format the data
source("herram_estruct_datos.r") # Add missing columns and trasform states, counties and coordinates to DwC Standard
load('divipola_codes.rds') # Divipola Conventions from States and counties 
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
  #--- First tab ---#
  ## Table 
  output$initTable <- renderDataTable({
    a<-dsimput()
    datatable(a,
              options = list(
                pageLength = 3,
                lengthMenu = c(2, 12, 18),
                searching= FALSE))
    
    
  })
  
  #--- Second tab ---#
  # Transform political administrative names and add missing data
  tbTranform <- eventReactive(input$tranformBtn, {
    estr_dt<-dsimput()

    if (input$dividata == 1) {
      datapol<- corr.geonames(data =estr_dt, depto = estr_dt$stateProvince,  mpio =  estr_dt$county,
                              routineType = 'G:/Cristian_data/Humboldt/Proyectos/2020/ANLA/Sninny/bioregistros-app/' )
      if (input$dataajust == 1) {
        validate(need(datapol, ""))
        adddata<- add.corr.dt(data = datapol)}
    } else if (input$dividata == 2) {
      if (input$dataajust == 1) {
        adddata<- add.corr.dt(data = estr_dt)
      }
    }
 })
  
  # Pulling the list of variable for choice of variable x
  output$vary <- renderUI({
    selectInput("variabley", "seleccione la latitud", choices=names(tbTranform()))
  })
  # Pulling the list of variable for choice of variable y
  output$varx <- renderUI({
    selectInput("variablex", "Seleccione la longitud", choices=names(tbTranform()))
  })
  

  # convert coords from planar to geographical standard
  tbCoords <- eventReactive(input$coordBtn, {
    coordsajust<-tbTranform()
    selectcoords<- input$coordSrc

    if (!is.numeric(input$variablex)) {
      print("debe seleccionar una columna correcta")
    }
    
        if (selectcoords == 1){
      datacoords<- trans.coord(data= coordsajust, 
                                 id = coordsajust$occurrenceID,
                                 lon =coordsajust$verbatimLongitude, 
                                 lat =coordsajust$verbatimLatitude, 
                                 coordreference = "magnafarwest")
    } else if (selectcoords == 2) {
      datacoords<- trans.coord(data= coordsajust, 
                                 id = coordsajust$occurrenceID,
                                lon =coordsajust$verbatimLongitude, 
                                lat =coordsajust$verbatimLatitude,  
                                 coordreference = "magnawest")
    } else if (selectcoords == 3) {
      datacoords<- trans.coord(data= coordsajust, 
                                 id = coordsajust$occurrenceID,
                               lon =coordsajust$verbatimLongitude, 
                               lat =coordsajust$verbatimLatitude, 
                                 coordreference = "magnabta")
    } else if (selectcoords == 4) {
      datacoords<- trans.coord(data= coordsajust, 
                                 id = coordsajust$occurrenceID,
                                lon =coordsajust$verbatimLongitude, 
                                lat =coordsajust$verbatimLatitude,   
                                 coordreference = "magnaeast")
    } else if (selectcoords == 5) {
      datacoords<- trans.coord(data= coordsajust, 
                                 id = coordsajust$occurrenceID,
                                 lon =coordsajust$verbatimLongitude, 
                                 lat =coordsajust$verbatimLatitude, 
                                 coordreference = "magnafareast")
    }
 })

  #visualizate  ajusted data
  output$tbTranform <- renderDataTable({
    if (!is.null(tbTranform()) && is.null(tbCoords()) ){
    datatable(tbTranform(),
              options = list(
                pageLength = 3,
                lengthMenu = c(2, 12, 18),
                searching= FALSE))
    }else if ( !is.null(tbTranform()) && !is.null(tbCoords()) ){
      datatable(tbCoords(),
                options = list(
                  pageLength = 3,
                  lengthMenu = c(2, 12, 18),
                  searching= FALSE))}
  })
  fileext<- reactive({
    switch (input$type,
            "Excel (CSV)" = "csv", "Texto (TSV)" = "txt", "Text (Separado por espacios)" = "txt")
  }) 
    fileext2<- reactive({
      switch (input$type2,
              "Excel (CSV)" = "csv", "Texto (TSV)" = "txt", "Text (Separado por espacios)" = "txt") 
    })
# Download data in the first tab ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("GDB_in_DwC", fileext(), sep = ".")
    },
    content = function(file) {
      sep<- switch (input$type, "Excel (CSV)" = ",", "Texto (TSV)" = "\t", "Text (Separado por espacios)" = " ")
      
      write.csv(dsimput(), file, row.names = FALSE)})
  
  
  
  # Download data in the second tab ----
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("GDB_estruct",fileext2(), sep = ".")
    },
    content = function(file2) {
      sep<- switch (input$type2, "Excel (CSV)" = ",", "Texto (TSV)" = "\t", "Text (Separado por espacios)" = " ")
      
      write.csv(tbCoords(), file2, row.names = FALSE)})
  
})
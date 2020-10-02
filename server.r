library(shiny)
library(shinyFiles)
library(leaflet)
library(mapview)
library(fs)
library(data.table)
library(rgdal)
library(sf)
library(dplyr)
library(stringi)
library(DT)
library(raster)

source("db_to_dwc_simp.r") #Pass from GDB to DWC format the data
source("herram_estruct_datos.r") # Add missing columns and trasform states, counties and coordinates to DwC Standard
source("validacionGeografica.R")
source("taxon_valid.R", local = TRUE)
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
    withProgress(message = 'Transfiendo datos de la GDB a una tabla',
                 detail = 'Espere un momento...', value = 0, {
      a<-dsimput()
      datatable(a,
                options = list(
                  pageLength = 3,
                  lengthMenu = c(2, 12, 18),
                  searching= FALSE))
    })
  })

  #--- Second tab ---#
  # Transform political administrative names and add missing data
  tbTranform <- eventReactive(input$tranformBtn, {
    withProgress(message = 'Convirtiendo informacion',
                 detail = 'Espere un momento...', value = 0.1, {
      estr_dt<-dsimput()
      if (input$dividata == 1) {
        datapol<- corr.geonames(data =estr_dt, depto = estr_dt$stateProvince,  mpio =  estr_dt$county )
        if (input$dataajust == 1) {
          validate(need(datapol, ""))
          adddata<- add.corr.dt(data = datapol)}
      } else if (input$dividata == 2) {
        if (input$dataajust == 1) {
          adddata<- add.corr.dt(data = estr_dt)
        }
      }
    })
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
    withProgress(message = 'Transformando coordenadas',
                 detail = 'Espere un momento...', value = 0.2, {
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
  })

  #visualizate  ajusted data
  output$tbTranform <- renderDataTable({
    if (!is.null(tbTranform()) & is.null(tbCoords()) ){
    datatable(tbTranform(),
              options = list(
                pageLength = 3,
                lengthMenu = c(2, 12, 18),
                searching= FALSE))
    }else if ( !is.null(tbTranform()) & !is.null(tbCoords()) ){
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

  #--- Third tab (Validacion taxonómica) ---#
  showOuput <- reactiveValues(table = NULL)

  observeEvent(input$verCsv, {
    inFile <- input$verCsv

    if (is.null(inFile))
      return(NULL)

    dataList <<- as.data.frame(fread(inFile$datapath))
    showOuput$table <- dataList
  })

  observeEvent(input$taxValBtn, {
    withProgress(message = 'Realizando verificación taxonómica',
                detail = 'Espere un momento...', value = 0.4, {
      validList <<- tax_res(dataList$scientificName)
      showOuput$table <- validList
    })
  })

  observeEvent(input$mergeBtn, {
    showOuput$table <- merge(dataList, validList, by.x= "scientificName", by.y="submitted_name", all.x = T)
  })

  output$estrTable <- DT::renderDataTable({
    if (is.null(showOuput$table)) return()
    showOuput$table
  })

  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("tax_validated", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(showOuput$table, file, row.names = FALSE)
    }
  )

  #--- Fourth tab (Validacion geografica) ---#
  observeEvent(input$runGV, {
    output$gvOutput <- renderDataTable({
      withProgress(message = 'Realizando verificación taxonómica', 
                   min = 0, max = 1, {
        req(input$gvInput)
        incProgress(0.1, detail = "Preparando datos...")
        gvFile <- input$gvInput
        set2 <<- as.data.frame(fread(gvFile$datapath, colClasses = "character", header = TRUE, encoding = "Latin-1"))
        set2$scriptID <<- 1:nrow(set2)
        set3 <- set2[, c("scriptID", "scientificName", "country", "stateProvince", "county", "decimalLatitude", "decimalLongitude")]
        colnames(set3) <- c("scriptID", "nombre", "pais", "departamento", "municipio", "latitud", "longitud")
        incProgress(0.4, detail = "Verificando datos...")
        verif <- VERIFICACION_PAISES(set3, routineType = "Colombia")
        verifSimple <- verif[[1]]
        verifTable <<- verifSimple[, !(names(verifSimple) %in% c("suggestedMunicipality", "duplicated"))]
      })
    })
  })

  output$downloadGVOutput <- downloadHandler(filename = "resultado_validacion_geografica.csv",
                                             content = function(file){
                                               write.csv(verifTable, file, row.names = FALSE)
                                             })

  #--- Fifth tab (BioModelos) ---#

  # Crea formato para los datos de la lacalidad ingresados por el usuario.
  points <- eventReactive(input$locality, {
    loc_user<-cbind(input$long, input$lat)
    print(loc_user)
    }, ignoreNULL = FALSE)


  # Dibuja el mapa en el panel principal.
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 4.721806, lng = -73.839222, zoom = 7) %>%
      addTiles() %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 5",
        onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
        addMarkers(data = points())
  })

  # Dibuja la localidad base en el mapa.
  observeEvent(input$locality,{
    isolate({
      new_zoom <- 10
      if(!is.null(input$map_zoom)) new_zoom <- input$map_zoom
      leafletProxy('mymap') %>%
        setView(lng = input$long, lat = input$lat, zoom = new_zoom)
    })
  })

  # Carga la localidad y cruza con shapefiles BioModelos.
  # spec_cross representa la tabla producto del cruce BioModelos * localidad.
  spec_cross <-eventReactive(input$locality, {
    temp<-cbind(input$long, input$lat)
    temp<-as.data.frame(temp)
    colnames(temp)<-c("x","y")
    temp <- SpatialPointsDataFrame(temp,temp)
    crs(temp) <- "+proj=longlat +datum=WGS84 +no_defs"
    maps<-readOGR("./appData/Biom_UICN.shp") # Cargar archivo shapefile BioModelos.
    species <- as.data.frame(over(temp,maps,returnList = TRUE))
    if (!is.null(input$bmInput)) {
      bmFile <- input$bmInput
      records <<- as.data.frame(fread(bmFile$datapath, colClasses = "character", header = TRUE, encoding = "Latin-1"))
      species$Coincidencia <- species$X1.binomial %in% records$scientificName
    }
    return(species)
    })

  # Muestra la tabla en el panel principal.
  # Table of selected dataset ----
  output$table <- DT::renderDataTable({
    spec_cross()
  })

  # Permite descargar la tabla "Downloadable csv" resultante.
  output$downloadDataBioMAD <- downloadHandler(
    filename = function() {
      paste("Especies sugeridas",date(),".csv", sep = " ")
    },
    content = function(file) {
      write.table(spec_cross(), file, row.names = FALSE,sep = ",")
    }
  )
})

##Functions for trasform data of ANLA GDB to Dwc standard

#By: Cristian Cruz-Rodr[i]guez 
#Date: 13-08-2020

' Funciones para trasformar informaci[o]n del formato ANLA al formato DwC
#' 
#' @param data data.frame con informaci[o]n biol[o]gica proveniente de la GDB con form[a]to ANLA
#' @param id Identificador del registro en la tabal de datos original
#' @param lon Coordenada x o de longintud, con la informaci[o]n en planas
#' @param lat Coordenada y o de latitud, con la informaci[o]n en planas
#' @param coordreference sistema de refercii original. Debe usarse magnafarwest. para EPSG 3114; magnawest para EPGS 3115; magnabta para EPSG 3116; magnaeast para EPSG 3117 y magnafareast para EPSG 3118 
#' @param depto c[o]digo del departamento usando las convenciones definidas por el DANE
#' @param mpio c[o]digo del municipio usando las convenciones definidas por el DANE
#' @param routineType Ruta en donde se encuentra el archivo  'divipola_codes.rs', necesario para la conversi[o]n de c[o]digos DANE
#' @return Datos convertidos y adheridos a \code{data}.

#add.corr.dt(function) Corrige la informaci[o]n contenida en la GDB y las trasnforma a la codificaci[on] UTF-8
add.corr.dt<-function (data=x) {
  
  cat(" \n  correci[o]n codificaci[o]n y adici[o]n de datos faltantes  \n")
  data$vernacularName<-stri_encode(data$vernacularName, 'UTF-8')
  data$locality<-stri_encode(data$locality, 'UTF-8')
  data$parentEventID<-stri_encode(data$parentEventID, 'UTF-8')
  data$habitat<-stri_encode(data$habitat, 'UTF-8')
  data$samplingEffort<-stri_encode(data$samplingEffort, 'UTF-8')
  data$waterBody<-stri_encode(data$waterBody, 'UTF-8')
  data$locality<-stri_encode(data$locality, 'UTF-8')
  data$scientificName<-stri_encode(data$scientificName, 'UTF-8')
  
  #Informaci[o]n faltante en las GDB
  data$occurrenceID <- paste0(1:nrow(data), "_", data$eventID,"_ANLA") 
  data$continent <- 'SA'
  data$country <- 'Colombia'
  data$countryCode <- 'CO'
  data$basisOfRecord <- 'HumanObservation'
  data$type <- 'Evento'
  data$language <-'es'
  cat("\n  Proceso Terminado \n ")
  return(data)
}

#trans.coord(function) transforma las coordenadas del formato MAGNA-SIRGAS a geogr[a]ficas decimales usando el sistema WGS84
trans.coord<-function(data=x, id= x$x,  lon= x$x, lat= x$x,coordreference = NA)  {
  cat(" \n  Transformaci[o]n de coordenadas de planas a geogr[a]ficas  \n")  
  library(sp)
    options(warn=-1)
    coords<- vector()
    coords$occurrenceID <- id
    coords$decimalLatitude<- as.numeric(lon)
    coords$decimalLongitude<- as.numeric(lat)
    coords<-as.data.frame(coords)
    
    no.coords <- which(is.na(coords$decimalLatitude) | is.na(coords$decimalLongitude))
    if (length(no.coords)>0){
      coords <- coords[-no.coords,]}
    coordinates(coords) =~ decimalLongitude + decimalLatitude
    
    #geographical coordiantes
    wgs84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #ESPG 4326
    #Planar coordiantes
    magnafarwest<-"+proj=tmerc +lat_0=4.596200416666666 +lon_0=-80.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #ESPG 3114
    magnawest<-"+proj=tmerc +lat_0=4.596200416666666 +lon_0=-77.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #ESPG 3115
    magnabta<-"+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #ESPG 3116
    magnaeast<- "+proj=tmerc +lat_0=4.596200416666666 +lon_0=-71.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #ESPG 3117
    magnafareast <-"+proj=tmerc +lat_0=4.596200416666666 +lon_0=-68.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # ESPG 3118
    
    if ('magnafarwest' %in% coordreference) {
          proj4string(coords)<-magnafarwest
          coords <- spTransform(coords, wgs84)
          coords2<-as.data.frame(coords)
          }else if ('magnawest' %in% coordreference) {
            proj4string(coords)<-magnawest
            coords <- spTransform(coords, wgs84)
            coords2<-as.data.frame(coords)
          }else if ('magnabta' %in% coordreference) {
            proj4string(coords)<-magnabta
            coords <- spTransform(coords, wgs84)
            coords2<-as.data.frame(coords)
          }else if ('magnaeast' %in% coordreference) {
            proj4string(coords)<-magnaeast
            coords <- spTransform(coords, wgs84)
            coords2<-as.data.frame(coords)
          }else if ('magnafareast' %in% coordreference) {
            proj4string(coords)<-magnafareast
            coords <- spTransform(coords, wgs84)
            coords2<-as.data.frame(coords)
          }else if (is.na (coordreference)){
            return(print("Debe definir el sistema de referencia"))
          }else if (is.character(coordreference)) {  
            return(print("Debe definir el sistema de referencia de manera correcta"))
          } else NULL
     
  data<-merge(data, coords2, by = 'occurrenceID', all.x = T, all.y= F)
  data$geodeticDatum <- "EPSG:4326"
  cat("\n  Proceso Terminado \n ")
  return(data)
}

#corr.geonames(function) Convierte los los departamentos y municipios a texto, cuando estos son c[o]digos usando el formato definido por el DANE
corr.geonames<- function(data = x, depto = x$x, mpio =x$x, routineType = NULL) {
  
  if (is.null(routineType)){
    stop(paste( "Debe definirse la ruta donde se encuentra el archivo 'divipola_codes.rs'" ))
} else load(paste0(routineType, '/divipola_codes.rds'))

  cat(" \n  Conversi[o]n de datos politico-administrativos  \n")
  library(Hmisc)
  y<- divipola_codes[divipola_codes$codmpio %in% mpio,]
  j<-1
  corr_mpio <- as.data.frame(matrix(NA, ncol = ncol(data), nrow = (0)))
  for (j in 1:nrow(y)) {
    cat("\n", j, "of", nrow(y), round(((j*100)/nrow(y)), 2), "%" ,y$municipio[j])
    y2<- data[data$county %in% y$codmpio[j],]
    y2$county<- y$municipio[j]
    y2$stateProvince<-y$departamento[j]
    corr_mpio<-rbind(corr_mpio, y2)
  }
  
  dif<-(nrow(data))-(nrow(corr_mpio))
  if (dif>0){
  adm_error<-data[data$occurrenceID %nin% corr_mpio$occurrenceID,]
  }
  if (exists('corr_mpio') & exists('adm_error')){
    data<-list(corr_mpio, adm_error)
  } else if (exists('corr_mpio') & !exists('adm_error')) {
    data <- corr_mpio
  }
  
  cat("\n  Proceso Terminado \n ")
  
return(data)
}



##Function for unify db of ANLA to Dwc standard

#By: Cristian Cruz-Rodr[i]guez 
#Date: 26-03-2020

' Extrae la informaci[o]n de las DGB con formato de la ANLA y la incorpora a la plantilla DwC
#' 
#' @param x archivo en formato 'gdb' (las tablas y archivos geogr[a]ficos deben contener los encabezados definidos por la ANLA).
#' @return Datos obtenidos de \code{x} en plantilla con campos coincidentes siguiendo el par[a]metro DwC.
#' @examples
#' db_to_dwc(geodatabase)
db_to_dwc<-function(x){
  
  library(data.table)
  library(rgdal)
  library(sf)
  library(dplyr)
  library(stringi)
  
  #Geographical shapes
  if ("PuntoMuestreoFauna" %in% ogrListLayers(x)) {
    
    cat("\n Puntos de Muestreo Fauna\n")
    Pmf<-  as.data.frame(readOGR(dsn = x, layer ="PuntoMuestreoFauna"))
    colnames(Pmf)<-toupper(colnames(Pmf))
    
    oldnames2<-c("OPERADOR", "VEREDA", "MUNICIPIO", "DEPTO", "NOMBRE", "ID_MUES_PT", "T_MUEST", "HABITAT", "DESCRIP", "FEC_MUEST", "CUERPO_AGU", "COTA", "COOR_ESTE", "COOR_NORTE")
    newnames2<- c("institutionCode", "locality", "county", "stateProvince", "parentEventID", "eventID",  "samplingProtocol", "habitat", "samplingEffort", "eventDate", "waterBody", "minimumElevationInMeters", "verbatimLongitude", "verbatimLatitude")
    setnames(Pmf, oldnames2, newnames2, skip_absent = T)
    Pmf<-(Pmf)[newnames2]
    rm(oldnames2, newnames2) 
  } else NULL
  
  if ("TransectoMuestreoFauna" %in% ogrListLayers(x)) {
    cat(" \n Transeco de Muestreo Fauna \n ")
    tmf<- as.data.frame(as((readOGR(dsn = x, layer ="TransectoMuestreoFauna")),"SpatialPointsDataFrame"))
    tmf<-tmf[!duplicated(tmf$Lines.ID),]
    colnames(tmf)<-toupper(colnames(tmf))
                           
    oldnames0<-c("OPERADOR", "VEREDA", "MUNICIPIO", "DEPTO", "NOMBRE", "ID_MUES_TR", "T_TRANSEC", "HABITAT", "DESCRIP", "FEC_MUEST", "CUERPO_AGU", "COTA_MIN", "COORDS.X1", "COORDS.X2" )
    newnames0<- c("institutionCode", "locality", "county", "stateProvince", "parentEventID", "eventID", "samplingProtocol", "habitat", "samplingEffort", "eventDate", "waterBody", "minimumElevationInMeters", "verbatimLongitude", "verbatimLatitude")
    setnames(tmf, oldnames0, newnames0, skip_absent = T)
    tmf<- (tmf)[newnames0]
    rm(oldnames0, newnames0)
  } else NULL
  
  
  if ("PuntoMuestreoFlora" %in% ogrListLayers(x)) {
    cat("\n  Puntos de Muestreo Flora \n")
    pmfr<-  as.data.frame(st_read(dsn = x, layer ="PuntoMuestreoFlora"))
    colnames(pmfr)<-toupper(colnames(pmfr))
    
    oldnames3<-c("OPERADOR", "VEREDA", "MUNICIPIO", "DEPTO", "NOMBRE", "ID_MUEST", "N_COBERT", "T_MUEST",  "CUERPO_AGU", "DESCRIP", "FEC_MUEST", "COTA", "COOR_ESTE", "COOR_NORTE")
    newnames3<- c("institutionCode", "locality", "county", "stateProvince", "parentEventID", "eventID", "habitat", "samplingProtocol", "waterBody", "samplingEffort", "eventDate", "minimumElevationInMeters", "verbatimLongitude", "verbatimLatitude")
    setnames(pmfr, oldnames3, newnames3, skip_absent = T)
    pmfr<-(pmfr)[newnames3]
    rm(oldnames3, newnames3)
  } else NULL  
  
  #tables with biological information  
  if ("MuestreoFaunaTB" %in% ogrListLayers(x)) {
    cat("\n  Datos de Muestreo Fauna  Punto\n")
    mf1<-  as.data.frame(read_sf(dsn = x, layer ="MuestreoFaunaTB"))
    colnames(mf1)<-toupper(colnames(mf1))
    
    oldnames1<-c("ID_MUES_PT", "DIVISION", "CLASE", "ORDEN", "FAMILIA", "GENERO", "ESPECIE", "N_COMUN", "ABUND_ABS", "OBSERV")      
    newnames1<-c("eventID", "phylum", "class", "order", "family", "genus", "scientificName", "vernacularName", "organismQuantity", "occurrenceRemarks")
    setnames(mf1, oldnames1, newnames1, skip_absent = T)
    mf1<-(mf1)[newnames1]
    mf1<-mf1[!is.na(mf1$eventID) & mf1$eventID!="" & mf1$eventID!= '<Null>' & mf1$eventID!= '<NULL>',]
    rm(oldnames1, newnames1)
  } else NULL
  
  if ("MuestreoFaunaTB" %in% ogrListLayers(x)) {
    
    cat("\n  Datos de Muestreo Fauna Transecto \n")
    mf2<-  as.data.frame(st_read(dsn = x, layer ="MuestreoFaunaTB"))
    colnames(mf2)<-toupper(colnames(mf2))
    
    oldnames5<-c("ID_MUES_TR", "DIVISION", "CLASE", "ORDEN", "FAMILIA", "GENERO", "ESPECIE", "N_COMUN", "ABUND_ABS", "OBSERV")      
    newnames5<-c("eventID", "phylum", "class", "order", "family", "genus", "scientificName", "vernacularName", "organismQuantity", "occurrenceRemarks")
    setnames(mf2, oldnames5, newnames5, skip_absent = T)
    mf2<-(mf2)[newnames5]
    mf2<-mf2[!is.na(mf2$eventID) & mf2$eventID!="" & mf2$eventID!= '<Null>' & mf2$eventID!= '<NULL>',]
    rm(oldnames5, newnames5)
  } else NULL 
  
  if ("MuestreoFloraRegeneracionTB" %in% ogrListLayers(x)) {
    
    cat(" \n Datos de Muestreo Flora  \n")
    mfr<-  as.data.frame(st_read(dsn = x, layer ="MuestreoFloraRegeneracionTB"))
    colnames(mfr)<-toupper(colnames(mfr))
    
    oldnames4<-c("ID_MUEST", "DIVISION", "CLASE", "ORDEN", "FAMILIA", "GENERO", "ESPECIE", "N_COMUN", "OBSERV", "INDIVIDUOS")      
    newnames4<-c("eventID", "phylum", "class", "order", "family", "genus", "scientificName", "vernacularName", "occurrenceRemarks",  "organismQuantity")
    setnames(mfr, oldnames4, newnames4, skip_absent = T)
    mfr<-(mfr)[newnames4]
    rm(oldnames4, newnames4)
  } else NULL
  
    if ("MuestreoFloraFustalTB" %in% ogrListLayers(x)) {
  
  cat("\n  Datos de Muestreo Flora Fustal  \n")
      mff<-  as.data.frame(st_read(dsn = x, layer ="MuestreoFloraFustalTB"))
      colnames(mff)<-toupper(colnames(mff))
      
      oldnames4<-c("ID_MUEST", "DIVISION", "CLASE", "ORDEN", "FAMILIA", "GENERO", "ESPECIE", "N_COMUN", "OBSERV")      
      newnames4<-c("eventID", "phylum", "class", "order", "family", "genus", "scientificName", "vernacularName", "occurrenceRemarks")
      setnames(mff, oldnames4, newnames4, skip_absent = T)
      mff<-(mff)[newnames4]
      mff$organismQuantity <- 1
      rm(oldnames4, newnames4)
    } else NULL
  
 
  if (exists('mf1') & exists('mf2')){
    mf1<-rbind(mf1,mf2)
    rm(mf2)
  } else NULL
  
  output<-vector()
  if (exists('mf1') & exists('Pmf')& exists('tmf')){
    w<-merge(mf1, Pmf, by ='eventID', all.x = F, all.y = F)
    t<-merge(mf1, tmf, by ='eventID', all.x = F, all.y = F)
    output<-rbind(w,t)
    }else if(exists('mf1') & exists('Pmf')& !exists('tmf')){
    output<-merge(mf1, Pmf, by ='eventID', all.x = F, all.y = F)
    } else if(exists('mf1') & exists('tmf')& !exists('Pmf')){
    output<-merge(mf1, tmf, by ='eventID', all.x = F, all.y = F)
    } else NULL
  
  if(exists('pmfr') & exists('mfr')& exists('mff')){
    a<-merge(pmfr, mfr, by ='eventID', all.x = F, all.y = F)
    b<-merge(pmfr, mff, by ='eventID', all.x = F, all.y = F)
    c<-rbind(a,b)
    output<-rbind(output, c)
    } else if(exists('pmfr') & exists('mfr') & !exists('mff')){
    c<-merge(pmfr, mfr, by ='eventID', all.x = F, all.y = F)
    output<-rbind(output, c)
  } else if(exists('pmfr') & exists('mff') & !exists('mfr')){
    c<-merge(pmfr, mff, by ='eventID', all.x = F, all.y = F)
    output<-rbind(output, c)
  } else NULL
  
  ## Fauna and Flora records
  output$verbatimCoordinateSystem<- 'MAGNA-SIRGAS / Unknown Origin'
  
  cat("\n  Proceso Terminado \n ")
  
  
  return(output)
}

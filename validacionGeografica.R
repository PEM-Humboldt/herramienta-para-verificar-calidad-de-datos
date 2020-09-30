VERIFICACION_PAISES <- function(set3, routineType, maxchar = 0.2, rdata = FALSE, evalAlt = FALSE){
  library(raster)
  library(maptools)
  library(rgdal)
  library(R.utils)

  scriptMaxchar <- maxchar
  proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # routineType <- 'Colombia'
  cat("\n\n\n\n  0. Loading information", "\n")
  load(paste0(routineType, ".RData"))

  #paises <- paises[, c('NAME_0', 'NAME_1', 'NAME_2', 'ISO2', 'ISO3')]
  #paises@data <- as.data.frame(apply(paises@data, 2, function(x) {iconv(x, from = 'latin1', to ='UTF-8')}))
  if (routineType == "Colombia") {
    mpios2017 <- mpios2017[, c('MPIO_CNMBR')]
    mpios2014 <- mpios2014[, c('MPIOS')]
    mpios2011 <- mpios2011[, c('MPIOS')]
    mpios2003 <- mpios2003[, c('MPIOS')]
    mpios1993 <- mpios1993[, c('MPIOS')]
    mpios1985 <- mpios1985[, c('MPIOS')]
    mpios1973 <- mpios1973[, c('MPIOS')]
    mpios1964 <- mpios1964[, c('MPIOS')]
    casco <- casco[, c('NOMBRE_GEO')]
  }

  set3$latitud <- as.numeric(set3$latitud)
  set3$longitud <- as.numeric(set3$longitud)
  (p0 <- length(which(!is.na(set3$latitud) & !is.na(set3$longitud))))


  cat("  1. Evaluating records coordinates", "\n")

  set3$conlat <- set3$conlon <- NA

  set3$conlat[which(!is.na(set3$latitud))] <- 1 # filas con latitud
  set3$conlon[which(!is.na(set3$longitud))] <- 1 # filas con latitud


  set5 <- set3[which(!is.na(set3$conlon) & !is.na(set3$conlat)), ]
  rm(set3)

  cat("  2. Evaluating country match", "\n")

  ubicacion <- set5[, c('pais', 'departamento', 'municipio', 'latitud', 'longitud')]
  coordinates(ubicacion) =~ longitud + latitud #
  ubicacion@proj4string@projargs <- proj

  ## Evaluar otros paises
  system.time(bien_pais <- over(ubicacion, paises))
  sugerencia_paises <- na.omit(
    data.frame("NOMB" = unique(bien_pais$NAME_0),
               "ISO2" = unique(bien_pais$ISO2),
               "ISO3" = unique(bien_pais$ISO3)))

  set5$bienPais <- NA
  #set5$pais <- iconv(set5$pais, from = 'latin1', to = 'UTF-8')
  for (p in 1:nrow(sugerencia_paises)){
    set5$bienPais[grep(toString(tolower(sugerencia_paises$NOMB[p])), tolower(set5$pais))] <- 1
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO2[p]))] <- 1
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO3[p]))] <- 1
  }
  set5$bienPais[which(gsub(" ", "", set5$pais) == gsub(" ", "", bien_pais$NAME_0))] <- 1
  set5$sugerencia_pais <- bien_pais$NAME_0


  cat("  3. Evaluating State/Province match", "\n")
  #bien_pais$NAME_1 <- iconv(bien_pais$NAME_1, from = 'latin1', to ='UTF-8')
  #set5$departamento <- iconv(set5$departamento, from = 'latin1', to ='UTF-8')
  DEP <- corroboracion(overPointsField = bien_pais$NAME_1, dataField = set5$departamento,
                       dataID = set5$scriptID, maxchar = scriptMaxchar)
  set5$bien_depto[set5$scriptID %in% DEP[[1]]] <- 1
  set5$sugerencia_depto <- DEP[[3]]
  rm(DEP, paises, ubicacion)

  cat("  4. Evaluating municipality match ", "\n")
  #set5$municipio <- iconv(set5$municipio, from = 'latin1', to ='UTF-8')
  A <- corroboracion(overPointsField = bien_pais$NAME_2, dataField = set5$municipio,
                     dataID = set5$scriptID, maxchar = scriptMaxchar)
  bien_mun <- data.frame(ID = set5$scriptID, bien_mun = NA, Mapa = NA, stringsAsFactors = FALSE)
  bien_mun$bien_mun[bien_mun$ID %in% A[[1]]] <- 1
  bien_mun$Mapa[bien_mun$ID %in% A[[1]]] <- "SouthAmerica"
  sugerencia_mun <- A[[3]]
  rm(A)

  if (routineType == "World") {
    set16 <-  cbind(set5, bien_mun = bien_mun$bien_mun, sugerencia_mun, Mapa = bien_mun$Mapa)
  } else if (routineType == "Colombia") {
    set6col <- subset(set5, set5$sugerencia_pais == "Colombia")
    #set6col$municipio <- iconv(set6col$municipio, from = 'latin1', to ='UTF-8')
    coordinates(set6col) =~ longitud + latitud #
    set6col@proj4string@projargs <- proj

    select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])

    if(length(select) > 0){
      cat("    Evaluating municipality match (2017)", "\n")
      set6A <- set6col[select, ]
      #set6A$municipio <- iconv(set6A$municipio, from = 'latin1', to ='UTF-8')
      over2017 <- over(set6A, mpios2017)
      #over2014$MPIOS <- iconv(over2014$MPIOS, from = 'latin1', to ='UTF-8')
      I <- corroboracion(overPointsField = over2017$MPIOS, dataField = set6A$municipio,
                         dataID = set6A$scriptID, maxchar = scriptMaxchar)
      bien_mun$bien_mun[bien_mun$ID %in% I[[1]]] <- 1
      bien_mun$Mapa[bien_mun$ID %in% I[[1]]]  <- 'CO:2017'
      #sugerencia_mun <- I[[3]]
      rm(over2017, I, set6A, mpios2017)


      select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])

      if(length(select) > 0){
        cat("    Evaluating municipality match (2014)", "\n")
        set6A <- set6col[select, ]
        over2014 <- over(set6A, mpios2014)
        #over2011$MPIOS <- iconv(over2011$MPIOS, from = 'latin1', to ='UTF-8')
        H <- corroboracion(overPointsField = over2014$MPIOS, dataField = set6A$municipio,
                           dataID = set6A$scriptID, maxchar = scriptMaxchar)
        bien_mun$bien_mun[bien_mun$ID %in% H[[1]]] <- 1
        bien_mun$Mapa[bien_mun$ID %in% H[[1]]]  <- 'CO:2014'
        rm(H, over2014, set6A, mpios2014)

        select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])
        if(length(select) > 0){
          cat("    Evaluating municipality match (2011)", "\n")
          set6A <- set6col[select, ]
          over2011 <- over(set6A, mpios2011)
          #over2003$MPIOS <- iconv(over2003$MPIOS, from = 'latin1', to ='UTF-8')
          G <- corroboracion(overPointsField = over2011$MPIOS, dataField = set6A$municipio,
                             dataID = set6A$scriptID, maxchar = scriptMaxchar)
          bien_mun$bien_mun[bien_mun$ID %in% G[[1]]] <- 1
          bien_mun$Mapa[bien_mun$ID %in% G[[1]]]  <- 'CO:2011'
          rm(G, over2011, set6A, mpios2011)

          select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])
          if(length(select)>0){
            cat("    Evaluating municipality match (2003)", "\n")
            set6A <- set6col[select, ]
            over2003 <- over(set6A, mpios2003)
            E <- corroboracion(overPointsField = over2003$MPIOS, dataField = set6A$municipio,
                               dataID = set6A$scriptID, maxchar = scriptMaxchar)

            bien_mun$bien_mun[bien_mun$ID %in% E[[1]]] <- 1
            bien_mun$Mapa[bien_mun$ID %in% E[[1]]] <- 'CO:2003'
            rm(E, set6A, over2003, mpios2003)


            select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])
            if(length(select)>0){
              cat("    Evaluating municipality match (1993)", "\n")
              set6A <- set6col[select, ]
              over1993 <- over(set6A, mpios1993)
              D <- corroboracion(overPointsField = over1993$MPIOS, dataField = set6A$municipio,
                                 dataID = set6A$scriptID, maxchar = scriptMaxchar)

              bien_mun$bien_mun[bien_mun$ID %in% D[[1]]] <- 1
              bien_mun$Mapa[bien_mun$ID %in% D[[1]]] <- 'CO:1993'
              rm(D, mpios1993, over1993, set6A)


              select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])
              if(length(select)>0){
                cat("    Evaluating municipality match (1985)", "\n")
                set6A <- set6col[select, ]
                over1985 <- over(set6A, mpios1985)
                C <- corroboracion(overPointsField = over1985$MPIOS, dataField = set6A$municipio,
                                   dataID = set6A$scriptID, maxchar = scriptMaxchar)
                bien_mun$bien_mun[bien_mun$ID %in% C[[1]]] <- 1
                bien_mun$Mapa[bien_mun$ID %in% C[[1]]] <- 'CO:1985'
                rm(C, set6A, mpios1985, over1985)


                select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])
                if(length(select)>0){
                  cat("    Evaluating municipality match (1973)", "\n")
                  set6A <- set6col[select, ]
                  over1973 <- over(set6A, mpios1973)
                  B <- corroboracion(overPointsField = over1973$MPIOS, dataField = set6A$municipio,
                                     dataID = set6A$scriptID, maxchar = scriptMaxchar)
                  bien_mun$bien_mun[bien_mun$ID %in% B[[1]]] <- 1
                  bien_mun$Mapa[bien_mun$ID %in% B[[1]]] <- 'CO:1973'
                  rm(B, set6A, over1973, mpios1973)

                  select <- which(set6col$scriptID %in% bien_mun$ID[which(is.na(bien_mun$bien_mun))])
                  if(length(select)>0){
                    cat("    Evaluating municipality match (1964)", "\n")
                    set6A <- set6col[select, ]
                    over1964 <- over(set6A, mpios1964)
                    A <- corroboracion(overPointsField = over1964$MPIOS, dataField = set6A$municipio,
                                       dataID = set6A$scriptID, maxchar = scriptMaxchar)
                    bien_mun$bien_mun[bien_mun$ID %in% B[[1]]] <- 1
                    bien_mun$Mapa[bien_mun$ID %in% B[[1]]] <- 'CO:1964'
                    rm(A, set6A, over1964, mpios1964)
                  }
                }
              }
            }
          }
        }
      }
    }

    set8 <- cbind(set5, bien_mun = bien_mun$bien_mun, sugerencia_mun = sugerencia_mun, Mapa = bien_mun$Mapa)
    rm(bien_mun, set6col)

    cat("  5. Evaluating records in rural and urban areas", "\n")
    mr <- SpatialPoints(cbind(set8$longitud, set8$latitud))
    mr@proj4string@projargs <- proj

    en_casco <- over(mr, casco)
    set8$suggestedMunicipality <- en_casco$NOMBRE_GEO
    rm(en_casco)

    over_mar <- over(mr, mar) #
    en_mar <- which(!is.na(over_mar[, 1]))
    en_col <- which(set8$pais == "CO" | gsub(" ", "", tolower(set8$pais)) == "colombia")
    set8$bienPais[en_mar[en_mar %in% en_col]] <- 1
    rm(over_mar, en_col, en_mar)

    cat("  6. Evaluating duplicity", "\n")

    Igeodup <- set8[!is.na(set8$bien_mun), c('scriptID', 'nombre', 'longitud', 'latitud')]

    coordinates(Igeodup) = ~longitud + latitud

    system.time(Igeodup$celda <- over(Igeodup, id))
    rm(id)

    dupCell <- data.frame(scriptID = Igeodup$scriptID,
                          duplicated = duplicated(data.frame(Igeodup$nombre, Igeodup$celda)) * 1)

    set12 <- merge(set8, dupCell, by = "scriptID", all = TRUE)
    set12$duplicated[is.na(set12$duplicated)] <- 0
    rm(set8, dupCell, Igeodup)

    if (evalAlt){

      cat("  Evaluating records elevation \n")


      IGeoALT <- set12[, c('scriptID', 'longitud', 'latitud')]
      coordinates(IGeoALT) =~ longitud + latitud
      prealt <- over(IGeoALT, ALT)
      set12$alt <- as.numeric(prealt[, 1])
      rm(IGeoALT, prealt)

      # Inicia metodo para detectar outliers (modified z-score method)
      listsp <- unique(set12$nombre) #

      altDATA <- data.frame(set12[, c("scriptID", "nombre", "alt")], "extremo" = NA, stringsAsFactors = TRUE)
      count <- 0
      for (w in 1:length(listsp))    {

        cat(w, "of", length(listsp), round((w/length(listsp)) * 100, 2), "%" ,listsp[w])
        pos <- which(altDATA$nombre == listsp[w] & !is.na(altDATA$alt))
        v <- altDATA[pos, c("scriptID", "alt")]; cat (" -", nrow(v), "registros")
        if (nrow(v)>0){
          s <- median(v$alt, na.rm = TRUE)
          N <- length(v$alt)
          m <- abs(v$alt - s) #debug add
          MAD <- median(m, na.rm = TRUE) # mediana de todos los datos |xi - xm | calculados
          if (MAD > 0) {
            pZ <- cbind(v$scriptID, abs(0.6745 * (v$alt - s)/MAD)) # formula para calculo de outliers segun  # el modified z-score method
            Z <- which(pZ[, 2] > 3.5)
            if (length(Z)!=0){
              altDATA$extremo[pos[-Z]] <- 1
              count <- count + 1
              cat(" - Extremo", count)
            } # 1 no es extremo
          }
        }; cat("\n")
      }

      set12$extremo <- altDATA$extremo
      set16 <- set12
      rm(altDATA)
      rm(set12)

    } else {
      set16 <- set12
      rm(set12)
    }
  }

  set16$sugerencia_pais <- capitalize(set16$sugerencia_pais)
  set16$sugerencia_depto <- capitalize(set16$sugerencia_depto)
  set16$sugerencia_mun <- capitalize(set16$sugerencia_mun)

  cat("  7. Generating out table", "\n")

  quitar.columnas <- which(colnames(set16) %in%  c("nombre", "pais", "departamento", "municipio", "latitud", "longitud"))

  set16 <- merge(x = set2, y = set16[, -quitar.columnas], by = 'scriptID', all.x = TRUE)
  set16 <- set16[order(set16$scriptID), ]

  colnames(set16) <- gsub('conlat', 'withLat', gsub('conlon', 'withLon', gsub('bienPais', 'correctCountry', colnames(set16))))
  colnames(set16) <- gsub('sugerencia_pais', 'suggestedCountry', gsub('bien_depto', 'correctStateProvince', colnames(set16)))
  colnames(set16) <- gsub('bien_mun', 'correctCounty', gsub('Mapa', 'sourceLayer', colnames(set16)))
  colnames(set16) <- gsub('sugerencia_depto', 'suggestedStateProvince', gsub('sugerencia_mun', 'suggestedCounty', colnames(set16)))
  colnames(set16) <- gsub('celda', 'pixel', gsub('duplicados', 'pixelDuplicated', colnames(set16)))
  colnames(set16) <- gsub('alt.x', 'verbatimAlt', gsub('alt.y', 'demAlt', colnames(set16)))

  set16 <<- set16

  (p <- nrow(set16))
  (p1 <- length(which(!is.na(set16$acceptedNameUsage))))
  (p2 <- length(which(set16$correctCountry == 1)))
  (p3 <- length(which(set16$correctStateProvince == 1)))
  (p4 <- length(which(set16$correctCounty == 1)))
  (p5 <- length(which(!is.na(set16$suggestedMunicipality))))
  summary <-  c(p, p0, p1, p2, p3, p4, p5)
  if (rdata){
    registros <- set16
    save(file ='set16.RData', list = c('registros', 'summary'))
    write.csv(summary, 'summarySet16.csv')
  }

  outFile <- list(set16 = set16, summary = c(p, p0, p1, p2, p3, p4))
  cat("\n \n \n  Script end \n \n \n \n")
  return(outFile)
}

stringMatch <- function(pattern, x, maxChar){
  # textB <- 'SataTeresa'; textA <- 'Santa Tereza'
  # maxChar <- 'A0.33'; maxChar <- 'B0.8'; maxChar <- 2
  isNumMaxChar <- as.numeric(maxChar)
  if (is.numeric(isNumMaxChar) & !is.na(isNumMaxChar)){
    nChar <- as.numeric(maxChar)
  } else if(length(grep('p', maxChar)) > 0) {
    porpA <- as.numeric(gsub('p', '', maxChar))
    nChar <- porpA * nchar(pattern)[1]

  } else if(length(grep('x', maxChar)) > 0) {
    porpB <- as.numeric(gsub('x', '', maxChar))
    nChar <- porpB * nchar(x)[1]
  }
  return(agrep(pattern = pattern, x = x, max = nChar, value = FALSE, ignore.case = TRUE))
}


corroboracion <- function(overPointsField, dataField, dataID, maxchar = 0.2){

  targetLayerField <- overPointsField
  lenTableLocalities <- length(which(!is.na(dataField)))

  if(lenTableLocalities == 1){
    tmp <- stringMatch(targetLayerField, dataField, maxChar = maxchar)
    correctRows <- dataID[tmp]
  } else if(lenTableLocalities > 1){
    eqLayerNTable <- which(gsub('[[:blank:]]', '', tolower(targetLayerField)) == gsub('[[:blank:]]', '', tolower(dataField))) ## datos con igual entidad # jmx
    difLayerNTable <- which(gsub('[[:blank:]]', '', tolower(targetLayerField)) != gsub('[[:blank:]]', '', tolower(dataField))) ## datos con diferente entidad # imx

    na.Rows <- which(is.na(targetLayerField))
    id.eqRows <- dataID[eqLayerNTable] # ID's de las filas exactas # id.exa

    CompareMun <- data.frame(posDifRows = difLayerNTable, layerValue = targetLayerField[difLayerNTable],
                             tableValue = as.character(dataField[difLayerNTable]), tableID = dataID[difLayerNTable], stringsAsFactors = FALSE)

    uniqueLayerMun <- sort(unique(CompareMun$layerValue)) # Saco valores unicos por municipio reportados en la capa
    (uniqueLayerMun <- uniqueLayerMun[which(!is.na(uniqueLayerMun) & uniqueLayerMun != '')])

    mmx <- c(0, 0)
    nmx <- c(0, 0)

    if (length(uniqueLayerMun) > 0){
      validRows <- NULL
      for (i in 1:length(uniqueLayerMun)){
        (pos.mun <- which(CompareMun$layerValue == uniqueLayerMun[i])) # Selecciono posiciones que contienen al municipio[i]
        if (length(pos.mun)> 0){

          # Sel. municipios de tabla para el municipio de tabla
          (mun.i <- CompareMun$tableValue[pos.mun])

          # Comparo similitud entre municipios reportados en tabla y extraidos con coordenada
          tmp <- stringMatch(pattern =  gsub('[[:blank:]]', '', uniqueLayerMun[i]), x = gsub('[[:blank:]]', '', mun.i), maxChar = maxchar)
          #(tmp <- agrep(gsub(" ", "", uniqueLayerMun[i]), gsub(" ", "", mun.i), max = 2, value = FALSE, ignore.case = TRUE))

          # Genero tabla con los ID de las filas bien
          (validRows <- c(validRows, CompareMun$tableID[pos.mun][tmp]))
        }
      }
      correctRows <- sort(as.integer(c(id.eqRows, validRows))) # Filas de la tabla con datos validados positivamente

    } else if (length(id.eqRows) > 0 & length(uniqueLayerMun) == 0){
      correctRows <- sort(as.integer(c(id.eqRows)))
    } else {
      correctRows <- 0
    }
  }

  output <- list()
  output[[1]] <- correctRows
  output[[2]] <- 0
  output[[3]] <- targetLayerField
  return(output)
}

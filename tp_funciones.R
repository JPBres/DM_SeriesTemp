convertToDate <- function(tiempo) {
  stiempo <- toString(tiempo)  
  year <- substring(tiempo, 1, 4)
  month <- substring(tiempo, 5, 6)
  day <- substring(tiempo, 7, 8)
  sdate <- paste(year,month,day, sep='')
  result <- as.Date(sdate, "%Y%m%d")
  return(result)
}

fillData <- function(data){
  #desde 1 ene 2014 hasta 30 dic 2015
  fechas = data.frame(as.character(as.Date(0:729, origin='2014-01-01')), c(0:729))  #, character(730), character(730), character(730), character(730))
  colnames(fechas) = c('fecha_oficia','numero_dia') #, 'aduana', 'aduana_desc', 'mercaderia', 'mercaderia_desc')
  result <- data
  result$fecha_oficia <- paste(substring(result$fecha_oficia, 1, 4), substring(result$fecha_oficia, 5, 6), substring(result$fecha_oficia, 7, 8), sep='-')
  
  result <- merge(fechas, result, by='fecha_oficia', all.x=TRUE)  
  result[,"aduana"] <- data$aduana[1]
  result[,"aduana_desc"] <- data$aduana_desc[1]
  result[,"mercaderia"] <- data$mercaderia[1]
  result[,"mercaderia_desc"] <- data$mercaderia_desc[1]
  
  #relleno con 0s
  result[is.na(result$cantidad),"cantidad"] <- 0
  result[is.na(result$kilos),"kilos"] <- 0
  result[is.na(result$importe),"importe"] <- 0
  
  return(result)
}

leerCSV <- function(nombre) {
  serie <- read.csv(file=nombre, header=TRUE, sep = "|")
  serie = serie[which(serie$mercaderia_desc!=''),]
  serie = serie[order(serie[,1]),]
  return(serie)
}

rellenarDias <- function(serie) {
  # innecesario normalizar bisiestos
  fechas = data.frame(as.character(as.Date(0:729, origin='2014-01-01')), c(0:729))
  colnames(fechas) = c('fecha_oficia','numero_dia')
  rellenado = serie
  rellenado$fecha_oficia = paste(substring(rellenado$fecha_oficia, 1, 4), substring(rellenado$fecha_oficia, 5, 6), substring(rellenado$fecha_oficia, 7, 8), sep='-')
  rellenado = merge(fechas, rellenado, by='fecha_oficia', all.x=TRUE)
  return(rellenado)
}

interpolarValores <- function(serie, interpolar) {
  interpolar = ifelse(interpolar & length(which(!is.na(serie$importe))) > 1, interpolar, F);
  if (interpolar) {
    # spline mete datos negativos, approx no
    interpolado = approx(serie$importe,n=nrow(serie))
  } else {
    interpolado = data.frame(x=c(1:nrow(serie)), y=ifelse(is.na(serie$importe), 0, serie$importe))
  }
  return(interpolado)
}

plotFFT <- function(fftmod, N, maxsindc, indicepicomax) {
  t = 0:(N-1)
  frecmuestreo = 1
  deltafrec = frecmuestreo/N
  frec = deltafrec*t
  if (is.na(maxsindc) | is.na(indicepicomax)) {
    maxsindc = max(fftmod[c(2:length(fftmod))])
    plot(frec, fftmod, type='l', xlim=c(0,1/2), ylim=c(0,maxsindc))
  } else {
    frecpicomax = frec[indicepicomax]
    frecpicomax = frecpicomax[1]
    frecpicomax.str = paste('cada',signif(1/frecpicomax,2),'d')
    plot(frec, fftmod, type='l', xlim=c(0,1/2), ylim=c(0,maxsindc), xaxt='n')
    axis(side=1, at=c(seq(0, 1, by=0.2), frecpicomax), labels = FALSE)
    text(x=c(seq(0, 1, by=0.2), frecpicomax), par("usr")[3], labels=c(seq(0, 1, by=0.2), frecpicomax.str), srt = 45, pos = 1, xpd = TRUE)
    abline(v=frecpicomax, lty=3)
  }
}
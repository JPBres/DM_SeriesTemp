workingDirectory = '~/Documents/maestria/series\ temporales/tp/subsets/'  
setwd(workingDirectory)

inputDirectory = '~/Documents/maestria/series\ temporales/tp/subsets'  
outputDirectory = '~/Documents/maestria/series\ temporales/tp/output/'


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

getFFTPower <- function(fft.data){
  #http://www.sc.ehu.es/sbweb/energias-renovables/MATLAB/datos/fourier/fourier_1.html
  power <- Mod(fft.data) ^ 2   #espectro.potencia
  return(power);
}

plotPower <- function(power, fileName){
  n <- length(power)
  dt <- 1 #diario
  t <- c(0:(n-1)) * dt    #vector de tiempos
  dw <- 2*pi/(n*dt)
  w <- c(0:(n-1))*dw    #vector de frecuencias angulares
  
  jpeg(paste(outputDirectory,fileName,"-power.jpg"))
  plot(w, power, type='p', pch=16, cex=0.6, xaxt="n", main=fileName)
  dev.off()
  
  
  return(0)
}

plotFFTI <- function(fft.data, fileName)
{
  n <- length(fft.data)
  dt <- 1 #diario
  t <- c(0:(n-1)) * dt    #vector de tiempos
  
  ffti.data <- fft(fft.data, inverse=TRUE)/n
  
  jpeg(paste(outputDirectory,fileName,"-ffti.jpg"))
  plot (t,Re(ffti.data),type='p', pch=16, cex=0.6, main=fileName)
  dev.off()
  
  return(0)
  
}

getMax <- function(data, cantElemsResult){
  
  result <- numeric(cantElemsResult)
  
  v <- data
  
  for (i in 1:cantElemsResult){
    tmp <-  max(v) 
    result[i] <- tmp
    v <- v[v != tmp]
  }

  return(result)
}

getIndexFromValues <- function(data, values){
  
  cantElemsResult <- length(values) 
  
  result <- numeric(cantElemsResult)
  
  for (i in 1:cantElemsResult){
    result[i] <- which(data==values[i])[1]
  }
  
  return(result)
}

analyzeField <- function(data.field, fileName)
{
  
  fft.data <- fft(data.field[,2])
  
  power <- getFFTPower(fft.data)
  plotPower(power, fileName)
  
  #cuales son los maximos? busco 10 maximos:
  maxs <- getMax(power, 10)
  wIndex <- getIndexFromValues(power, maxs)
  
  power.filtrado <- power
  for(i in 1:length(power.filtrado))
  {
    if (! i %in% wIndex)
    {
      power.filtrado[i] <- 0
    }
  }
  plotPower(power.filtrado, fileName)
  
  #saco todas las frecuencias menos las de maximas amplitud
  fft.data.filtrado <- fft.data
  for(i in 1:length(fft.data.filtrado))
  {
    if (! i %in% wIndex)
    {
      fft.data.filtrado[i] <- 0
    }
  }
  
  #antitransformo y grafico:
  plotFFTI(fft.data.filtrado, fileName)
  
  return(0)  
}



analyzeCantidad <- function(data, fileName)
{
  #analizo cantidad, me quedo con las columnas cantidad y tiempo

  jpeg(paste(outputDirectory,fileName,"-cantidad.jpg"))
  plot(data$numero_dia, data$cantidad, type='p', pch=16, cex=0.6, xaxt="n", main=fileName)
  dev.off()
  
  data.cantidad <- data[, c("numero_dia", "cantidad")]
  
  analyzeField(data.cantidad, fileName)
  
  return(0)
}


analyzeKilos <- function(data, fileName)
{

  jpeg(paste(outputDirectory,fileName,"-kilos.jpg"))
  plot(data$numero_dia, data$kilos, type='p', pch=16, cex=0.6, xaxt="n", main=fileName)
  dev.off()
  
  data.kilos <- data[, c("numero_dia", "kilos")]
  
  analyzeField(data.kilos, fileName)
  
  return(0)
}


analyzeImporte <- function(data, fileName)
{
  #analizo cantidad, me quedo con las columnas cantidad y tiempo
  
  #plot(data$numero_dia, data$importe, type='p', pch=16, cex=0.6, xaxt="n", main=fileName)
  
  jpeg(paste(outputDirectory,fileName,"-importe.jpg"))
  plot(data$numero_dia, data$importe, type='p', pch=16, cex=0.6, xaxt="n", main=fileName)
  dev.off()
  
  
  data.importe <- data[, c("numero_dia", "importe")]
  
  analyzeField(data.importe, fileName)
  
  return(0)
}


analizeFile <- function(fileName)
{
  #fileName <- "series_2014_2015_aduana_10_mercaderia_44.csv"
  
  mydata.subset <- read.csv(file.path(inputDirectory, fileName), header=TRUE, sep = "|")
  #mydata.subset <- read.csv(file=fileName, header=TRUE, sep = "|")
  mydata.subset <- mydata.subset[order(mydata.subset$fecha_oficia),]
  mydata.subset.rellenado <- fillData(mydata.subset)
  
  mainTitle = paste(fileName,' - ', "cantidad")
  analyzeCantidad(mydata.subset.rellenado, mainTitle)
  
  mainTitle = paste(fileName,' - ', "kilos")
  analyzeKilos(mydata.subset.rellenado, mainTitle)
  
  mainTitle = paste(fileName,' - ', "importe")
  analyzeImporte(mydata.subset.rellenado, mainTitle)
  
  return(0)
  
}


getFilesNames <- function(dir)
{
  result <- list.files(path = ".", pattern = NULL, all.files = FALSE,
            full.names = FALSE, recursive = FALSE,
            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  return(result)
}




#paula domingo 29/5

filesNames <- getFilesNames(workingDirectory)


#for(file in filesNames[1:10])
for(file in filesNames)
{
  #mydata.subset.file = 'series_2014_2015_aduana_37_mercaderia_3.csv'
  #analizeFile(mydata.subset.file)
  if(file != "borrar")
  {
    analizeFile(file)
  }

}













#workingDirectory = 'F:\\tp-series temporales\\subsets\\'  
#setwd(workingDirectory)

leerCSV <- function(nombre) {
	serie <- read.csv(file=nombre, header=TRUE, sep = "|")
	# repite fecha? series_2014_2015_aduana_1_mercaderia_22.csv
	# [25] 2014-01-25 2014-01-26 2014-01-27 2014-01-28 2014-01-29 2014-01-30
	# [31] 2014-01-30 2014-01-31 2014-02-01 2014-02-02 2014-02-03 2014-02-04
	# otros
	#series_2014_2015_aduana_1_mercaderia_22.csv:20140130|1|"BS.AS.(CAPITAL)"|22|""|1|5940|25460
	#series_2014_2015_aduana_1_mercaderia_22.csv:20140805|1|"BS.AS.(CAPITAL)"|22|""|1|180|1300
	#series_2014_2015_aduana_38_mercaderia_39.csv:20140602|38|"MENDOZA"|39|""|1|15|276
	serie = serie[which(serie$mercaderia_desc!=''),]
	serie = serie[order(serie$fecha_oficia),]
	return(serie)
}

convertToDate <- function(tiempo) {
  
  stiempo <- toString(tiempo)
  
  year <- substring(tiempo, 1, 4)
  month <- substring(tiempo, 5, 6)
  day <- substring(tiempo, 7, 8)
  
  sdate <- paste(year,month,day, sep='')
  
  result <- as.Date(sdate, "%Y%m%d")
  
  return(result)
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

interpolar = TRUE;
#interpolar = FALSE;
##############################################################

# series_2014_2015_aduana_37_mercaderia_3 tiene muchos dias con datos (pero no para todos los dias)
mydata.subset.file = 'series_2014_2015_aduana_37_mercaderia_3.csv'
# para exagerar discontinuidades, series_2014_2015_aduana_8_mercaderia_73 tiene muchos menos dias con datos
#mydata.subset.file = 'series_2014_2015_aduana_8_mercaderia_73.csv'

# plot datos originales segun script de Paula
# plot parece 'rellenar' dias pero en realidad solo lo hace en forma grafica, los dias no quedan agregados al data set
mydata.subset = leerCSV(mydata.subset.file)
plot(convertToDate(mydata.subset$fecha_oficia), mydata.subset$importe, type='l', pch=16, cex=0.6, xaxt="n")
axis.Date(1, at=seq(as.Date('20140101', "%Y%m%d"), as.Date('20160101', "%Y%m%d"), by="30 day"), format="%m-%Y", las=2)

# faltan varios dias: 'rellenar' (aun sin dato de importe) para tener equiespaciado
mydata.subset.rellenado = rellenarDias(mydata.subset)
plot(mydata.subset.rellenado$numero_dia, mydata.subset.rellenado$importe, type='l', pch=16, cex=0.6, xaxt="n")
points(mydata.subset.rellenado$numero_dia, mydata.subset.rellenado$importe, pch='.', cex=3)

# para completar valor de importe en dias metidos de 'relleno' interpolar y comparar con original (en linea punteada valores interpolados)
# spline mete datos negativos, approx no
mydata.subset.rellenado.interpolado = interpolarValores(mydata.subset.rellenado, interpolar)
plot(mydata.subset.rellenado$importe, type='l')
lines(mydata.subset.rellenado.interpolado$y, lty=3)

# usando datos interpolados y ya no original
plot(as.Date(mydata.subset.rellenado.interpolado$x, origin='2014-01-01'), mydata.subset.rellenado.interpolado$y, type='l', xaxt="n")
axis.Date(1, at=seq(as.Date('20140101', "%Y%m%d"), as.Date('20160101', "%Y%m%d"), by="30 day"), format="%m-%Y", las=2)

# fft para detectar picos en frecuencias especificas, filtrar, etc
mydata.subset.rellenado.N = 365*2
mydata.subset.rellenado.N = length(mydata.subset.rellenado.interpolado$x)
fft.mydata.subset.rellenado.interpolado = fft(mydata.subset.rellenado.interpolado$y)
fftmod.mydata.subset.rellenado.interpolado = Mod(fft.mydata.subset.rellenado.interpolado)
# picos evitar tomar niveldc
fftmod.mydata.subset.rellenado.interpolado.ordsindc = sort(fftmod.mydata.subset.rellenado.interpolado[c(2:(length(fftmod.mydata.subset.rellenado.interpolado)/2))], decreasing=T)
fftmod.mydata.subset.rellenado.interpolado.maxsindc = fftmod.mydata.subset.rellenado.interpolado.ordsindc[1]

# detectar picos en frecuencias especificas
# por ejemplo en series_2014_2015_aduana_37_mercaderia_3: estacionalidad semanal?
mydata.subset.rellenado.indicepicomax = which(fftmod.mydata.subset.rellenado.interpolado==fftmod.mydata.subset.rellenado.interpolado.maxsindc)
plotFFT(fftmod.mydata.subset.rellenado.interpolado, mydata.subset.rellenado.N, fftmod.mydata.subset.rellenado.interpolado.maxsindc, mydata.subset.rellenado.indicepicomax)

# filtrar estacionalidad
# sin estacionalidad
filtro.quitaestacionalidad = rep(1, mydata.subset.rellenado.N)
# como pasa-bajos?
filtro.quitaestacionalidad[mydata.subset.rellenado.indicepicomax[1]:mydata.subset.rellenado.indicepicomax[2]] = 0
fft.mydata.subset.rellenado.interpolado.sinestacionalidad = filtro.quitaestacionalidad * fft.mydata.subset.rellenado.interpolado
fftmod.mydata.subset.rellenado.interpolado.sinestacionalidad = Mod(fft.mydata.subset.rellenado.interpolado.sinestacionalidad)

mydata.subset.rellenado.interpolado.sinestacionalidad = Re(fft(fft.mydata.subset.rellenado.interpolado.sinestacionalidad, inverse=TRUE) / mydata.subset.rellenado.N)
plot(as.Date(mydata.subset.rellenado.interpolado$x, origin='2014-01-01'), mydata.subset.rellenado.interpolado.sinestacionalidad, type='l', xaxt="n")
axis.Date(1, at=seq(as.Date('20140101', "%Y%m%d"), as.Date('20160101', "%Y%m%d"), by="30 day"), format="%m-%Y", las=2)


#fft.mydata.subset.rellenado.interpolado[1] = 0
#fft.mydata.subset.rellenado.interpolado[2] = 0

#paula:

N <- length(fft.mydata.subset.rellenado.interpolado)
tiempo = 0:(N-1)
filtroDF = rep(1,N)
filtroDF[1:15] = 0
filtroDF[(N-15):N] = 0
fft.mydata.subset.rellenado.interpolado.DF = filtroDF * fft.mydata.subset.rellenado.interpolado
#op <- par(mfrow = c(1, 2))
plot (tiempo, filtroDF ,type='l')
plot (Mod(fft.mydata.subset.rellenado.interpolado.DF),type='l')

tiempo = 0:(N-1)
mydata.subset.rellenado.interpolado.DF =  Re(fft(fft.mydata.subset.rellenado.interpolado.DF,inverse=TRUE)/N)
#mydata.subset.rellenado.interpolado.DF[1] <- 0
#mydata.subset.rellenado.interpolado.DF[2] <- 0
#mydata.subset.rellenado.interpolado.DF[N] <- 0
plot (tiempo,mydata.subset.rellenado.interpolado.DF,type='l')
#lines (tiempo,mydata.subset.rellenado.interpolado.DF,col='red')



#filtrado media movil (clase 5)
mydata.subset.rellenado.interpolado.MA3 = filter(mydata.subset.rellenado.interpolado$y, rep(1/3,3) , circular =TRUE)
lines (mydata.subset.rellenado.interpolado$x, mydata.subset.rellenado.interpolado.MA3, col='red')


fft.mydata.subset.rellenado.interpolado.MA3 = Mod(fft(mydata.subset.rellenado.interpolado.MA3))
#quito la primera frecuencia porque es muy alta y deforma el grafico:
fft.mydata.subset.rellenado.interpolado.MA3[1] <- 0
plot(fft.mydata.subset.rellenado.interpolado.MA3, type='l')

mydata.subset.rellenado.interpolado.anti =  Re(fft(fft.mydata.subset.rellenado.interpolado.MA3,inverse=TRUE)/N)
plot (mydata.subset.rellenado.interpolado$x, mydata.subset.rellenado.interpolado.anti,type='l')

# Busca 14 dias
mydata.subset = leerCSV(mydata.subset.file)
mydata.subset <- subset(mydata.subset,fecha_oficia >= 20140131 & mydata.subset$fecha_oficia < 20140215)
plot(convertToDate(mydata.subset$fecha_oficia), mydata.subset$importe, type='l', pch=16, cex=0.6, xaxt="n",xlab='dias',ylab='importe')
axis.Date(1, at=seq(as.Date('20140131', "%Y%m%d"), as.Date('20140214', "%Y%m%d"), by="1 day"), format="%d-%m-%Y", las=2)

# Rellena dias sin datos. Damian
fechas = data.frame(as.character(as.Date(0:14, origin='2014-01-31')), c(0:14))
colnames(fechas) = c('fecha_oficia','numero_dia')
mydata.subset.rellenado = mydata.subset
mydata.subset.rellenado$fecha_oficia = paste(substring(mydata.subset.rellenado$fecha_oficia, 1, 4), substring(mydata.subset.rellenado$fecha_oficia, 5, 6), substring(mydata.subset.rellenado$fecha_oficia, 7, 8), sep='-')
mydata.subset.rellenado = merge(fechas, mydata.subset.rellenado, by='fecha_oficia', all.x=TRUE)
# Reemplaza NA por ceros
mydata.subset.rellenado$fecha_oficia <- as.Date(mydata.subset.rellenado$fecha_oficia,"%Y-%m-%d")
mydata.subset.rellenado$cantidad = sapply(mydata.subset.rellenado$cantidad,function(x) if(is.na(x)){ 0 }else{ x})
mydata.subset.rellenado$kilos = sapply(mydata.subset.rellenado$kilos,function(x) if(is.na(x)){ 0 }else{ x})
mydata.subset.rellenado$importe = sapply(mydata.subset.rellenado$importe,function(x) if(is.na(x)){ 0 }else{ x})

plot(mydata.subset.rellenado$fecha_oficia, mydata.subset.rellenado$importe, type="l", pch=16, cex=0.6, xaxt="n")
axis.Date(1, mydata.subset.rellenado$fecha_oficia, format="%d-%m", las=2)
title(xlab='dias', line=4)
title(ylab='importe')

# fft para detectar picos en frecuencias 
fft.mydata.subset.rellenado = fft(mydata.subset.rellenado$importe)
plot(Mod(fft.mydata.subset.rellenado), type='l')

# Filtro Pasa Altos. Paula
N <- length(fft.mydata.subset.rellenado)
Nfiltro <- 2
filtroDF = rep(1,N)
filtroDF[1:Nfiltro] = 0
filtroDF[(N-Nfiltro):N] = 0
fft.mydata.subset.rellenado.DF = filtroDF * fft.mydata.subset.rellenado
# muestra grafico en fft
op <- par(mfrow = c(1, 2))
plot ( filtroDF ,type='l')
plot (Mod(fft.mydata.subset.rellenado.DF),type='l')
op <- par(mfrow = c(1, 1))
# Vuelve a tiempo
tiempo = 0:(N-1)
mydata.subset.rellenado.DF =  Re(fft(fft.mydata.subset.rellenado.DF,inverse=TRUE)/N)

# Compara ambos. fft elimina los picos, parece que tiene negativos
plot(tiempo, mydata.subset.rellenado$importe, type="l",ylab='importe')
lines (tiempo,mydata.subset.rellenado.DF,col='red')
title(ylab='importe',main="Relación entre original y luego de fft")


##############################################################
#Normaliza las variables numericas, ya rellenas con todos los dias y con ceros.
mydata.subset.rellenado$cantidad.scaled  <- scale(mydata.subset.rellenado$cantidad)
mydata.subset.rellenado$kilos.scaled <- scale(mydata.subset.rellenado$kilos)
mydata.subset.rellenado$importe.scaled <- scale(mydata.subset.rellenado$importe)
# se comparan
plot(  tiempo,mydata.subset.rellenado$importe.scaled , type="l",ylab='valores')
lines (tiempo,mydata.subset.rellenado$kilos.scaled,col='red')
lines (tiempo,mydata.subset.rellenado$cantidad.scaled,col='green')
# Correlaciones
ccf(drop(mydata.subset.rellenado$importe.scaled),drop(mydata.subset.rellenado$kilos.scaled),lag.max=50)
ccf(drop(mydata.subset.rellenado$importe.scaled),drop(mydata.subset.rellenado$cantidad.scaled),lag.max=50)
ccf(drop(mydata.subset.rellenado$kilos.scaled),drop(mydata.subset.rellenado$cantidad.scaled),lag.max=50)


#workingDirectory = 'F:\\tp-series temporales\\subsets\\'  
#setwd(workingDirectory)

convertToDate <- function(tiempo) {
  
  stiempo <- toString(tiempo)
  
  year <- substring(tiempo, 1, 4)
  month <- substring(tiempo, 5, 6)
  day <- substring(tiempo, 7, 8)
  
  sdate <- paste(year,month,day, sep='')
  
  result <- as.Date(sdate, "%Y%m%d")
  
  return(result)
}

##############################################################

# series_2014_2015_aduana_37_mercaderia_3 tiene muchos dias con datos (pero no para todos los dias)
mydata.subset.file = 'series_2014_2015_aduana_37_mercaderia_3.csv'
# para exagerar discontinuidades, series_2014_2015_aduana_8_mercaderia_73 tiene muchos menos dias con datos
#mydata.subset.file = 'series_2014_2015_aduana_8_mercaderia_73.csv'

# plot datos originales segun script de Paula
# plot parece 'rellenar' dias pero en realidad solo lo hace en forma grafica, los dias no quedan agregados al data set
mydata.subset <- read.csv(file=mydata.subset.file, header=TRUE, sep = "|")
mydata.subset = mydata.subset[order(mydata.subset$fecha_oficia),]
plot(convertToDate(mydata.subset$fecha_oficia), mydata.subset$importe, type='l', pch=16, cex=0.6, xaxt="n")
axis.Date(1, at=seq(as.Date('20140101', "%Y%m%d"), as.Date('20160101', "%Y%m%d"), by="30 day"), format="%m-%Y", las=2)

# faltan varios dias: 'rellenar' (aun sin dato de importe) para tener equiespaciado
fechas = data.frame(as.character(as.Date(0:729, origin='2014-01-01')), c(0:729))
colnames(fechas) = c('fecha_oficia','numero_dia')
mydata.subset.rellenado = mydata.subset
mydata.subset.rellenado$fecha_oficia = paste(substring(mydata.subset.rellenado$fecha_oficia, 1, 4), substring(mydata.subset.rellenado$fecha_oficia, 5, 6), substring(mydata.subset.rellenado$fecha_oficia, 7, 8), sep='-')
mydata.subset.rellenado = merge(fechas, mydata.subset.rellenado, by='fecha_oficia', all.x=TRUE)
plot(mydata.subset.rellenado$numero_dia, mydata.subset.rellenado$importe, type='l', pch=16, cex=0.6, xaxt="n")
points(mydata.subset.rellenado$numero_dia, mydata.subset.rellenado$importe, pch='.', cex=3)

# para completar valor de importe en dias metidos de 'relleno' interpolar y comparar con original (en linea punteada valores interpolados)
# spline mete datos negativos, approx no
mydata.subset.rellenado.interpolado = approx(mydata.subset.rellenado$importe,n=nrow(mydata.subset.rellenado))
plot(mydata.subset.rellenado$importe, type='l')
lines(mydata.subset.rellenado.interpolado$y, lty=3)

# usando datos interpolados y ya no original
plot(as.Date(mydata.subset.rellenado.interpolado$x, origin='2014-01-01'), mydata.subset.rellenado.interpolado$y, type='l', xaxt="n")
axis.Date(1, at=seq(as.Date('20140101', "%Y%m%d"), as.Date('20160101', "%Y%m%d"), by="30 day"), format="%m-%Y", las=2)

# fft para detectar picos en frecuencias especificas
fft.mydata.subset.rellenado.interpolado = fft(mydata.subset.rellenado.interpolado$y)
fftmod.mydata.subset.rellenado.interpolado = Mod(fft.mydata.subset.rellenado.interpolado)
# picos evitar tomar niveldc
fftmod.mydata.subset.rellenado.interpolado.ordsindc = sort(fftmod.mydata.subset.rellenado.interpolado[c(2:(length(fftmod.mydata.subset.rellenado.interpolado)/2))], decreasing=T)
fftmod.mydata.subset.rellenado.interpolado.maxsindc = fftmod.mydata.subset.rellenado.interpolado.ordsindc[1]

# detectar picos en frecuencias especificas
# por ejemplo en series_2014_2015_aduana_37_mercaderia_3: estacionalidad semanal?
mydata.subset.rellenado.N = 365*2
mydata.subset.rellenado.N = length(mydata.subset.rellenado.interpolado$x)
mydata.subset.rellenado.t = 0:(mydata.subset.rellenado.N-1)
mydata.subset.rellenado.frecmuestreo = 1
mydata.subset.rellenado.deltafrec = mydata.subset.rellenado.frecmuestreo/mydata.subset.rellenado.N
mydata.subset.rellenado.frec = mydata.subset.rellenado.deltafrec*mydata.subset.rellenado.t
mydata.subset.rellenado.indicepicomax = which(fftmod.mydata.subset.rellenado.interpolado==fftmod.mydata.subset.rellenado.interpolado.maxsindc)
mydata.subset.rellenado.frecpicomax = mydata.subset.rellenado.frec[mydata.subset.rellenado.indicepicomax]
mydata.subset.rellenado.frecpicomax = mydata.subset.rellenado.frecpicomax[1]
mydata.subset.rellenado.frecpicomax.str = paste('cada',signif(1/mydata.subset.rellenado.frecpicomax,2),'d')
plot(mydata.subset.rellenado.frec, fftmod.mydata.subset.rellenado.interpolado, type='l', xlim=c(0,1/2), ylim=c(0,fftmod.mydata.subset.rellenado.interpolado.maxsindc), xaxt='n')
axis(side=1, at=c(seq(0, 1, by=0.2), mydata.subset.rellenado.frecpicomax), labels = FALSE)
text(x=c(seq(0, 1, by=0.2), mydata.subset.rellenado.frecpicomax), par("usr")[3], labels=c(seq(0, 1, by=0.2), mydata.subset.rellenado.frecpicomax.str), srt = 45, pos = 1, xpd = TRUE)
abline(v=mydata.subset.rellenado.frecpicomax, lty=3)

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

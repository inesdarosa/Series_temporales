www <- 'https://raw.githubusercontent.com/AtefOuni/ts/master/Data/Herald.dat'
Herald.dat <- read.table(www, header = T)
attach (Herald.dat)

# Distintas formas de calcular covarianza
x <- CO; y <- Benzoa; n <- length(x)
sum((x - mean(x))*(y - mean(y))) / (n - 1)

mean((x - mean(x)) * (y - mean(y)))# E[(x − µx)(y − µy)]

cov(x, y)

# Dos formas de calculo de correlación

cov(x,y) / (sd(x)*sd(y))
cor(x,y)


www <- 'https://raw.githubusercontent.com/AtefOuni/ts/master/Data/wave.dat'
wave.dat <- read.table (www, header=T) ; attach(wave.dat)
# se ve valores consecutivos más o menos similares con una cuasi-periodicidad
# pero sin frecuencia fija:
plot(ts(waveht)) ; plot(ts(waveht[1:60]))

# Autocorrelación:
acf(waveht)$acf[1]# esto es la correlación consigo mismo, sin lag
acf(waveht)$acf[2]# acá es con un lag=1

plot(waveht[1:396],waveht[2:397])# plot entre xt y xt+1.
acf(waveht, type = c("covariance"))$acf[2]# Esto es la autocovarianza de lag=1
# También muestra la autocovarianza en función del lag.

acf(waveht)#Esto muestra el Correlograma.

acf(AirPassengers)


data(AirPassengers)
AP <- AirPassengers

# Aquí la serie esta ajustada estacionalmente y la tendencia está removida
# usando la función decompose()
AP.decom <- decompose(AP, "multiplicative")
# Dado el cálculo a través de medias móviles eso hace que los primeros seis
# y últimos seis valores no pueden ser calculados en el componente randómico,
# por eso los valores de los índices abajo
plot(ts(AP.decom$random[7:138]))# ploteo del componente randómico
acf(AP.decom$random[7:138])# ploteo del correlograma del componente randómico

# El correlograma muestra un resultado consistente con un modelo autoregresivo
# de orden 2.

sd(AP[7:138])# desvío en la serie original
sd(AP[7:138] - AP.decom$trend[7:138])# desvío luego de remover la tendencia
sd(AP.decom$random[7:138])# desvío luego del ajuste estacional, muestra una gran
                          # reducción lo que sugiere que el ajuste estacional fue
                          # efectivo.

www <- 'https://raw.githubusercontent.com/AtefOuni/ts/master/Data/Fontdsdt.dat'
Fontdsdt.dat <- read.table(www, header=T)
attach(Fontdsdt.dat)

# adflow es el residuo de la serie original luego de haber estimado la tendencia
# y estacionalidad a través de regresiones. Se percibe que adflow es estacionaria

plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = 'lag (months)', main="")

# El resultado plantea que el modelo autoregresivo es de orden 1. Lo que indicaría
# que es probable que el flujo del mes que viene no supere el promedio si el flujo
# de este mes no supera el promedio.También, si el influjo del mes actual está por
# debajo del promedio la del mes que viene estará por debajo del promedio.




















data("AirPassengers")
AP <- AirPassengers
AP
class(AP);start(AP)
end(AP); frequency(AP)
summary(AP)
plot(AP, ylab='Passengenrs (1000s)')

layout(1:2)
plot(aggregate(AP)) # la función aggregate saca la estacionalidad del gráfico
boxplot(AP ~ cycle(AP)) # la funcion cycle extrae la estacionalidad para cada
                        # item, cada número es un més, el 6 es junio.


www <- 'https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/Maine.dat'
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month)
class(Maine.month)

# con ts() transformo el dataframe en sere temporal, start indico donde empieza
# la serie que considero, por ej en el primer caso en 1996 mes 1.
Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
Maine.annual.ts <- aggregate(Maine.month.ts)/12

layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")

Maine.Feb <- window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/USunemp.dat'
US.month <- read.table(www, header = T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq = 12)
plot(US.month.ts, ylab = "unemployed (%)")

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/cbe.dat'
CBE <- read.table(www, header = T)
CBE[1:5, ]
class(CBE)

Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

# The intersection between the air passenger data and the electricity
# data is obtained as follows:

AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)

AP.elec[1:3, ]
AP <- AP.elec[,1]; Elec <- AP.elec[,2]

layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")

plot(as.vector(AP), as.vector(Elec),
     xlab = "Air passengers / 1000's",
     ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))

cor(AP, Elec)

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/pounds_nz.dat'
Z <- read.table(www, header = T)
Z[1:4, ]
Z.ts <- ts(Z, st = 1991, fr = 4)

plot(Z.ts, xlab = "time / years",
     ylab = "Quarterly exchange rate in $NZ / pound")

Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96.98 <- window(Z.ts, start = c(1996, 1), end = c(1998, 1))

layout (1:2)
plot(Z.92.96, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )
plot(Z.96.98, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/global.dat'
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                  fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
# con aggregate te hace la media anual usando la fr=12 a través del argumento FUN=mean
plot(Global.ts)
plot(Global.annual)

# Acá tomo un pedazo de la serie con windows y con time creo el vector de tiempo
# en el que se muestreó la serie de tiempo.

New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series ~ New.time))

plot(decompose(Elec.ts))# La función decompose() descompone los datos, 
                        # estima tendencia y efectos estacionales
                        # usando el método de medias móviles.

Elec.decom <- decompose(Elec.ts, type = "mult")# en este caso el efecto es multiplicativo
plot(Elec.decom)

# la tendencia con el efecto estacional multiplicativo superpuesto
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)






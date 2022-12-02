# Basic Stochastic Models

# Simulation

set.seed(1)
w <- rnorm(100)# simulan 100 variables normales independientes, es equivalente
               # a simular serie de sonido blanco de largo 100
plot(w, type='l')

set.seed(1); w <- rnorm(100); plot(w, type='l')# Esto es lo mismo que lo anterior

var(w)

plot(rnorm(100), type='l')# es lo mismo que lo anterior pero al no tener la
                          # funcion set.seed() el arranque varía con diferentes
                          # datos simulados

# simular distribución normal

x <- seq(-3,3, length=1000)
hist(rnorm(100), prob=T); points(x, dnorm(x), type='l')

# Simulated white noise data will not have autocorrelations that are exactly
# zero (when k 6= 0) because of sampling variation.

set.seed(2); acf(rnorm(100))

# En el lag 7 se ve que la autocorrelación no es cero.

# 4.2.5 Fitting a white noise model

#A white noise series usually arises as a residual series after fitting
# an appropriate time series model. The correlogram generally provides
# sufficient evidence provided the series is of a reasonable length, to support
# the conjecture that the residuals are well approximated by white noise.

#The only parameter for a white noise series is the variance σ2, which is
# estimated by the residual variance, adjusted by degrees of freedom, given in
# the computer output of the fitted model. If your analysis begins on data that
# are already approximately white noise, then only σ2 needs to be estimated,
# which is readily achieved using the var function.


# Random walks

x <- w <- rnorm(1000)# This command places a white noise series into w and uses
                     # this series to initialise x
for (t in 2:1000) x[t] <- x[t-1] + w[t]# The ‘for’ loop generates the random walk
plot(x, type = 'l')

acf(x)# correlograma

acf(diff(x))# autocorrelción de la derivada primera de x

# El resultado del correlograma muestra que la variable x sigue una random walk.

# Usando otros datos, previos, del Cap. 1

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/pounds_nz.dat'
Z <- read.table(www, header = T)
Z[1:4, ]
Z.ts <- ts(Z, st = 1991, fr = 4)

acf(diff(Z.ts))# hay un valor significativo a lag=1, podría indicar que un
               # random walk sería una buena aproximación para la serie.

# Esto no lo entiendo bien: An additional term can be added to the random
# walk model using the Holt-Winters procedure, allowing the parameter β to
# be non-zero but still forcing the seasonal term γ to be zero:

Z.hw <- HoltWinters(Z.ts, alpha = 1, gamma = 0)
acf(resid(Z.hw))

# En teoría las barritas deberían caer dentro de la banda del 5%, eso indicaría
# que no habría correlación en los resíduos, la serie residuos sería un white noise,
# por lo que el modelo propuesto es bueno para la serie de datos. A mí no me da 
# esto no se por qué, quizás los datos no son exactamente los mismos.

Z.hw$alpha; Z.hw$beta


# Random walk with drift

# datos de empresa de ventas de acciones

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/HP.txt'
HP.dat <- read.table(www, header = T) ; attach(HP.dat)

# as.ts: Converts a Serie object to a Time-Series (ts) one.

plot (as.ts(Price))
DP <- diff(Price) ; plot (as.ts(DP)) ; acf (DP)

mean(DP)# The mean of the differences is 0.0399, and this is our estimate of
# the drift parameter.
sd(DP)# The standard deviation of the 671 differences is 0.460

# an approximate 95% confidence interval for the drift parameter is [0.004, 0.075]:
mean(DP) + c(-2, 2) * sd(DP)/sqrt(length(DP))

#Autoregresive models

# The random walk is the special case AR(1)

# Simulation of AR(1), autocorrelation and partial correlation

set.seed(2)
x <- w <- rnorm(1000)
for (t in 2:100) x[t] <- 0.7*x[t-1] + w[t]
plot(x, type = 'l')
acf(x)
pacf(x)

x.ar <- ar(x, method = "mle")
x.ar$order

x.ar$ar # me da el alpha
x.ar$ar + c(-2, 2) * sqrt(x.ar$asy.var)

# Los valores de órdenes varía y por lo tanto el número y valor de los coeficientes
# Esto si no consideramos la función set.seed()


# Exchange rate series: Fitted AR model

Z.ar <- ar(Z.ts)
mean(Z.ts)

Z.ar$order
Z.ar$ar

Z.ar$ar + c(-2, 2) * sqrt(Z.ar$asy.var)
acf(Z.ar$res[-1])

# In the code above, a “−1” is used in the vector of residuals to remove the
# first item from the residual series (Fig. 4.13). (For a fitted AR(1) model, the
# first item has no predicted value because there is no observation at t = 0; in
# general, the first p values will be ‘not available’ (NA) in the residual series of
# a fitted AR(p) model.)

# zˆt = 2.8 + 0.89(zt−1 − 2.8)# función de predicción

# 2.8 es: mean(Z.ts)
# 0.89 es: Z.ar$ar


# Global temperature series: Fitted AR model

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/global.dat'
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12),
                fr = 12)

Global.ar <- ar(aggregate(Global.ts, FUN = mean), method = "mle")
mean(aggregate(Global.ts, FUN = mean))
Global.ar$order
Global.ar$ar
acf(Global.ar$res[-(1:Global.ar$order)], lag = 50)

# El gráfico de autocorrelación indica que el modelo AR(4) funciona, entonces
# el modelo es:

# xˆt = −0.14 + 0.59(xt−1 + 0.14) + 0.013(xt−2 + 0.14)+0.11(xt−3 + 0.14) + 0.27(xt−4 + 0.14)

# Es el modelo de predicción.

plot(Global.ts)

# As the AR model has no deterministic trend component, the trends in
# the data can be explained by serial correlation and random variation, implying
# that it is possible that these trends are stochastic (or could arise from a purely
# stochastic process). 
# Again we emphasise that this does not imply that there is no underlying reason
# for the trends.If a valid scientific explanation is known,
# such as a link with the increased use of fossil fuels, then this information would
# clearly need to be included in any future forecasts of the series.













                                                     












































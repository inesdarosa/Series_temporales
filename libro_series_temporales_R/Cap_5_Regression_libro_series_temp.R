# Regression

# Simulations

# The model for the code can be expressed as xt = 50 + 3t + zt, where
# {zt} is the AR(1) process zt = 0.8zt−1 + wt and {wt} is Gaussian white noise
# with σ = 20.

set.seed(1)
z <- w <- rnorm(100, sd=20)
for (t in 2:100) z[t] <- z[t-1] + w[t]
Time <- 1:100
x <- 50 + 3*Time + z
plot(x, xlab = 'time', type = 'l')

# Linear models are usually fitted by minimising the sum of squared errors

x.lm <- lm(x~Time)
coef(x.lm)

# El resultado del intercepto es cercano a 50 y la pendiente a 3, más o menos,
# como debería de ser ya que el modelo fue propuesto con esos parámetros.

# The standard errors are  extracted using the square root of the diagonal elements
# obtained from vcov (value under Time), although these standard errors are likely
# to be underestimated because of autocorrelation in the residuals.

sqrt(diag(vcov(x.lm)))

summary(x.lm)#  t-tests showed may be incorrect for a time series regression analysis
             # due to autocorrelation in the residuals.

# In the case of time series regression, an important diagnostic plot is the
# correlogram of the residuals:

acf(resid(x.lm))# Se observa la autocorrelación de los residuos

pacf((resid(x.lm)))# 

# Model fitted to the temperature series (1970–2005)

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/global.dat'
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)

plot(Global.ts)

temp <- window(Global.ts, start = 1970)
temp.lm <- lm(temp ~ time(temp))
coef(temp.lm)
confint(temp.lm)

acf(resid(lm(temp~time(temp))))

# The confidence interval for the slope does not contain zero, which would provide
# statistical evidence of an increasing trend in global temperatures if the
# autocorrelation in the residuals is negligible. However, the residual series is
# positively autocorrelated at shorter lags, leading to an underestimate of the
# standard error and too narrow a confidence interval for the slope.


# GLS fit to simulated series

# For a positive serial correlation in the residual series, this implies that
# the standard errors of the estimated regression parameters are likely to be 
# underestimated (Equation (5.5)), and should therefore be corrected.

# vamos a usar la serie simulada anteriormente: x
# hay que llamar a una libreria

library(nlme)

x.gls <- gls(x ~ Time, cor = corAR1(0.8))# A lag 1 autocorrelation of 0.8 is used
                                         # above because this value was used to 
                                         # simulate the data.
coef(x.gls)
sqrt(diag(vcov(x.gls)))

# In the example above, the standard errors of the parameters are considerably 
# greater than those obtained from OLS using lm and are more accurate as they 
# take the autocorrelation into account.

# The parameter estimates from GLS will generally be slightly different from 
# those obtained with OLS, because of the weighting. For example, the slope is 
# estimated as 3.06 using lm but 3.04 using gls. In principle, the GLS estimators
# are preferable because they have smaller standard errors.


# Example: Seasonal model for the temperature series (temperature series (1970–2005):

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/global.dat'
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
temp <- window(Global.ts, start = 1970)

Seas <- cycle(temp)#  to seasonal indices extracted using the function cycle
Time <- time(temp)

temp.lm <- lm(temp ~ 0 + Time + factor(Seas))
coef(temp.lm)

library(nlme)# usando gls o ols da, en este caso, lo mismo
temp.gls <- gls(temp ~ 0 + Time + factor(Seas))
coef(temp.gls)

# A zero is used within the formula to ensure that the model does not have an
# intercept. If the intercept is included in the formula, one of the seasonal terms
# will be dropped and an estimate for the intercept will appear in the output.

# Using the above fitted model, a two-year-ahead future prediction for the
# temperature series is obtained as follows:

new.t <- seq(2006, len = 2 * 12, by = 1/12)

alpha <- coef(temp.lm)[1]
beta <- rep(coef(temp.lm)[2:13], 2)
(alpha * new.t + beta)[1:4]

# Alternatively, the predict function can be used to make forecasts provided
# the new data are correctly labelled within a data.frame:

new.dat <- data.frame(Time = new.t, Seas = rep(1:12, 2))
predict(temp.lm, new.dat)[1:24]

# Si no estoy entendiendo mal, genero los valores de los coeficientes del término
# estacional para las predicciones. Ese coeficiente (factor) lo uso en la fórmula para 
# el cálculo de la tempertura: temp ~ 0 + Time + factor(Seas)


# Harmonic seasonal models

TIME <- seq(1, 12, len = 1000)
plot(TIME, sin(2 * pi * TIME/12), type = "l")

plot(TIME, sin(2 * pi * TIME/12) + 0.2 * sin(2 * pi * 2 *
      TIME/12) + 0.1 * sin(2 * pi * 4 * TIME/12) + 0.1 *
      cos(2 * pi * 4 * TIME/12), type = "l")


#  Simulation

set.seed(1)
TIME <- 1:(10*12)
w <- rnorm(10*12, sd=0.5)

Trend <- 0.1 +0.005*TIME + 0.001*TIME^2
Seasonal <- sin(2*pi*TIME/12) + 0.2*sin(2*pi*2*TIME/12) +
  0.1*sin(2*pi*4*TIME/12) + 0.1*cos(2*pi*4*TIME/12)

x <- Trend + Seasonal + w
plot(x, type = 'l')

#################################
# Esto es aparte, solo para ver cómo afecta cada término en la serie
Seasonal.1 <- sin(2*pi*TIME/12)
x.1 <- Trend + Seasonal.1 + w
plot(x.1, type = 'l')

x.2 <- Trend + Seasonal.1
plot(x.2, type = 'l')
################################

# Fit to simulated series

SIN <- COS <- matrix(nr = length(TIME), nc = 6)

for (i in 1:6){
  COS[, i] <- cos(2 * pi * i * TIME/12)
  SIN[, i] <- sin(2 * pi * i * TIME/12)
}

x.lm1 <- lm(x ~ TIME + I(TIME^2) + COS[, 1] + SIN[, 1] +
              COS[, 2] + SIN[, 2] + COS[, 3] + SIN[, 3] + COS[, 4] +
              SIN[, 4] + COS[, 5] + SIN[, 5] + COS[, 6] + SIN[, 6])

coef(x.lm1)/sqrt(diag(vcov(x.lm1)))

# Para elegir con cuáles de los términos me quedo, por convención se dice que la
# significancia es cuando t-ratio es por lo menos valor 2. Ese valor se consigue
# dividiendo el coeficiente estimado dividido el error estandar del coeficiente
# que en este caso es: coef(x.lm1)/sqrt(diag(vcov(x.lm1)))

# Por lo arriba mencionado, me quedo con tres términos para construir el modelo:

x.lm2 <- lm(x ~ I(TIME^2) + SIN[, 1] + SIN[, 2])
coef(x.lm2)/sqrt(diag(vcov(x.lm2)))

# Los coeficientes del modelo son:
coef(x.lm2)

# El modelo de predicción es:

# xˆt = 0.280 + 0.00104t^2 + 0.900 sin(2πt/12) + 0.199 sin(4πt/12)

# The AIC can be used to compare the two fitted models:

AIC(x.lm1)
AIC(x.lm2)# Este tiene valor menor, entonces es el elegido

# Due to sampling variation, the best-fitting model is not
# identical to the model used to simulate the data, as can easily be verified by
# taking the AIC of the known underlying model:

AIC(lm(x ~ TIME +I(TIME^2) +SIN[,1] +SIN[,2] +SIN[,4] +COS[,4]))

step(x.lm1)# Esta función prueba todas las opciones y al final te tira el mejor
           # modelo, coincide con el que encontramos a pedal: x.lm2


# HARMONIC MODEL FITTED TO TEMPERATURE SERIES (1970–2005)

# The units for the ‘time’ variable are in ‘years’, so the divisor of 12 is not
# needed when creating the harmonic variables.

# To reduce computation error in the OLS procedure due to large numbers,
# the TIME variable is standardized after the COS and SIN predictors have been calculated.

SIN <- COS <- matrix(nr = length(temp), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * time(temp))
  SIN[, i] <- sin(2 * pi * i * time(temp))
}

TIME <- (time(temp) - mean(time(temp)))/sd(time(temp))
mean(time(temp))
sd(time(temp))

temp.lm1 <- lm(temp ~ TIME + I(TIME^2) +
                 COS[,1] + SIN[,1] + COS[,2] + SIN[,2] +
                 COS[,3] + SIN[,3] + COS[,4] + SIN[,4] +
                 COS[,5] + SIN[,5] + COS[,6] + SIN[,6])


coef(temp.lm1)/sqrt(diag(vcov(temp.lm1)))

temp.lm2 <- lm(temp ~ TIME + SIN[, 1] + SIN[, 2])
coef(temp.lm2)

AIC(temp.lm)
AIC(temp.lm1)
AIC(temp.lm2)

plot(time(temp), resid(temp.lm2), type = "l")
abline(0, 0, col = "red")
acf(resid(temp.lm2))
pacf(resid(temp.lm2))

# El plot de acf muestra que los residuos están autocorrelacionados. Mirando
# el pacf se sugiere AR(2) en los residuos.

res.ar <- ar(resid(temp.lm2), method = "mle")
res.ar$ar
sd(res.ar$res[-(1:2)])# es el desvío de los resíduos

acf(res.ar$res[-(1:2)])# estos residuos aparecen como white noise.


# The fitted model for the monthly temperature series can be written as:

# xt = 0.175 + (0.184(t − 1988))/10.4 + 0.0204 sin(2πt) + 0.0162 sin(4πt) + zt

# (0.184(t − 1988))/10.4 Esto no es solo t porque TIME fue estandarizado

# where t is ‘time’ measured in units of ‘years’, the residual series {zt} follow
# an AR(2) process given by:

  # zt = 0.494zt−1 + 0.307zt−2 + wt

# 0.494 y 0.307 vienen de res.ar$ar
# {wt} is white noise with mean zero and standard deviation 0.0837 (sd(res.ar$res[-(1:2)]))

# Importante: If we require an accurate assessment of the standard error, we
# should refit the model using gls, allowing for an AR(2) structure for the errors


# LOGARITHMIC TRANSFORMATIONS

data("AirPassengers")
AP <- AirPassengers

plot(AP)# aumenta la varianza con el tiempo.
plot(log(AP))# permite mantener la varianza menos variable en el tiempo.

# A harmonic model with polynomial trend is fitted to the air passenger series

# The function time is used to extract the time and create a standardised time variable TIME.

SIN <- COS <- matrix(nr = length(AP), nc = 6)
for (i in 1:6) {
  SIN[, i] <- sin(2 * pi * i * time(AP))
  COS[, i] <- cos(2 * pi * i * time(AP))
}

TIME <- (time(AP) - mean(time(AP)))/sd(time(AP))
mean(time(AP))
sd(time(AP))

AP.lm1 <- lm(log(AP) ~ TIME + I(TIME^2) + I(TIME^3) + I(TIME^4) +
               SIN[,1] + COS[,1] + SIN[,2] + COS[,2] + SIN[,3] + COS[,3] +
               SIN[,4] + COS[,4] + SIN[,5] + COS[,5] + SIN[,6] + COS[,6])

coef(AP.lm1)/sqrt(diag(vcov(AP.lm1)))

AP.lm2 <- lm(log(AP) ~ TIME + I(TIME^2) + SIN[,1] + COS[,1] +
               SIN[,2] + COS[,2] + SIN[,3] + SIN[,4] + COS[,4] + SIN[,5])

coef(AP.lm2)/sqrt(diag(vcov(AP.lm2)))
 
AIC(AP.lm1)
AIC(AP.lm2)

acf(resid(AP.lm2))
pacf(resid(AP.lm2))

#The residual correlogram indicates that the data are positively autocorrelated.
# As mentioned in §5.4, the standard errors of the parameter estimates are
# likely to be under-estimated if there is positive serial correlation
# in the data. This implies that predictor variables may falsely appear
# ‘significant’ in the fitted model. 

# GLS is used to check the significance of the variables in the fitted model,
# using the lag 1 autocorrelation (approximately 0.6).

library(nlme)
AP.gls <- gls(log(AP) ~ TIME + I(TIME^2) + SIN[,1] + COS[,1] +
                SIN[,2] + COS[,2] + SIN[,3] + SIN[,4] + COS[,4] + SIN[,5],
              cor = corAR1(0.6))

coef(AP.gls)/sqrt(diag(vcov(AP.gls)))

AP.ar <- ar(resid(AP.lm2), order = 1, method = "mle")
AP.ar$ar

acf(AP.ar$res[-1])# autocorrelación de los residuos que provienen del modelo de 
# autoregresion de los residuos del modelo principal. 

# The correlogram of the residuals of the fitted AR(1) model might be taken
# for white noise given that only one autocorrelation is significant.

# Sin embargo, se ve en el seasonal lag=12 un valor significativo, indicando un
# fallo para dar cuenta de la variación estacional de los datos, cosa rara ya que
# el modelo fue armado considerando un modelo armónico estacional. Esto se debe
# a que el efecto estacional puede ser estocástico como la tendencia, y nosotros
# usamos un modelo estacional determinista.

############################
# probemos con step()
step(AP.lm1)

AP.lm3 <- lm(log(AP) ~ TIME + I(TIME^2) + I(TIME^4) + SIN[, 1] + 
               COS[, 1] + SIN[, 2] + COS[, 2] + SIN[, 3] + COS[, 3] + SIN[, 4] +
               COS[, 4] + SIN[, 5])
AIC(AP.lm3)
# Con step() el modelo elegido tiene más términos
acf(resid(AP.lm3))# practicamente es igual a acf(resid(AP.lm2))


# con gls
AP.lm4 <- gls(log(AP) ~ TIME + I(TIME^2) + I(TIME^3) + I(TIME^4) +
               SIN[,1] + COS[,1] + SIN[,2] + COS[,2] + SIN[,3] + COS[,3] +
               SIN[,4] + COS[,4] + SIN[,5] + COS[,5] + SIN[,6] + COS[,6])

step(AP.lm4)# esto me da error, no se si porque no se puede hacer o hay que
# agregar algún parámetro.

############################

# Non-linear models

# Example of a simulated and fitted non-linear series
# Below, a non-linear series with AR(1) residuals is simulated and plotted

# In R, a non-linear model can be fitted by least squares using the function nls.

set.seed(1)
w <- rnorm(100, sd=10)
z <- rep(0, 100)
for (t in 2:100) z[t] <- 0.7 * z[t - 1] + w[t]
Time <- 1:100
f <- function(x) exp(1 + 0.05 * x)# xt = −c0 + e(α0+α1t) + zt, este es el modelo
                                  # usado para luego comparar los parámetros
                                  # estimados con nls.

x <- f(Time) + z
plot(x, type = "l")
abline(0, 0)

# El plot muestra valores negativos, so that a direct log-transformation
# cannot be used and a non-linear model is needed.

# In R, a non-linear model is fitted by specifying a formula with the parameters
# and their starting values contained in a list:

x.nls <- nls(x ~ exp(alp0 + alp1 * Time), start = list(alp0 = 0.1, alp1 = 0.5))
summary(x.nls)$parameters

# The estimates for α0 and α1 are close to the underlying values that were
# used to simulate the data, although the standard errors of these estimates are
# likely to be underestimated because of the autocorrelation in the residuals.


# Forecasting from regression

# In the code below, we use this function in the fitted regression model
# to forecast the number of air passengers travelling for the 10-year
# period that follows the record. The forecast is given by applying
# the exponential function (anti-log) to predict because the regression model
# was fitted to the logarithm of the series:

data("AirPassengers")
AP <- AirPassengers

new.t <- time(ts(start = 1961, end = c(1970, 12), fr = 12))
TIME <- (new.t - mean(time(AP)))/sd(time(AP))
SIN <- COS <- matrix(nr = length(new.t), nc = 6)

for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * new.t)
  SIN[, i] <- sin(2 * pi * i * new.t)
}

SIN <- SIN[, -6]
new.dat <- data.frame(TIME = as.vector(TIME), SIN = SIN, COS = COS)

AP.pred.ts <- exp(ts(predict(AP.lm2, new.dat), st = 1961, fr = 12))# AP.lm2 viene de antes

ts.plot(log(AP), log(AP.pred.ts), lty = 1:2)
ts.plot(AP, AP.pred.ts, lty = 1:2)


# Inverse transform and bias correction:
# La transformación logarítmica y luego la operiación para invertir los resultados
# para volverlos en sus valores a escalas orginiales generan sesgo en las medias.
# Ese sesgo será bajo si el ajuste es bueno. Si los residuos son una serie tipo
# White noise la corrección puede ser a través del cálculo de la media empírica
# o a través de una factor de corrección o usando la varianza calculada.

# Abajo se muestra esa corrección para el modelo anterior de los pasajeros.

summary(AP.lm2)$r.sq# R2

sigma <- summary(AP.lm2)$sigma# 0.048
lognorm.correction.factor <- exp((1/2) * sigma^2)# 1.001171
empirical.correction.factor <- mean(exp(resid(AP.lm2)))# 1.00108

AP.pred.ts <- AP.pred.ts * empirical.correction.factor# Predicción corregida





























































































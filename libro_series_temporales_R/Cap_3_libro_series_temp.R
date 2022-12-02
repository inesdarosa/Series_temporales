www <- 'https://raw.githubusercontent.com/AtefOuni/ts/master/Data/ApprovActiv.dat'

Build.dat <- read.table(www, header=T) ; attach(Build.dat)
App.ts <- ts(Approvals, start = c(1996,1), freq=4)
Act.ts <- ts(Activity, start = c(1996,1), freq=4)
ts.plot(App.ts, Act.ts, lty = c(1,3))# Building approvals (solid line) and 
                                     # building activity (dotted line).

# the building activity tends to lag one quarter behind the building approvals,
# or equivalently that the building approvals appear to lead the building activity by a quarter.
# The cross-correlation function,which is abbreviated to ccf, can be used 
# to quantify this relationship.

acf(ts.union(App.ts, Act.ts))# La función ts.union vincula series de tiempo con
                             # una frecuencia común

app.ran <- decompose(App.ts)$random
app.ran.ts <- window (app.ran, start = c(1996,3))
act.ran <- decompose (Act.ts)$random
act.ran.ts <- window (act.ran, start = c(1996,3))
acf(ts.union(app.ran.ts, act.ran.ts), na.action = na.pass)
ccf(app.ran.ts, act.ran.ts, na.action = na.pass)# muestra una relación retrasada

print(acf(ts.union(app.ran.ts, act.ran.ts), na.action = na.pass))
print(ccf(app.ran.ts, act.ran.ts, na.action = na.pass))


#Bass model, ver página 52, ajuste de curva relacionada con las ventas
#Ejemplo

T79 <- 1:10
Tdelt <- (1:100) / 10
Sales <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)# suma acumulada

Bass.nls <- nls(Sales ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /
                    (1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=60630, P=0.03, Q=0.38))

# nls: non-linear least squares function

summary(Bass.nls)

Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p+q) * Tdelt)
Bpdf <- m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from 1979",
                  ylab = "Sales per year", type='l')
points(T79, Sales)

Bcdf <- m * (1 - ngete)/(1 + (q/p)*ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979",
                  ylab = "Cumulative sales", type='l')
points(T79, Cusales)

# No logro entender cuándo predigo con este modelo?


# Complaints to a motoring organisation

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/motororg.dat'

Motor.dat <- read.table(www, header = T); attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), fr = 12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")

# There is no evidence of a systematic trend or seasonal effects, so it seems
# reasonable to use exponential smoothing for this time series. Exponential
# smoothing is a special case of the Holt-Winters algorithm

Comp.hw1 <- HoltWinters(complaints, beta = FALSE, gamma = FALSE); Comp.hw1
Comp.hw1$SSE
plot(Comp.hw1)

Comp.hw2 <- HoltWinters(complaints, alpha = 0.2, beta = FALSE, gamma = FALSE)
# acá le marco alpha=0.2
Comp.hw2


# Sales of Australian wine, results for the model with multiplicative seasonals only
# Acá se consideran presentes tendencias y estacionalidad.

www <- 'https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/master/wine.dat'

wine.dat <- read.table(www, header = T) ; attach (wine.dat)
sweetw.ts <- ts(sweetw, start = c(1980,1), freq = 12)
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)")
sweetw.hw <- HoltWinters (sweetw.ts, seasonal = "mult")
sweetw.hw ; sweetw.hw$coef ; sweetw.hw$SSE
sqrt(sweetw.hw$SSE/length(sweetw))
sd(sweetw)
plot (sweetw.hw$fitted)
plot (sweetw.hw)


# Four-year-ahead forecasts for the air passenger data

data(AirPassengers)
AP <- AirPassengers

AP.hw <- HoltWinters(AP, seasonal = "mult")
plot(AP.hw)

AP.predict <- predict(AP.hw, n.ahead = 4 * 12)# Esta está buena!!!
ts.plot(AP, AP.predict, lty = 1:2)

# The estimates of the model parameters:

AP.hw$alpha; AP.hw$beta; AP.hw$gamma



























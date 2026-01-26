
#Commodity Forward

#Costo de acarreo como porcentaje
s0 <- 1000
r <- 0.05 #tasa de plusvalia 
a <- 0.02 #costo de acarreo
t <- 90 #días
Ft.sim <- s0*(1+(r+a)*t/360)
Ft.com <- s0*(1+r+a)^(t/360)
Ft.cont <- s0*exp((r+a)*t/360)
cat(Ft.sim,Ft.com,Ft.cont)

#Costo de acarreo como cantidad monetaria
s0 <- 1000
r <- 0.05 #tasa de plusvalia 
a <- 100 #costo de acarreo
i <- 0.10 #tasa de interes
t <- 90 #días
Ft.sim <- s0*(1+(r)*t/360)+a*(1+i*t/360)
Ft.com <- s0*(1+r)^(t/360)+a*(1+i)^(t/360)
Ft.cont <- s0*exp((r)*t/360)+a*exp(i*t/360)
cat(Ft.sim,Ft.com,Ft.cont)

##Caso de estudio: Forward sobre el petroleo

#Librerias
install.packages('tseries')
library(tseries) #Modelos de series de tiempo
library(forecast) #predicción de modeos
library(nortest) #pruebas de bondad de ajuste
library(readxl) 
library(ggfortify) #gráficos series de tiempo

#Notas: Modelos de series de tiempo 
# AR -> MA -> ARIMA -> S-ARIMA -> ARCH -> GARCH

#Carga de datos
ruta <- "F:/07 RYSC/AxMexico/Mercado de Derivados
/ECR/Aplicaciones/A5 Commodity Forward.xlsx"
datos <- read_excel(ruta,sheet = "Petroleo")

#Paso 1. Convertir los datos a serie de tiempo
n <- length(datos$MM_USD) #punto final de la serie
m <- 121 #punto de partida de la serie
serie1 <- ts(datos$MM_USD[m:n],
             start = c(2015,1),frequency = 12)
autoplot(serie1)

#Paso 2. Comprobar Estacionariedad
#p valor > 0.05 la serie no es estacionaria
#p valor < 0.05 la serie es estacionaria

#Logaritmo 
serie1.Log <- log(serie1)
autoplot(serie1.Log)
adf.test(serie1.Log,alternative = "stationary")

#Diferencias
serie1.Dif1 <- diff(serie1, differences = 1)
autoplot(serie1.Dif1)
adf.test(serie1.Dif1,alternative = "stationary")

serie1.Dif2 <- diff(serie1, differences = 2)
autoplot(serie1.Dif2)
adf.test(serie1.Dif2,alternative = "stationary")

#Paso 3. Determinar los autoregresores y promedios moviles

par(mfrow=c(2,1))
acf(serie1.Dif1,lag.max = 50) #determinar parametros de MA
pacf(serie1.Dif1,lag.max = 50) #determinar parametros de AR

#Se propone:
#MA: q=1 y AR: p=1 o 2, Dif: I=1
#ARIMA(p,d,q)

#Paso 4. Propuesta del modelo
mod1 <- arima(serie1,order = c(1,1,1))
mod2 <- arima(serie1,order = c(2,1,1))
mod3 <- arima(serie1,order = c(1,1,0))

#Paso 5. Seleccion del modelo
#Selecionamos el modelo con el menor AIC
AIC(mod1,mod2,mod3)

#Paso 6. Validación del modelo 
# p-valor > 0.05: No se rechaza H0 no hay evidencia de autocorrelación, 
#los residuos parecen ruido blanco el modelo se ajusta bien.

# p-valor < 0.05: Se rechaza H0 hay autocorrelación significativa 
#en los residuo, el modelo no captura toda la estructura temporal.

Box.test(mod1$residuals,type = "Ljung-Box")
par(fmrow=c(1,1))
tsdiag(mod1)

#paso 7. Pronostico del modelo

pronostico <- forecast(mod1,h=6)
autoplot(pronostico)
plot(pronostico)

#Pronostico de diferentes modelos
mod4 <- auto.arima(serie1)
pronostico2 <- forecast(mod4,h=6)
autoplot(pronostico2)
plot(pronostico2)

# Propuesta de Precio Forward
Ft.ARIMA <- pronostico$mean[6]

# Precio teorico con tasa de plusvalia
datos$Rend <- NA
for(k in 2:n){
  datos$Rend[k] <- datos$MM_USD[k]/datos$MM_USD[k-1]-1
}
r <- mean(datos$Rend[m:n]) #tasa mensual
t <- 6
Ft.Teorico <- datos$MM_USD[n]*exp(r*t)

#Comparativo de modelos
cat(Ft.ARIMA,Ft.Teorico)



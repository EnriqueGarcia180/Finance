
# Equity Forward

# Pago dividendos de forma continua
s0 <- 60
acciones <- 100
r <- 0.05 #tasa de plusvalia
d <- 0.02 #tasa de dividendo
t <- 90 #dias
Ft.sim <- s0*acciones*(1+(r-d)*t/360)
Ft.com <- s0*acciones*(1+(r-d))^(t/360)
Ft.cont <- s0*acciones*exp((r-d)*t/360)
cat(Ft.sim,Ft.com,Ft.cont)

#Pago de dividendos de forma discreta
s0 <- 60
acciones <- 100
r <- 0.05 #tasa de plusvalia
d <- 1 #monto de dividendo
i <- 0.10 #tasa de interes
t <- 90 #dias
td <- 30 #periodo de pago del dividendo
Ft.sim <- s0*acciones*(1+(r)*t/360)-d*acciones*(1+i*(t-td)/360)
Ft.com <- s0*acciones*(1+(r))^(t/360)-d*acciones*(1+i)^((t-td)/360)
Ft.cont <- s0*acciones*exp((r)*t/360)-d*acciones*exp(i*(t-td)/360)
cat(Ft.sim,Ft.com,Ft.cont)

## Caso de estudio. Forward sobre precio de acciones
#Proceso estocástico de Weiner 

library(quantmod) #finanzas cuantitativas

cartera = c("AMZN")
getSymbols(cartera, src="yahoo", from="2024-01-01", to="2025-05-30") # ver Data window

d1 = data.frame(Precio=AMZN$AMZN.Close, Rend=NA) 
# la libreria entrega una Serie de tiempo, aqui lo convertimos a DataFrame.
# Tomamos el precio de Cierre no el Ajustado porque no incluye los eventos corporativos como pago de dividendos, etc.,
# el precio ajustado se ve al siguiente dia, cuando abre el mercado ya ajusta por los eventos corporativos,
# es basicamente para tener congruencia en todos los datos
colnames(d1) = c("Precio", "Rend")

# Análisis de la Serie
chartSeries(AMZN, type = "candlesticks", theme = chartTheme('white')) 
addSMA(n=10, on=1, col='blue')
addBBands(n=10, sd=2)

# Proyeccion del Activo Financiero con un proceso Estocastico de Norbert Weiner
n = length(d1$Precio)

for(k in 2:n){
  d1$Rend[k] = d1$Precio[k]/d1$Precio[k-1] - 1
}

# Supuesto de Normalidad para Procesos Estocasticos
plot(d1$Rend[2:n], type="l", col="blue")
abline(h=0, col="red")
abline(h=0.05, col="red") # intervalos de confianza al +/-5%
abline(h=-0.05, col="red")
# se muestra un comportamiento mas o menos estable, a exepcion de 6 puntos

# Histograma
hist(d1$Rend[2:n], breaks =  "Sturges")
hist(d1$Rend[2:n], breaks =  "Scott")
hist(d1$Rend[2:n], breaks =  "freedman-diaconis")

# Definir datos:
m = mean(d1$Rend[2:n])
s = sd(d1$Rend[2:n])
S0 = d1$Precio[n]
t = 1 # tiempo expresado en dias, hare prediccion diaria
esc = 1000 # numero de posibles escenarios
np = 100 # numero de dias de proyeccion en adelante

# Prediccion del Precio para distintas trayectorias
# Precio del activo para 100 dias, 1000 posibles escenarios.
stm = matrix(data=NA, nrow = np, ncol = esc) # [100 x 1000] stm = Matriz de precios S_t

# llenar la matriz stm
set.seed(600)
for(j in 1:esc){ # numero de escenarios
  for(k in 1:np){ # numero de dias de proyeccion
    if(k==1){
      stm[k,j] = S0
    }else{
      stm[k,j] = stm[k-1,j]+stm[k-1,j]*(m*t+s*sqrt(t)*rnorm(1,mean=0,sd=1)) # al final agregamos el 'Ruido Blanco' del proceso Estocastico
    }
  }
}

# Proyección del proceso estocástico del precio del activo financiero en los próximos 100 dias
plot(stm[,1], type = "l", col=1, ylim = c(min(stm), max(stm))) # para una trayectoria
for(k in 2:esc){
  lines(stm[,k], type = "l", col=k)
} # Ver la tendencia de la media si es a la baja o a la alta.

# Obtener el precio Teorico en el dia 100 en el futuro (n=100)

# Opcion1: Precio forward con el Proceso de Weiner
hist(stm[np,], breaks = "freedman-diaconis")  # histograma de los posibles precios en el dia 100
F0t_Weiner = mean(stm[np,]) # $228.06 usd. El precio pronosticado despues de 100 dias por el Modelo de Weiner

# Opcion 2: Precio teorico con tasa de plusvalia (un modelo mas sencillo)
r = mean(d1$Rend[2:n]) # obtener una tasa promedio de los historicos del rendimiento, tasa de Plusvalia
t = 100
F0t_Plusvalia = d1$Precio[n]*exp(r*t) # 229.600usd. El precio Forward (pronosticado) despues de 100 dias por el Modelo de Plusvalia



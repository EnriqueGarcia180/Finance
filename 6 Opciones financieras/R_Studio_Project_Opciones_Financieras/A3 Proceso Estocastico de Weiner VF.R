# Valuación de Opciones mediante del Proceso Estocástico de Weiner

#Bibliográfica: Riesgos financieros y económicos.pdf (page 36 sección 1.63)
#nota: es un método similar al aritmético de abajo, pero el del libro es geométrico

########################
# Call and Put functions
########################

# Payoff Long Call Function (Cuadrante 1)
payoffCall = function(S,K){
  if(S < K){
    p = 0 # p: payoff
  }else{
    p = S-K
  }
  return(p)
}

# Payoff Long Put Function (Cuadrante 2)
payoffPut = function(S,K){
  if(S < K){
    p = K-S
  }else{
    p = 0
  }
}

# Obtencion de datos
# install.packages('quantmod')
library(quantmod)
cartera = c("NVDA", "NFLX", "TSLA", "AAPL")
getSymbols(cartera, src="yahoo", from="2023-01-01", to="2025-08-27")

d1 = data.frame(Precio=NVDA$NVDA.Close, Ren=NA) #Ren: Rendimiento
colnames(d1)[1] = "Precio"

plot(d1$Precio, type = "l")
summary(d1$Precio)

# Rendimientos
n = length(d1$Precio)

for (j in 2:n){
  d1$Ren[j] = d1$Precio[j]/d1$Precio[j-1] - 1
}
plot(d1$Ren, type="l")
hist(d1$Ren, breaks = 30)

# Supuesto: los rendimientos tienen una densidad normal
m = mean(d1$Ren[2:n])
s = sd(d1$Ren[2:n])


#Modelo de Predicción de precios mediante Proceso de Weiner

#Modelo con crecimiento Aritmético (existe el método por crecimiento Geométrico)
s0 = d1$Precio[n] #ultimo precio
t = 1 #expresado en días, delta t
st = c()
n.dias = 100 #numero de días de predicción

for (j in 1:n.dias){
  if(j==1){
    st[j] = s0
  }else{
    st[j] = st[j-1]+st[j-1]*(m*t+s*sqrt(t)*rnorm(1,0,1)) # expresion del Proceso Estocastico Aritmetico
                              # m*t nos dice cuanto estara creciendo en promedio en el tiempo
                              # rnorm() es el ruido blanco
  }
}
plot(st, type='l', col="blue") # cada corrida da un escenario diferente por el rnorm()

# Matriz de trayectorias para un conjunto de escenarios
n.sim = 1000 # escenarios o numero de simulaciones
n.dias = 100 # dias de proyeccion
stm = matrix(data=NA, nrow = n.dias, ncol = n.sim) # 100 row x 1000 col

for (i in 1:n.sim){
  for (j in 1:n.dias){
    if(j==1){
      stm[j,i] = s0 #todo a partir del ultimo precio real conocido
    }else{
      stm[j,i] = stm[j-1,i]+stm[j-1,i]*(m*t+s*sqrt(t)*rnorm(1,0,1))
    }
  }
}
plot(stm[,1], type="l", col=1, ylim=c(min(stm), max(stm)))
for (j in 2:n.sim){
  lines(stm[,j], type="l", col=j)
}

############
#TO-DO:
# take last values and plot box-plot to get quantiles and percentiles
############




# Determinar las primas Call y Put
# se usan los resultados de la ultima corrida, es decir los ultimos precios calculados arriba

s0 = d1$Precio[n]
k = 200 #Strike price
r = 0.045 #risk free rate USA
tem = 100/252 #dias del contrato, temporalidad dado en fraccion de un anio

d2 = data.frame(St=stm[n.dias,], PC=NA, PP=NA, Prima.Call=NA, Prima.Put=NA) #St: Precio del subyacente
for(j in 1:n.sim){
  d2$PC[j] = payoffCall(d2$St[j],k) # estas ganancias potenciales estan valoradas en el dia 100
  d2$PP[j] = payoffPut(d2$St[j],k) 
  d2$Prima.Call[j] = d2$PC[j]*exp(-r*tem) # traer a valor presente
  d2$Prima.Put[j] = d2$PP[j]*exp(-r*tem)
}

hist(d2$Prima.Call, breaks=30)
p.call = mean(d2$Prima.Call) # Estimador. Promedio o Esperanza

hist(d2$Prima.Put, breaks=30)
p.put = mean(d2$Prima.Put)

p.call
p.put


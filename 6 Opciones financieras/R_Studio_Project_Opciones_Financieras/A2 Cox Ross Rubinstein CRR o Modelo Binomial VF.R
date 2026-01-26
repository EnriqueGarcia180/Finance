# Modelo Cox, Ross y Rubinstein (CRR) o Modelo Binomial
# Monte Carlo
# Opción real YahooFinance

# 1. Precio de la Opción en YahooFinance
# 2. Definir funciones Call y Put
# 3. Obtener datos del Stock (Return, Volatility)
# 4. Modelo CRR (binomial)
# 5. Simulaciones Monte Carlo
# 6. Comparación de Resultados de ambos métodos CRR (binomial) y Monte Carlo con la Opcion de YahooFinance



# 1. Precio de la opción en YahooFinance

# Option Chain
option_chain = getOptionChain(Symbols = "NVDA", Exp = NULL) # Exp = c("2025-07-10", "2025-12-29")
  # returns a nested list.

# Fechas de Opciones Disponibles (fechas caen en Viernes) 
dates = names(option_chain) # es el primer nivel de la lista option_chain
dates

# Datos
k = 170
exp_date = "Jul.18.2025"

# Calls
calls = option_chain[[exp_date]][["calls"]]
calls
strike_Row = which(calls$Strike == k)
opcion_call = calls[strike_Row,]
opcion_call
prima.yahooCall = opcion_call$Last
prima.yahooCall

# Puts
puts = option_chain[[exp_date]][["puts"]]
puts
strike_Row = which(puts$Strike == k)
opcion_put = puts[strike_Row,]
opcion_put
prima.yahooPut = opcion_put$Last
prima.yahooPut




# 2. Funciones Call y Put

# Payoff Long Call 

payoffCall = function(S,K){
  if(S < K){
    p = 0 # p: payoff
  }else{
    p = S-K
  }
  return(p)
}

# Payoff Long Put Function
payoffPut = function(S,K){
  if(S < K){
    p = K-S
  }else{
    p = 0
  }
}


# 3. Obtención de Datos de Stock (Return, Volatility)

library(quantmod)

fecha_inicial = "2023-01-01"
cartera = c("NVDA", "NFLX", "TSLA", "AAPL", "AMZN")

today = Sys.Date()
getSymbols(cartera, src = 'yahoo', from=fecha_inicial, to=today)

d1 = data.frame(Precio=NVDA$NVDA.Close, Ren=NA)  #Ren: Rendimiento
colnames(d1)[1] = "Precio"

plot(d1$Precio, type="l", col=4, main = "Historico de Precios del Asset", xlab = "Day number", ylab = "$ USD")
summary(d1$Precio)


# Return
n = length(d1$Precio)
for(j in 2:n){
  d1$Ren[j] = d1$Precio[j]/d1$Precio[j-1]-1
}
plot(d1$Ren[2:n], type="l", col=4, main = "Historico de Rendimientos diarios del Asset", xlab = "Day number", ylab = "Rendimiento")
hist(d1$Ren[2:n], breaks = 30, main = "Histograma de Rendimientos diarios del Asset", xlab = "Rendimiento")

# Volatility (sobre los Rendimientos del Activo)
vol.diaria = sd(d1$Ren[2:n]) # Volatilidad diaria
vol.a = vol.diaria*sqrt(252) # Volatilidad anual
# vol.a = opcion_call$IV




# 4. Modelo Cox, Ross y Rubinstein (CRR) o Modelo Binomial

# Datos iniciales
s0 = d1$Precio[n] # Spot price
vol.a # volatilidad anual
# Step (tiempo o periodo en anios):
exp.date = as.Date(exp_date, format = "%b.%d.%Y") #usando "exp_date" definida arriba
today = as.Date(Sys.Date())
days.toExp = as.numeric(difftime(exp.date, today, units = "days"))
days.toExp
t = days.toExp/360/2 # <- definir Periodo o pasos base ANUAL (Dividido entre 2 porque son dos pasos del boninomial tree)

# Risk-free Rate:
library(treasury)
yield_curve <- tr_yield_curve(2025) # https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value=2025
tail(yield_curve,14) # mostrar yield curve
len = length(yield_curve$date)
r = yield_curve$rate[len-7]/100  # Latest 1-year (Annual) rate

# up - down
u = exp(vol.a*sqrt(t))  # factor de incremento
d = exp(-vol.a*sqrt(t)) # factor de disminución
# p,  q
p = (exp(+r*t)-d)/(u-d) # risk neutral probability. probabilidad a la alza
q = 1-p                 # probabilidad a la baja

# Determinar las Primas
#(ver triangulo de pascal: pasos(n) y estados(n+1))
pasos = 2 #numero de ramas del modelo
m = pasos + 1 #numero de estados
d2 = data.frame(St=rep(NA,m), 
                PC=NA, # Payoff Call
                PP=NA, # Payoff Put
                Proba=NA, # Probabilidad de ocurrencia
                Prima.Call=NA, 
                Prima.Put=NA)

for(j in 1:m){
  d2$St[j] = s0*u^(m-j)*d^(j-1) # los precios para 2 pasos
  d2$PC[j] = payoffCall(d2$St[j], k)
  d2$PP[j] = payoffPut(d2$St[j], k)
  d2$Proba[j] = dbinom(j-1,pasos,q)
  d2$Prima.Call[j] = d2$PC[j]*d2$Proba[j]*exp(-r*t*2) #producto suma y traerlo a VP
  d2$Prima.Put[j] = d2$PP[j]*d2$Proba[j]*exp(-r*t*2) 
}
# usamos q porque:  tiene que ver con la forma en como construimos la tabla que empieza del valor más alto al más bajo
# en caso de ordenar los valores de estimación de st al reves, si hubiermos podido tomar como parámetro p
# si quieres puedes validar que el primer valor de que suba dos veces el subyacente es p*p
# ahí fue donde me di cuenta que nos quedo al reves

#Primas Call y Put por Metodo CRR
prima.call = sum(d2$Prima.Call)
prima.put = sum(d2$Prima.Put)



# 4. Modelo de Simulación Monte Carlo para calculo de Primas
n.sim = 100000 # no de simulaciones
st.call = c()
st.put = c()
prima.simcall = c()
prima.simput = c()
epsilon = 0.001 # valor de convergencia de la simulación monte Carlo con respecto al valor teórico 
diff_call = c() # para ir calculando la diferencia entre monte Carlo y Teórico
diff_put = c()

for(j in 1:n.sim){
  a <- runif(2,0,1) # returns two values min and max
  if(a[1]<p){#subio 
    if(a[2]<p){#subio (u-u)
      st.call[j] = d2$PC[1]*exp(-r*t*2) # ganancia potencial traída a VP para poder valorizarla en t=0
      st.put[j] = d2$PP[1]*exp(-r*t*2)
    }else{#bajo (u-d)
      st.call[j] = d2$PC[2]*exp(-r*t*2)
      st.put[j] = d2$PP[2]*exp(-r*t*2)
    }
  }else{#bajo
    if(a[2]<p){#subio (d-u)
      st.call[j] = d2$PC[2]*exp(-r*t*2)
      st.put[j] = d2$PP[2]*exp(-r*t*2)
    }else{#bajo (d-d)
      st.call[j] = d2$PC[3]*exp(-r*t*2)
      st.put[j] = d2$PP[3]*exp(-r*t*2)
    }
  }
  prima.simcall[j] = mean(st.call) # El valor de la Prima por simulaciones de Monte Carlo es el promedio de los valores calculados
  prima.simput[j] = mean(st.put)
  
  # Parar la simulación si la diferencia entre Valor Teórico y Monte Carlo es < epsilon, después de 10000 simulaciones al menos:
  diff_call[j] = prima.call - prima.simcall[j]
  diff_put[j] = prima.put - prima.simput[j]
  if (j > 10000){
    if (abs(diff_call[j]) < epsilon | abs(diff_put[j]) < epsilon){
      last.j = j
      break
    }else{}
  }else{}
  #
}
print(c("diff_call:",round(abs(diff_call[last.j]),5),"in",last.j,"simulations"))
print(c("diff_put:",round(abs(diff_put[last.j]),5),"in",last.j,"simulations"))






# 6. Comparar ambos métodos CRR (binomial) y Monte Carlo con la Opcion de YahooFinance

prima.call        #CRR
prima.simcall[last.j]  #Monte Carlo
prima.yahooCall # Yahoo
plot(prima.simcall[1:last.j], type = "l", col=4, main = "Valor de la Prima Call por simulaciones de Monte Carlo", xlab = "no. de iteraciones", ylab = "Precio Prima Call en $ USD") # plot
abline(h=prima.call, col="red") # visualizar el valor de la prima calculado con el modelo teórico Binomial como referencia para ver si converge el modelo de Monte Carlo, es decir la media de las n simulaciones cada vez se acerca el valor teorico
abline(h=prima.yahooCall, col='green') # visualizar el valor de la prima en YahooFinance como referencia para ver si converge el modelo de Monte Carlo.

prima.put
prima.simput[last.j]
prima.yahooPut
plot(prima.simput[1:last.j], type = "l", col=4, main ="Valor de la Prima Put por simulaciones de Monte Carlo", xlab = "no. de iteraciones", ylab = "Precio Prima Put en $ USD")
abline(h=prima.put, col="red") 
abline(h=prima.yahooPut, col='green') # visualizar el valor de la prima en YahooFinance como referencia para ver si converge el modelo de Monte Carlo.

# plot de la diferencia entre Monte Carlo y Teórico para ver como el error converge a cero
plot(diff_call[1:last.j], type = "l", col = 4, main = "Error entre Valor de Prima Call Teorico y Simulaciones Monte Carlo", xlab = "no. de iteraciones", ylab = "Error")
abline(h=0, col="red") 

plot(diff_put[1:last.j], type = "l", col = 4, main = "Error entre Valor de Prima Put Teorico y Simulaciones Monte Carlo", xlab = "no. de iteraciones", ylab = "Error")
abline(h=0, col="red")



print("Si el Valor teorico es mayor al del Mercado, esta undervalued. Comprar!")
print("Si el Valor teorico es menor al del Mercado, esta overvalued. Vender a opcion!")

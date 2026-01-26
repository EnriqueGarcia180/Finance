# Modelo de Black-Scholes

# 1. BS sin pago de dividendos
# 2. BS con pago de dividendos
# 3. BS Divisas o FX
# 4. BS Futuros o Forwards (BS.76)
# 5. Ejemplo con datos de Mercado




###########################################
# 1. BS sin pago de dividendos
###########################################

BS = function(S,K,vol,r,t,op){ #Datos anualizados. vol: volatilidad anual. op: opcion
  d1 = (log(S/K)+(r+(vol^2)/2)*t)/(vol*sqrt(t))
  d2 = d1-vol*sqrt(t)
  if(op==TRUE){ #call
    n1 = pnorm(d1,0,1)
    n2 = pnorm(d2,0,1)
    prima = S*n1-K*exp(-r*t)*n2
  }else{ #put
    n1 = pnorm(-d1,0,1)
    n2 = pnorm(-d2,0,1)
    prima = K*exp(-r*t)*n2-S*n1
  }
  return(prima)
}

# ejemplo
s = 30 #Precio del subyacente
k = 20
vol = 0.30
r = 0.06
t = 180/360

# Valuación
p.call = BS(s, k, vol, r, t, TRUE)
p.put = BS(s, k, vol, r, t, FALSE)

# Put-Call Parity (equivalencia)
p.call+k*exp(-r*t) # es comprar la opción call y un bono cupón cero traído a valor presente
p.put+s # es comprar la opción put y comprar el stock





###########################################
# 2. BS con pago de dividendos
###########################################

BS.Div = function(S,K,vol,r,q,t,op){ 
  d1 = (log(S/K)+(r-q+(vol^2)/2)*t)/(vol*sqrt(t))
  d2 = d1-vol*sqrt(t)
  if(op==TRUE){ #call
    n1 = pnorm(d1,0,1)
    n2 = pnorm(d2,0,1)
    prima = S*exp(-q*t)*n1-K*exp(-r*t)*n2
  }else{ #put
    n1 = pnorm(-d1,0,1)
    n2 = pnorm(-d2,0,1)
    prima = K*exp(-r*t)*n2-S*exp(-q*t)*n1
  }
  return(prima)
}
# ejemplo
s = 30 #Precio del subyacente
k = 20
vol = 0.30
r = 0.06
q = 0.02
t = 180/360

# Valuación
p.call = BS.Div(s, k, vol, r, q, t, TRUE)
p.put = BS.Div(s, k, vol, r, q, t, FALSE)

# Put-Call Parity (equivalencia)
p.call+k*exp(-r*t)
p.put+s*exp(-q*t)





###########################################
#  3. BS Divisas
###########################################

BS.FX <- function(S,K,vol,rl,rf,t,op){
  d1 <- (log(S/K)+(rl-rf+(vol^2)/2)*t)/(vol*sqrt(t))
  d2 <- d1-vol*sqrt(t)
  if(op==TRUE){ #call
    n1 <- pnorm(d1,0,1)
    n2 <- pnorm(d2,0,1)
    prima <- S*exp(-rf*t)*n1-K*exp(-rl*t)*n2
  }else{#put
    n1 <- pnorm(-d1,0,1)
    n2 <- pnorm(-d2,0,1)
    prima <- K*exp(-rl*t)*n2-S*exp(-rf*t)*n1
  }
  return(prima)
}
# ejemplo
s = 30 #Precio del subyacente
k = 20
vol = 0.30
rl = 0.06
rf = 0.02
t = 180/360

# Valuación
p.call = BS.FX(s, k, vol, rl, rf, t, TRUE)
p.put = BS.FX(s, k, vol, rl, rf, t, FALSE)

# Put-Call Parity (equivalencia)
p.call+k*exp(-rl*t)
p.put+s*exp(-rf*t)





##############################################
# 4. BS Futuros o Forwards (Black-Scholes 76)
##############################################

BS.76 = function(F0t,K,vol,r,t,op){ 
  d1 = (log(F0t/K)+((vol^2)/2)*t)/(vol*sqrt(t))
  d2 = d1-vol*sqrt(t)
  if(op==TRUE){ #call
    n1 = pnorm(d1,0,1)
    n2 = pnorm(d2,0,1)
    prima = F0t*exp(-r*t)*n1-K*exp(-r*t)*n2
  }else{ #put
    n1 = pnorm(-d1,0,1)
    n2 = pnorm(-d2,0,1)
    prima = K*exp(-r*t)*n2-F0t*exp(-r*t)*n1
  }
  return(prima)
}
# ejemplo
F0t = 30 #Precio del subyacente
k = 20
vol = 0.30
r = 0.06
t = 180/360

# Valuación
p.call = BS.76(F0t, k, vol, r, t, TRUE)
p.put = BS.76(F0t, k, vol, r, t, FALSE)

# Put-Call Parity (equivalencia)
p.call+k*exp(-r*t)
p.put+F0t*exp(-r*t)






###########################################
# 5. Ejemplo con datos de Mercado
###########################################

# Obtencion de datos
library(quantmod)
cartera = c("NVDA", "NFLX", "TSLA", "AAPL")
getSymbols(cartera, src="yahoo", from="2024-01-01", to="2025-07-15")

d1 = data.frame(Precio=AAPL$AAPL.Close, Ren=NA) #Ren: Rendimiento
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

# Supuesto: los renimientos tienen una densidad normal
m = mean(d1$Ren[2:n])
s = sd(d1$Ren[2:n])  # <- volatilidad historica usando todos los datos (diarios por anio y medio)
                     # también podemos usar EUMA con volatilidad dinámica, modelos gatch
                     # ventanas de 90 días también se recomienda
                     # La volatilidad implícita la calculan despejando de la formula
                     # o calcularla con solver para cada contrato (dia) y entonces tendremos diferentes
                     # valores de volatilidad por dia, l oque nos genera una curva de volatilidad
s.anual = s*sqrt(252)

# Valuación de Opcion Call y Put
# ejemplo
s = d1$Precio[n] #ultimo precio conocido de la accion
k = 240
vol = s.anual
r = 0.045
t = 100/360

# Valuación
p.call = BS(s, k, vol, r, t, TRUE)
p.put = BS(s, k, vol, r, t, FALSE)

# Put-Call Parity (equivalencia)
p.call+k*exp(-r*t) # es comprar la opción call y un bono cupón cero traído a valor presente
p.put+s # es comprar la opción put y comprar el stock





#### TO-DO
#### 1. Valuar una opción con todos los modelos.
#### Binomial converge al BS si dividimos infinitesimal.
#### proceso de Weiner.
#### 2. usar la funcion optim() como si fuera el solver para 
#### encontrar la Volatilidad Implicita del precio de la opcion
#### como en el archivo de excel.

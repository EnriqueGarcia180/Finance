# A6 Letras Griegas

#librerias
library(derivmkts)

# ####################################
# # Modelo de BS sin pago de dividendos
# ####################################
S = 30
K = 20
sigma = 0.30 #volatility
r = 0.05 #tasa libre de riesgo
tt = 180/360 #plazo expresado en ainos (252 dias si cotiza en bolsa)
d = 0 #tasa de dividendos

# Griegas:
# Call
greeks(bscall(S,K,sigma,r,tt,d))
# Put
greeks(bsput(S,K,sigma,r,tt,d))


# Analisis Grafico de las letras Griegas
s = seq(0.5,100, by=0.5) #variar el precio spot

#Call
call_griegas = greeks(bscall(s,K,sigma,r,tt,d))
for(j in row.names(call_griegas)){
  plot(s, call_griegas[j,], main = paste(j, "- Call"), sub = "Modelo de BS sin pago de dividendos", ylab=j, type = "l", col="blue")
}

#Put
put_griegas = greeks(bsput(s,K,sigma,r,tt,d))
for(j in row.names(put_griegas)){
  plot(s, put_griegas[j,], main = paste(j, "- Put"), sub = "Modelo de BS sin pago de dividendos", ylab=j, type = "l", col="red")
}






# ####################################
# Modelo de BS con pago de dividendos
# ####################################
S = 30
K = 20
sigma = 0.30 #volatility
r = 0.05 #tasa libre de riesgo
tt = 180/360 #plazo expresado en ainos (252 dias si cotiza en bolsa)
d = 0.03 #tasa de dividendos

# Griegas:
# Call
greeks(bscall(S,K,sigma,r,tt,d))
# Put
greeks(bsput(S,K,sigma,r,tt,d))


# Analisis Grafico de las letras Griegas
s = seq(0.5,100, by=0.5) #variar el precio spot

#Call
call_griegas = greeks(bscall(s,K,sigma,r,tt,d))
for(j in row.names(call_griegas)){
  plot(s, call_griegas[j,], main = paste(j, "- Call"), sub = "Modelo de BS con pago de dividendos", ylab=j, type = "l", col="blue")
}

#Put
put_griegas = greeks(bsput(s,K,sigma,r,tt,d))
for(j in row.names(put_griegas)){
  plot(s, put_griegas[j,], main = paste(j, "- Put"), sub = "Modelo de BS con pago de dividendos", ylab=j, type = "l", col="red")
}




# ####################################
# Modelo de BS para Divisas
# ####################################
S = 20
K = 22
sigma = 0.30 #volatility
rl = 0.05 #tasa local
rf = 0.03 #tasa foranea
tt = 180/360 #plazo expresado en ainos (252 dias si cotiza en bolsa)

# Griegas:
# Call
greeks(bscall(S,K,sigma,rl,tt,rf))
# Put
greeks(bsput(S,K,sigma,rl,tt,rf))


# Analisis Grafico de las letras Griegas
s = seq(0.5,100, by=0.5) #variar el precio spot

#Call
call_griegas = greeks(bscall(s,K,sigma,rl,tt,rf))
for(j in row.names(call_griegas)){
  plot(s, call_griegas[j,], main = paste(j, "- Call"), sub = "Modelo de BS para Divisas", ylab=j, type = "l", col="blue")
}

#Put
put_griegas = greeks(bsput(s,K,sigma,rl,tt,rf))
for(j in row.names(put_griegas)){
  plot(s, put_griegas[j,], main = paste(j, "- Put"), sub = "Modelo de BS para Divisas", ylab=j, type = "l", col="red")
}






# ####################################
# Modelo de BS-76 para Futuros
# ####################################
S = 20
K = 30
sigma = 0.30 #volatility
r = 0.05 #tasa libre de riesgo
tt = 180/360 #plazo expresado en ainos (252 días si cotiza en bolsa)

# Griegas:
# Call
greeks(bscall(S,K,sigma,r,tt,r))
# Put
greeks(bsput(S,K,sigma,r,tt,r))


# Análisis Grafico de las letras Griegas
s = seq(0.5,100, by=0.5) #variar el precio spot

#Call
call_griegas = greeks(bscall(s,K,sigma,r,tt,r))
for(j in row.names(call_griegas)){
  plot(s, call_griegas[j,], main = paste(j, "- Call"), sub = "Modelo de BS-76 para Futuros", ylab=j, type = "l", col="blue")
}

#Put
put_griegas = greeks(bsput(s,K,sigma,r,tt,r))
for(j in row.names(put_griegas)){
  plot(s, put_griegas[j,], main = paste(j, "- Put"), sub = "Modelo de BS-76 para Futuros", ylab=j, type = "l", col="red")
}





# ####################################
# Sabana de Griegas (individual plots) - CALL OPTION
# ####################################

K = 35
sigma = 0.30 #volatility
r = 0.05 #tasa libre de riesgo
d = 0.03
griegas = list()
for (g in 1:8){
  s = seq(0.5,100, by=0.5) #variar el precio spot
  t = seq(5, 365, by = 5) #variar el tiempo
  z = matrix(NA, length(s), length(t)) # Precio del contrato
  for (i in 1:length(s)){ #price change
    for (j in 1:length(t)){ # time change
      tt = t[j]/365 #time to maturity in years
      call_griegas = greeks(bscall(s[i],K,sigma,r,tt,d)) #same price multiple times
      z[i,j] = call_griegas[g]
      # Premium=1, Delta=2, Gamma=3, Vega=4, Rho=5, Theta=6, Psi=7, Elasticity=8
    }
  }
  griegas[g] = list(z) #guardar todos los resultados de todas las griegas
}
library(plotly)

# Create the surface plot
plot_ly(x = t, y = s, z = griegas[[1]], type = "surface") %>%
layout(title = "PREMIUM - CALL", scene = list(xaxis = list(title = "tiempo en dias"),
                      yaxis = list(title = "spot price"),
                      zaxis = list(title = "Premium")))

plot_ly(x = t, y = s, z = griegas[[2]], type = "surface") %>%
  layout(title = "DELTA - CALL",scene = list(xaxis = list(title = "tiempo en dias"),
                      yaxis = list(title = "spot price"),
                      zaxis = list(title = "Delta")))

plot_ly(x = t, y = s, z = griegas[[3]], type = "surface") %>%
  layout(title = "GAMMA - CALL",scene = list(xaxis = list(title = "tiempo en dias"),
                                      yaxis = list(title = "spot price"),
                                      zaxis = list(title = "Gamma")))

plot_ly(x = t, y = s, z = griegas[[4]], type = "surface") %>%
  layout(title = "VEGA - CALL",scene = list(xaxis = list(title = "tiempo en dias"),
                                      yaxis = list(title = "spot price"),
                                      zaxis = list(title = "Vega")))

plot_ly(x = t, y = s, z = griegas[[5]], type = "surface") %>%
  layout(title = "RHO - CALL",scene = list(xaxis = list(title = "tiempo en dias"),
                                      yaxis = list(title = "spot price"),
                                      zaxis = list(title = "Rho")))

plot_ly(x = t, y = s, z = griegas[[6]], type = "surface") %>%
  layout(title = "THETA - CALL",scene = list(xaxis = list(title = "tiempo en dias"),
                                      yaxis = list(title = "spot price"),
                                      zaxis = list(title = "Theta")))

plot_ly(x = t, y = s, z = griegas[[7]], type = "surface") %>%
  layout(title = "PSI",scene = list(xaxis = list(title = "tiempo en dias"),
                                      yaxis = list(title = "spot price"),
                                      zaxis = list(title = "Psi")))

plot_ly(x = t, y = s, z = griegas[[8]], type = "surface") %>%
  layout(title = "ELASTICITY",scene = list(xaxis = list(title = "tiempo en dias"),
                                      yaxis = list(title = "spot price"),
                                      zaxis = list(title = "Elasticity")))




# TO-DO: PUT OPTION



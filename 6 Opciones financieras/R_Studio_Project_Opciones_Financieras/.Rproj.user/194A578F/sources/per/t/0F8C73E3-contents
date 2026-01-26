# A5 Black Scholes con derivmkts

# 'derivmkts' que viene del libro "Derivatives Markets McDonald.pdf"

library(derivmkts)

# Modelo de BS sin pago de dividendos
S = 30
K = 20
sigma = 0.30 #volatility
r = 0.05 #tasa libre de riesgo
tt = 180/360 #plazo expresado en ainos (252 dias si cotiza en bolsa)
d = 0 #tasa de dividendos
bscall(S, K, sigma, r, tt, d)
bsput(S, K, sigma, r, tt, d)


# Modelo de BS con pago de dividendos
S = 30
K = 20
sigma = 0.30 #volatility
r = 0.05 
tt = 180/360 
d = 0.03 #tasa de dividendos
bscall(S, K, sigma, r, tt, d)
bsput(S, K, sigma, r, tt, d)



# Modelo de BS para Divisas
S = 20
K = 22
sigma = 0.30
rl = 0.05 #tasa local
rf = 0.03 #tasa foranea
tt = 180/360 
bscall(S, K, sigma, rl, tt, rf)
bsput(S, K, sigma, rl, tt, rf)



# Modelo de BS-76 para Futuros
F0 = 20
K = 22
sigma = 0.30
r = 0.05 #tasa libre de riesgo
tt = 180/360 
bscall(F0, K, sigma, r, tt, r)
bsput(F0, K, sigma, r, tt, r)



# Simulación del Precio de un activo S
S = 30
K = 20
sigma = 0.30 #volatility
r = 0.05 #tasa libre de riesgo
tt = 180/360 #plazo expresado en ainos (252 dias si cotiza en bolsa)
d = 0 #tasa de dividendos

# prediccion del precio mediante un modelo estocastico
st = simprice(S, sigma, r, tt, d, trials = 10000, periods = 1)

# Determinar la prima Call
# calcular la ganancia potencial para cada uno de los valores y luego traerla a Valor Presente
mean(pmax(st[st$period==1,]$price-K,0))*exp(-r*tt) #valor de la prima simulado
#precio teorico con BS
bscall(S, K, sigma, r, tt, d) 

# Determinar la prima Put
mean(pmax(K-st[st$period==1,]$price,0))*exp(-r*tt) #valor de la prima simulado
#precio teorico con BS
bsput(S, K, sigma, r, tt, d) #precio teorico con BS

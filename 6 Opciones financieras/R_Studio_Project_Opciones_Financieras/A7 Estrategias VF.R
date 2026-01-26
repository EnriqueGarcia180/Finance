
#Estrategias de Cobertura 

#Payoff functions
#(desde el punto de vista del Comprador)
payoffCall <- function(S,K){
  if(S<K){
    p <- 0
  }else{
    p <- S-K
  }
  return(p)
}

payoffPut <- function(S,K){
  if(S<K){
    p <- K-S
  }else{
    p <- 0
  }
  return(p)
}

payoffForward <- function(S,K){ #Lo podriamos interpretar como realizar la compra del subyacente
                                #el dia de hoy o realizar la compra de un bono cero con el valor
                                #del suyacente
  p <- S-K
  return(p)
}

#Datos:
t <- 5/12 #tiempo de vencimiento en meses (i.e. de julio a diciembre)
r <- 0.05 
s0 <- 330 #valor del subyacente
st <- seq(0,600,by=1) #vector de precios desde 0 a 600
n <- length(st)
F0t <- 330 #precio forward, lo que vale al dia de hoy el subyacente

#Strikes y Primas:
#(los strike se eligieron así para que quedaran simétricas las estrategias, 
#ver grafica "Long Combo" abajo)
#pc1 pago de la prima del contrato call
#pp1 pago de la prima del contrato put
#nota: la prima se paga en tiempo 0 y la ganancia se obtiene en tiempo t
#por eso las llevamos a valor futuro, para tener la ganancia en el mismo periodo
#de tiempo, todo evaluado en tiempo t y asi sea comparable con el Strike.
k1 <- 300 
pc1 <- 63.45*exp(r*t) #ITM
pp1 <- 28.60*exp(r*t) #OTM
k2 <- 315
pc2 <- 55.65*exp(r*t) #ITM
pp2 <- 35.49*exp(r*t) #OTM
k3 <- 330 #<- este es At the money (de acuerdo a los precios de ese dia) y tomo
          # dos dentro y dos fuera del dinero
pc3 <- 48.78*exp(r*t) #ATM
pp3 <- 43.30*exp(r*t) #ATM
k4 <- 345 
pc4 <- 42.50*exp(r*t) #OTM
pp4 <- 52.02*exp(r*t) #ITM
k5 <- 360 
pc5 <- 37.15*exp(r*t) #OTM
pp5 <- 61.05*exp(r*t) #ITM

# Siguiente paso es calcular la ganancia potencial de los contratos de arriba
# para todos los valores de precios propuestos de 0 a 600.


#Forward:
fl <- c() #Long (l: largo)
fc <- c() #Short (c: corto)
for(j in 1:n){
  fl[j] <- payoffForward(st[j],F0t)
  fc[j] <- -payoffForward(st[j],F0t)
}


# Call:
cl <- matrix(data=NA,nrow=n,ncol=5) #Long
cc <- matrix(data=NA,nrow=n,ncol=5) #Short

for(j in 1:n){
  cl[j,1] <- payoffCall(st[j],k1)-pc1 # -pc1 porque se paga la prima (long)
  cc[j,1] <- -payoffCall(st[j],k1)+pc1 # +pc1 porque se recibe la prima (short)
  cl[j,2] <- payoffCall(st[j],k2)-pc2
  cc[j,2] <- -payoffCall(st[j],k2)+pc2
  cl[j,3] <- payoffCall(st[j],k3)-pc3
  cc[j,3] <- -payoffCall(st[j],k3)+pc3
  cl[j,4] <- payoffCall(st[j],k4)-pc4
  cc[j,4] <- -payoffCall(st[j],k4)+pc4
  cl[j,5] <- payoffCall(st[j],k5)-pc5
  cc[j,5] <- -payoffCall(st[j],k5)+pc5
}

#Put:
pl <- matrix(data=NA,nrow=n,ncol=5) #Long
pc <- matrix(data=NA,nrow=n,ncol=5) #Short

for(j in 1:n){
  pl[j,1] <- payoffPut(st[j],k1)-pp1
  pc[j,1] <- -payoffPut(st[j],k1)+pp1
  pl[j,2] <- payoffPut(st[j],k2)-pp2
  pc[j,2] <- -payoffPut(st[j],k2)+pp2
  pl[j,3] <- payoffPut(st[j],k3)-pp3
  pc[j,3] <- -payoffPut(st[j],k3)+pp3
  pl[j,4] <- payoffPut(st[j],k4)-pp4
  pc[j,4] <- -payoffPut(st[j],k4)+pp4
  pl[j,5] <- payoffPut(st[j],k5)-pp5
  pc[j,5] <- -payoffPut(st[j],k5)+pp5
}
# Con lo anterior ya tenemos la ganancia potencial de los 5 contratos call
# y 5 contratos put de arriba variando los precios St de $0 a $600



# Siguente paso: hay que ver como quedan las Estrategias



#Bull - Mercado Alcista:

bull <- matrix(data=NA,nrow=n,ncol=9)
# Estrategias básicas: nos estarían dando la ganancia potencial al tener un
# incremento en el precio del subyacente
bull[,1] <- fl #Long Forward
bull[,2] <- cl[,1] #Long Call
bull[,3] <- pc[,1] #Short Put
# Posiciones sintéticas: van a replicar el valor de la posición 
bull[,4] <- fl+pl[,1] #Synthetic long call
bull[,5] <- fl+cc[,1] #Synthetic Short Put
# otras estrategias:
bull[,6] <- pc[,1]+cl[,5] #Long Combo
bull[,7] <- cl[,1]+cc[,2] #Bull Call spread
bull[,8] <- pl[,1]+pc[,2] #Bull Put spread
bull[,9] <- fl+pl[,1]+cc[,5] #Long Collar

#Grafica
par(mfrow=c(3,3))
for(j in 1:9){
  plot(x=st,y=bull[,j],type = "l",col="blue")
  abline(h=0,col="gray")  
}



#Bear - Mercado Bajista: 

#Las ganancias se generan con la disminución del precio del subyacente.
bear <- matrix(data=NA,nrow=n,ncol=9)
bear[,1] <- fc #Long Forward
bear[,2] <- cc[,1] #Short Call
bear[,3] <- pl[,1] #Long Put
bear[,4] <- fc+pc[,1] #Synthetic short call
bear[,5] <- fc+cl[,1] #Synthetic long Put
bear[,6] <- pl[,1]+cc[,5] #short Combo
bear[,7] <- cc[,1]+cl[,2] #Bear Call spread
bear[,8] <- pc[,1]+pl[,2] #Bear Put spread
bear[,9] <- fc+pc[,1]+cl[,5] #Short Collar

#Grafica
par(mfrow=c(3,3))
for(j in 1:9){
  plot(x=st,y=bear[,j],type = "l",col="blue")
  abline(h=0,col="gray")  
}




#Volatility: 

vol <- matrix(data=NA,nrow=n,ncol=8)
# Conos de volatilidad: nos generan una ganancia limitada en funcion de la variacion de los precios
vol[,1] <- cl[,1]+pl[,1] #Long Straddle : ganancias ante Alta volatilidad
vol[,2] <- cc[,1]+pc[,1] #short Straddle : ganancias ante Baja volatilidad
# estas 2 le quitan el pico al cono:
vol[,3] <- pl[,1]+cl[,2] #Long Strangle : ganancias ante Alta volatilidad
vol[,4] <- pc[,1]+cc[,2] #Short Strangle : ganancias ante Baja volatilidad
# Estrategia de Mariposa: se usa mas que los conos porque tienen ganancias y perdidas limitadas aunque en costos el cono es mas barato que la mariposa
vol[,5] <- cl[,1]+cc[,3]+cc[,3]+cl[,5] #Long Call Butterfly : ganancias ante Baja volatilidad
vol[,6] <- cc[,1]+cl[,3]+cl[,3]+cc[,5] #Short Call Butterfly : ganancias ante Alta volatilidad
# estas dos le quitan la punta a la mariposa
vol[,7] <- cl[,1]+cc[,2]+cc[,4]+cl[,5] #Long Call Condor : ganancias ante Baja volatilidad
vol[,8] <- cc[,1]+cl[,2]+cl[,4]+cc[,5] #Short Call Condor : ganancias ante Alta volatilidad

# TO-DO: agregarle a los Conos de Volatilidad las simulaciones (conos horizontales) para ver si
# cerramos o abrimos el cono y con ellos seleccionar los valores de strike del cono


#Grafica
par(mfrow=c(4,2))
for(j in 1:8){
  plot(x=st,y=vol[,j],type = "l",col="blue")
  abline(h=0,col="gray")  
}

# Mas estrategias en la Bibliografia:
# 1. 
# Cohen G. The Bible of Options Strategies (FTPH, 2005)(ISBN 0131710664)(401s)_FDtrd_
# i.e. Short call page 50
# ver el análisis de las griegas para cada estrategia
# 
# 2.
# Estrategias_derivados.pdf
# incluye las 22 estrategias mas utilizadas en un paper de la india




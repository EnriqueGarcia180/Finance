# Payoff plots

# Note:
# At the money: S = K (punto de equilibrio, es indiferente comprar en el mercado o ejercer la opcion)
# Call: in the money s>k, out the money s<k
# Put: in the money s<k, out the money s>k

library(gt) # para formato de tablas

########################
# Call and Put functions
########################

# Payoff Long Call Function (Quadrante 1)
payoffCall = function(S,K){
  if(S < K){
    p = 0 # p: payoff
  }else{
    p = S-K
  }
  return(p)
}

# Payoff Long Put Function (Quadrante 2)
payoffPut = function(S,K){
  if(S < K){
    p = K-S
  }else{
    p = 0
  }
}

#datos
s = seq(0,50, by=1) # precio del subyacente
k = 30              # Strike Price
pc = c()#payoff del contrato Call
pp = c()#payoff del contrato Pall
n = length(s)

# Llenar pc: payoff call and pp: payoff put
for(j in 1:n){
  pc[j] = payoffCall(s[j], k)
  pp[j] = payoffPut(s[j], k)
}

#Payoff plots
plot(pc, type="b", col="blue") # Long Call payoff plot (Q1)
plot(pp, type="b", col="blue") # Long Put payoff plot (Q2)



#################################
# P&L Diagrams (Payoff Diagrams)
#################################

# Crear "Option Chain" table
d1 = data.frame(Spot=s, 
                PC.Long=NA,  # PC: Payoff Call
                PC.Short=NA, 
                PP.Long=NA,  # PP: Payoff Put
                PP.Short=NA) 

# Llenar "Option Chain" table
for (j in 1:n){
  #Q1  Long Call
  d1$PC.Long[j] = payoffCall(d1$Spot[j],k) # En contrato Call el que esta en largo es el que compra y en corto el que vende la opcion
  #Q2  Long Put
  d1$PP.Long[j] = payoffPut(d1$Spot[j],k) # En contrato Put el que esta en largo es el que vende y en corto el que compra la opcion
  #Q3  Short Call
  d1$PC.Short[j] = -payoffCall(d1$Spot[j],k)
  #Q4  Short Put
  d1$PP.Short[j] = -payoffPut(d1$Spot[j],k)
}
# El resultado del for() deberia ser algo como Slide 7 de "16 Elementos de las Primas.pdf"
# o tambien como en: https://www.nasdaq.com/market-activity/stocks/aapl/option-chain

# Agregando colores al "Option Chain" Table:
gt_table <- gt(d1)
gt_table %>%
  data_color(
    #columns = everything(),
    columns = c("PC.Long", "PP.Long", "PC.Short", "PP.Short"),
    fn = function(x) { 
      ifelse(x >= 0, scales::col_numeric(palette = "#949494", domain = c(0, 0))(x), scales::col_numeric(palette = "#FEB7AA", domain = c(min(d1), max(d1)))(x))
    }
  )

# Plot P&L Diagrams
par(mfrow=c(2,2))
plot(d1$PC.Long,  type = "b", col="black", xlab = "Strike ($ USD)", ylab = "Payoff ($USD)", main = "Long Call") # Valor Intrinseco.  ~95% del valor del contrato
plot(d1$PP.Long,  type = "b", col="black", xlab = "Strike ($ USD)", ylab = "Payoff ($USD)", main = "Long Put") # EL Valor Tiempo (Valor Extrinseco) toma mayor importancia en el valor donde estamos "at the money" s=k, el precio del subyacente es el del precio strike
plot(d1$PC.Short, type = "b", col="black", xlab = "Strike ($ USD)", ylab = "Payoff ($USD)", main = "Short Call") # El valor de la prima. Corresponde a la ganancia potencial que tenemos del contrato
plot(d1$PP.Short, type = "b", col="black", xlab = "Strike ($ USD)", ylab = "Payoff ($USD)", main = "Short Put") # El valor de la prima. Corresponde a la ganancia potencial que tenemos del contrato

##### 
# TO-DO
# 2. Agregar lo de la Prima
#####





######################################
# Valor Intrinseco vs Valor Extrinseco
######################################

# Contrato Call (contrato de Compra, Long Call)
# (con las siguientes consideraciones iniciales)
Spot = 14
Strike = 5
Vol = 0.20 # Volatilidad
r = 0.045 # Risk Free Rate
t = 500 # Dias de Vencimiento
prima = 9.3058 # valor de la prima (VI + VE)
pc1 = Spot-Strike # Payoff Call. Valor Intriseco
# En este contrato de arriba el que Vende el subyacente ya esta perdiendo 14-5 = $9usd. Por eso 
# el valor de la prima es cerca a 9usd que es el payoff + el valor Extrinseco = $9.3058,
# prima = (VI + VE)
VI = Spot-Strike # Valor Intrinseco. ~95% del valor del contrato
VE = prima-VI # Valor Extrinseco


d2 <- data.frame(Strike=seq(1,24,by=1),
                 # los valores de las Primas se pueden obtener de las tablas de Option Chain i.e. Nasqad
                 Prima=c(13.0611553005607,12.1223106011214,11.1834659016829,10.2446212049275,9.30577707018633,8.36695578585603,7.42845587813854,6.49212147911554,5.56436733554767,4.65955315255009,3.80029480801186,3.01301834006207,2.32090407014313,1.7380642925389,1.26734372448543,0.901671164029958,0.627407627161539,0.428008016773418,0.286932863257157,0.189452594065106,0.123452867082304,0.0795403097724353,0.0507552371121447,0.0321236466374278),
                 VI=NA, # Valor Intrinseco
                 VT=NA, # Valor Tiempo. o Valor Extrinseco. La differencia de la Prima y el valor Intrinseco
                 VI.P=NA, # Valor Intrinseco en porcentaje
                 VT.P=NA) # Valor Tiempo (Valor Extrinseco) en porcentaje
n = length(d2$Strike)

for(j in 1:n){
  d2$VI[j] <- payoffCall(Spot,d2$Strike[j])
  d2$VT[j] <- d2$Prima[j]-d2$VI[j]
  d2$VI.P[j] <- d2$VI[j]/d2$Prima[j]
  d2$VT.P[j] <- d2$VT[j]/d2$Prima[j]
} 
# Al Leer la tabla d2 recordar que el Spot es $14 definido arriba
# Despues del renglon 14, el 100% de la prima es el Valor Extrinseco, es pura especulacion porque no hay Valor Intrinseco

par(mfrow=c(1,3))
plot(d2$VI,type = "b",col="blue", xlab = "Strike ($ USD)", ylab = "$Valor USD", main = "Valor Intrinseco +") # Valor Intrinseco.  ~95% del valor del contrato
plot(d2$VT,type = "b",col="green", xlab = "Strike ($ USD)", ylab = "$Valor USD", main = "Valor Extrinseco o Valor Tiempo") # EL Valor Tiempo (Valor Extrinseco) toma mayor importancia en el valor donde estamos "at the money" s=k, el precio del subyacente es el del precio strike
plot(d2$Prima,type = "b",col="red", xlab = "Strike ($ USD)", ylab = "$Valor USD", main = "= Prima") # El valor de la prima. Corresponde a la ganancia potencial que tenemos del contrato



# Siguientes Clases:
# Arboles binoimiales
# montecarlo
# 
# Proceso estocastico de Weiner
# crecimiento geometrico y aritmético
# 
# black & Scholes (Continuo)
# con pago de dividendos
# sin pago de dividendos
# futuros (black 76)
# divisas (carman collagen)
# 
# Cuanto vale la prima con los diferentes modelos

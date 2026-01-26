
#Tasa de equivalencia Swap

monto <- 10000 #USD
rl <- 0.08
rf <- 0.04
s <- 18.82
plazo <- 28 #intercambio cada 28 dias por 2 años

#Determinar Fx Swap
d1 <- data.frame(Plazo=seq(28,672,by=28),Fx.Swap=NA)
n <- length(d1$Plazo)

for(k in 1:n){
  d1$Fx.Swap[k] <- s*exp((rl-rf)*d1$Plazo[k]/360)
}

#Optimizacion
tc.ref <- 20
f1 <- function(tc.ref,data){
  #(monto*(tc.ref-Fx.Swap))^2
  sum((monto*(tc.ref-data$Fx.Swap))^2)
}

res1 <- optim(par=c(19),f1,data=d1)

#Equivalencia de la cobertura
tc.ref <- res1$par[1]
#tc.ref <- 19
d1$Monto.MXN <- NA
d1$Monto.Ref <- NA
d1$Resultado <- NA

for(k in 1:n){
  d1$Monto.MXN[k] <- d1$Fx.Swap[k]*monto
  d1$Monto.Ref[k] <- tc.ref*monto
  d1$Resultado[k] <- d1$Monto.Ref[k]-d1$Monto.MXN[k]
}
barplot(d1$Resultado,col="blue")

sum(d1$Resultado)





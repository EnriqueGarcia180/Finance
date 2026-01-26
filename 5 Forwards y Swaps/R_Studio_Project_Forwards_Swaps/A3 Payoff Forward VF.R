
# A3. Payoff Forward

Payoff <- function(st,Ft,op){  #op: 0-Long, 1-Short
  if(op==0){  
    p <- st-Ft # Long
  }else{      
    p <- Ft-st # short
  }
  return(p)
}

st <- seq(0,100,by=0.5)  #Precio del Subyacente
Ft <- 40 # precio pactado

Plong <- Payoff(st,Ft,0)  # Payoff Long
Pshort <- Payoff(st,Ft,1) # Payoff Short

plot(x=st,y=Plong,type = "l",col="blue", 
     xlab = "Spot Preice St", 
     ylab = "Payoff ($)", 
     main = "Payoff (P/L) Diagram Long vs Short")
lines(x=st,y=Pshort,type = "l",col="orange")
abline(v=Ft ,col="black")
abline(h=0  ,col="black")

# Se dice que el Vendedor tiene Ganancias Finitas y perdidas ilimitadas,
# el Comprador tiene Ganancias ilimitadas y perdidas finitas.
# Por lo tanto habrá un componente de riesgo diferente dependiendo si eres 
# comprador o vendedor y de como vaya evolucionando el precio del subyacente.




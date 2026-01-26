
#Precio Teorico Forward

s0 <- 2000
r <- 0.10 #tasa de plusvalia
t <- 60 #tiempo expresado en días
Ft.sim <- s0*(1+r*t/360) #interes simple
Ft.com <- s0*(1+r)^(t/360) #interes compuesto
Ft.cont <- s0*exp(r*t/360) #interes continuo
cat(Ft.sim,Ft.com,Ft.cont)

#Riesgo Base de los forward
s0 <- 20
r <- 0.30
t <- seq(1,90,by=1)
set.seed(600)
P.cont <- s0*(1+runif(90,0,1)/100)
Ft <- P.cont*exp(r*t/360)
r.base <- Ft -P.cont

plot(x=t,y=P.cont,type="b",col="blue",
     ylim = c(min(P.cont,Ft),max(P.cont,Ft)),
     main = "Riesgo Base")
lines(x=t,y=Ft,type="b",col="red")
abline(v=90,col="black")

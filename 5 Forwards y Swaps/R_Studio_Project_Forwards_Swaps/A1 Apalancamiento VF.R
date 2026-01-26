
#A1. Compras con Apalancamiento 

# Liquidas en tiempo t=T y recibes el activo en tiempo t0



#############################
# Sistema Frances 
#############################

# Producto o crédito hipotecario tradicional
# cuotas niveladas: el VP de los Pagos de las mensualidades = Préstamo
# L = P*a_n 
# despejando P tenemos: 
# P = L / a_n   
# a_n: anualidad n-periodos de tiempo

L <- 2000000 # Loan (prestamo) i.e. hipotecario
i <- 0.10/12 # tasa efectiva por periodo mensual 
n <- 20*12 # 20 anios en meses
pago <- L /((1-(1+i)^(-n))/i) # Pago mensual

# Tabla de Amortización:
ASF <- data.frame(
      Tiempo=seq(0,n,by=1),   # ASF: Amortización Sistema Frances
      Pago=NA,
      Interes=NA,
      Capital=NA,
      Balance=NA)

for(k in 0:n){
  if(k==0){
    ASF$Pago[k+1] <- 0
    ASF$Interes[k+1] <- 0
    ASF$Capital[k+1] <- 0
    ASF$Balance[k+1] <- L
  }else{
    ASF$Pago[k+1] <- pago # pago 'nivelado' para todos los periodos de tiempo
    ASF$Interes[k+1] <- ASF$Balance[k]*i # tasa efectiva mensual
    # nota: esta tabla es simplificada en el pago de intereses en forma mensual
    # pero los bancos calculan los intereses por dias exactos en cada mes.
    ASF$Capital[k+1] <- ASF$Pago[k+1]-ASF$Interes[k+1]
    ASF$Balance[k+1] <- ASF$Balance[k]-ASF$Capital[k+1]
  }
}
sum(ASF$Pago)
sum(ASF$Interes)
sum(ASF$Capital)




#############################
# Sistema Alemán
#############################

# Producto hipotecario de liquidez
# Cuotas a capital niveladas, es decir el préstamo es el numero de aportaciones
# a capital que se realicen.

# L = capital * n
# despejando capital:
# capital = L / n

L <- 2000000
i <- 0.10/12
n <- 20*12
capital <- L / n
ASA <- data.frame(   # ASA: Amortización Sistema Alemán
  Tiempo=seq(0,n,by=1),
  Pago=NA,
  Interes=NA,
  Capital=NA,
  Balance=NA)

for(k in 0:n){
  if(k==0){
    ASA$Pago[k+1] <- 0
    ASA$Interes[k+1] <- 0
    ASA$Capital[k+1] <- 0
    ASA$Balance[k+1] <- L
  }else{
    ASA$Capital[k+1] <- capital
    ASA$Interes[k+1] <- ASA$Balance[k]*i
    ASA$Pago[k+1] <- ASA$Capital[k+1]+ASA$Interes[k+1]
    ASA$Balance[k+1] <- ASA$Balance[k]-ASA$Capital[k+1]
  }
}

sum(ASA$Pago)
sum(ASA$Interes)
sum(ASA$Capital)

# Se pago menos interes con el sistema Alemán que con el Frances ($623,770.6)


#############################
# Sistema Americano
#############################

# Se pagan intereses y el capital al final.
# Para créditos de capital de trabajo, créditos de la cartera comercial a
# corto plazo.
# No es usual para créditos hipotecarios porque son a largo plazo.
# Créditos "Bullet" que son como un Bono Cupón cero.

L <- 2000000
i <- 0.10/12
n <- 20*12
ASEU <- data.frame(   # ASEU: Amortización Sistema Americano (Estados Unidos)
  Tiempo=seq(0,n,by=1),
  Pago=NA,
  Interes=NA,
  Capital=NA,
  Balance=NA)

for(k in 0:n){
  if(k==0){
    ASEU$Pago[k+1] <- 0
    ASEU$Interes[k+1] <- 0
    ASEU$Capital[k+1] <- 0
    ASEU$Balance[k+1] <- L
  }else{
    if(k==n){ # ultimo pago es diferente por que se paga el 100% del capital
      ASEU$Interes[k+1] <- ASEU$Balance[k]*i
      ASEU$Pago[k+1] <- ASEU$Interes[k+1]+L
      ASEU$Capital[k+1] <- ASEU$Pago[k+1]-ASEU$Interes[k+1]
      ASEU$Balance[k+1] <- ASEU$Balance[k]-ASEU$Capital[k+1]
    }else{
      ASEU$Interes[k+1] <- ASEU$Balance[k]*i
      ASEU$Pago[k+1] <- ASEU$Interes[k+1]
      ASEU$Capital[k+1] <- ASEU$Pago[k+1]-ASEU$Interes[k+1]
      ASEU$Balance[k+1] <- ASEU$Balance[k]-ASEU$Capital[k+1]
    }
  }
}

sum(ASEU$Pago)
sum(ASEU$Interes)
sum(ASEU$Capital)

# Con este sistema se pagarían muchísimos Intereses ($6,000,000)

# Habria que agregar:
#   Costo Administrativo (costo por apertura)
#   Costo de seguro de vida del que paga, 
#   costo de seguro de danios, 
#   IVA a los intereses.
#   La suma de lo anterior nos daria la CAT (tasa anual total)
#   i.e. del 10% se iría al 11.5% aprox. la tasa real



# Gráfico que muestra la disminución del Balance en el tiempo (meses)
plot(x=ASA$Tiempo,y=ASA$Balance,type="l", col="black",   # Alemán
     main ="Balance disminuye en el tiempo",
     xlab = "Tiempo (meses)", ylab = "Balance ($)")
lines(x=ASF$Tiempo,y=ASF$Balance,type="l",col="red")     # Frances
lines(x=ASEU$Tiempo,y=ASEU$Balance,type="l",col="blue")  # Americano

# Hace sentido el gráfico porque en el Alemán los pagos a capital son Constantes,
# en el Frances los pagos a capital van aumentando con el tiempo 
# en el Americano el balance se finiquita al final.



####
# Forward de Tasa de Interes
####


# Forward de Tasa de Interes

# 1. Calculo de Forward rate entre dos periodos i.e. t1 y t2:
#   a. Con Interes Compuesto
#   b. Con Interes Continuo

#Curva de tasa de Interes (source: Investing MEXICO bonds)
CurvaY <- data.frame(Dias=c(28,91,180,360,720,1080,1800,3600,7200,10800,18000),
                     Tasa=c(6,6.5,7,7.5,8,8.25,8.5,9,10,10.5,11))

# a. TASA FORWARD - INTERES COMPUESTO
x = c(720, 1080) # para calcular la forward Rate entre estos dos periodos
y = c(CurvaY$Tasa[5]/100, CurvaY$Tasa[6]/100) # las tasas de interes a 720 y 1080 dias respectivamente
F_t1_t2_compuesto = (((1+y[2])^(x[2]/360) / (1+y[1])^(x[1]/360)) ^ (360 / (x[2]-x[1]))) - 1 # Tasa Forward de interes efectiva entre t1 y t2. Inversion en el largo plazo y2 / Inversion en el corto plazo y1

  # comprobacion
(1+y[2])^(x[2]/360) # Inversion en t0 con tasa de interes de largo plazo por t2 periodos (x[2])
(1+y[1])^(x[1]/360)*(1+F_t1_t2_compuesto)^((x[2]-x[1])/360) # Inversion en t0 a la tasa de interes de corto plazo * Inversion en t0 a la tasa Forward de los periodos 1 y 2 (x[2]-x[1])
# dan el mismo resultado!

# b. TASA FORWARD - INTERES CONTINUO O FUERZA DE INTERES
x = c(720, 1080)    # para calcular la forward Rate entre estos dos periodos
y = c(CurvaY$Tasa[5]/100, CurvaY$Tasa[6]/100) # las tasas de interes a 720 y 1080 dias respectivamente
F_t1_t2_continuo = (y[2]*x[2]/360 - y[1]*x[1]/360) * (360/(x[2]-x[1]))

  # comprobacion
exp(y[2]*x[2]/360)
exp(y[1]*x[1]/360)*exp(F_t1_t2_continuo*(x[2]-x[1])/360)
# dan el mismo resultado!

# 2. Construccion de la Curva de Tasas Forward con ambos, interes compuesto y continuo
n = length(CurvaY$Dias)
CurvaY$F_Compuesto = NA
CurvaY$F_Continuo = NA

for(k in 1:n){
  if(k==1){
    CurvaY$F_Compuesto[k] <- CurvaY$Tasa[k]
    CurvaY$F_Continuo[k] <- CurvaY$Tasa[k]
  }else{
    CurvaY$F_Compuesto[k] <- ((1+CurvaY$Tasa[k])^(CurvaY$Dias[k]/360)/(1+CurvaY$Tasa[k-1])^(CurvaY$Dias[k-1]/360))^(360/(CurvaY$Dias[k]-CurvaY$Dias[k-1]))-1
    CurvaY$F_Continuo[k] <- (CurvaY$Tasa[k]*CurvaY$Dias[k]/360-CurvaY$Tasa[k-1]*CurvaY$Dias[k-1]/360)*(360/(CurvaY$Dias[k]-CurvaY$Dias[k-1]))
  }
}

plot(x=CurvaY$Dias,y=CurvaY$Tasa,type="b",col="black",ylim = c(5,12)) # Curva de Tasas de Interes Mexico Bonds (original)
lines(x=CurvaY$Dias,y=CurvaY$F_Compuesto,type = "b",col="blue") # Curva de Tasas Forward con interes Compuesto
lines(x=CurvaY$Dias,y=CurvaY$F_Continuo,type = "b",col="red") # Curva de Tasas Forward con interes Continuo











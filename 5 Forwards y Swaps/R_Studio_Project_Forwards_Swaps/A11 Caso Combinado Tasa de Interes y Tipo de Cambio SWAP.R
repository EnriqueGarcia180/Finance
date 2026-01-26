# Caso Combinado, caso general: Tasa de Interes y Tipo de Cambio

# Intercambio de flujos de efectivo con tasa fija y tasa variable y con moneda nacional y moneda extranjera.


# SWAP

# Curva de tasas de interes:
Curva <- data.frame(Dias=c(28,56,84,112), # Intercambio de Flujos de Efectivo cada n dias.
                    Y.USD=c(0.0120,0.0130,0.0135,0.0138), # Yield en dolares.
                    Y.MXN=c(0.0490,0.0485,0.0480,0.0470), # Yield en pesos
                    Y.FRAUSD=NA, # Vamos a construir estas curvas Forward o FRA. 
                    Y.FRAMXN=NA) # esto implica

# Hay mucho trabajo preliminar antes de determinar el valor del derivado, tenemos que adecuar los insumos
# de mercado antes de hacer la Valuacion de los Flujos de Efectivo. Esto implica poder determinar los
# tipos de cambio futuros (FX swap) y poder calcular las estructuras de tasas FRA (forward).
# Con respecto a las curvas el preimer paso seria calcular las tasas forward (FRA), luego los FX Swaps.

# Insumos
TC <- 19.6851 # tipo de cambio USD/MXN
Noc.USD <- 100000 # nocional al tiempo 0
Noc.MXN <- Noc.USD*TC # nocional al tiempo 0
plazo <- 28 # plazo del intercambio de flujos de efectivo expresado en dias
n <- 4 # cuatro flujos de efectivo

#Construccion de los insumos de mercado: Tasas por periodo [FRA] y tipos de cambio futuros [FX Swaps]

# Tasas FRA
for(k in 1:n){
  if(k==1){ # El primer nodo queda igual, el de la curva yield y el forward, el interes ganado del tiempo t=0 al t=1, o sea de 28 a 56 dias.
    Curva$Y.FRAUSD[k] = Curva$Y.USD[k]
    Curva$Y.FRAMXN[k] = Curva$Y.MXN[k]
  }else{ # (k>1), A partir del nodo de 56 dias, vamos a generar el calculo de la tasa de interes efectiva por 28 dias. Usando Formula slide 4 de '09 Forward Rate Agreement FRA.pfg'
    # nota: se divide entre 360 para obtener la tasa efectiva por dia.
    Curva$Y.FRAUSD[k] = ((1+Curva$Y.USD[k]*Curva$Dias[k]/360)/(1+Curva$Y.USD[k-1]*Curva$Dias[k-1]/360)-1)*(360/(Curva$Dias[k]-Curva$Dias[k-1]))
    Curva$Y.FRAMXN[k] = ((1+Curva$Y.MXN[k]*Curva$Dias[k]/360)/(1+Curva$Y.MXN[k-1]*Curva$Dias[k-1]/360)-1)*(360/(Curva$Dias[k]-Curva$Dias[k-1]))
  }
}


# Fx Swaps (Tipos de Cambio futuros)
 
# Pronostico de los tipos de cambio. Valor Futuro de los tipos de cambio para cada uno de los dias de valuacion , dependen de la curva yield.
Fx.Swap = data.frame(Dias=seq(28,112,by=28),
                      Fx.Swap=NA)
for(k in 1:4){
  Fx.Swap$Fx.Swap[k] = TC*(1+Curva$Y.MXN[k]*Curva$Dias[k]/360)/(1+Curva$Y.USD[k]*Curva$Dias[k]/360) # Multiplicar el tipo de cambio Spot t_0 (TC) por la inversion en moneda nacional entre la inversion en moneda extranjera
}




# Valuacion de SWAP por esquema de Flujos de Efectivo 

d1 <- data.frame(Dias=seq(28,112,by=28),
                 Recibe.USD=NA, # La cantidad que Recibimos en USD
                 Paga.MXN=NA, # La cantidad de la contraparte va a pagar
                 Recibe.MXN=NA, # La cantidad que recibimos en MXN
                 Neto=NA, # Neto de FLujo de Efectivo
                 VP=NA) # VP del Neto por periodo.

for(k in 1:n){
  d1$Recibe.USD[k] = Noc.USD*Curva$Y.FRAUSD[k]*28/360 # Positivo (+) porque Recibimos el dinero. Tasa FRA en dolares que es la tasa cupon efectiva por cada periodo de 28 dias.
  d1$Paga.MXN[k] = -Noc.MXN*Curva$Y.FRAMXN[k]*28/360 # Pagamos esta cantidad en MXN usando el nocional en MXN y la tasa FRA en MXN
  d1$Recibe.MXN[k] = d1$Recibe.USD[k]*Fx.Swap$Fx.Swap[k] # Recibimos en MXN, se hace la conversion de tipos de cambio. L oque recibimos en USD lo multiplicamos por el tipo de cambio correspondiente para ese periodo (FX Swap del periodo)
  d1$Neto[k] = d1$Paga.MXN[k]+d1$Recibe.MXN[k]
  d1$VP[k] <- d1$Neto[k]/(1+Curva$Y.MXN[k]*d1$Dias[k]/360) # Traer a Valor Presente con la Yield (ojo no con la FRA) de los pesos porque lo queremos a VP (t_0)
}

# Explicacion de la tabla de arriba 'd1':
# Recibe.USD: Es una cantidad que se estara pagando en USD considerando el Nocional en USD y la tasa FRA que se determino de acuerdo a la estructura de las tasas de interes Yield Rate.
# Paga.MXN: Es lo mismo que arriba pero en MXN
# Recibe.MXN: Cuanto recibimos en MXN
# Neto: La diferencia entre lo que pagamos en MXN y recibimos en MXN
# VP: Lo traemos cada neto a VP
 
# Valoracion del Derivado
swap <- sum(d1$VP)
# Cuanto dinero gane o perdi al momento de intercambiar este flujo de efectivo de moneda extranjera.

# Puntos Swap
puntos.swap = TC - Fx.Swap$Fx.Swap[4]
# Diferencia entre el tipo de cambio Spot y el tipo de cambio futuro a una fecha de valuacion, en este caso de la ultima valuacion (al dia 112), 
# es la cantidad de ganancia o perdida (en puntos base) que tuve al momento de hacer la valuacion del derivado considerando el Nocional del tipo de cambio.
# Su unidad sigue siendo USD/MXN


# Si 'compramos' el Derivado pagamos en la Divisa, Si 'vendemos' el Derivado recibimos en la Divisa.


#Comprobación
swap_comp = Noc.USD*puntos.swap / (1+Curva$Y.MXN[4]*Curva$Dias[4]/360) 
# Al multiplicar el Nocional x puntos.swap el Valor de Ganancia o Perdida queda ya en MXN
# treaer al VP usando la tasa yield en MXN de 112 dias.

swap
swap_comp

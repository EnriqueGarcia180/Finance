# SWAP para obtener una tasa equivalente para equilibrar ambas posiciones,
# para no empezar el SWAP con una posición desequilibrada y alguno de los 
# participantes tenga que pagar un costo extra. Se busca un punto de equilibro
# para que ninguna de las dos partes tenga un costo al inicio del contrato. 

# Tasa de Equivalencia SWAP

# Funciona para tipo de cambio (CCS) o para tasa de interes (IRS), donde hay una parte variable y una fija. 
# Se obtiene un valor de tasa fija con el cual el Valor Presente de los Flujos de efectivo sea equivalente (cero).

# Ejercicio: Estructurar un CCS con Intercambio de flujos de efectivo cada 28 días por 2 anios.
monto = 10000 # (USD) Monto del intercambio
tasa_local = 0.08 # Mexico la tasa de interes
tasa_foranea = 0.04 # tasa USA
s = 18.82 # Precio Spot USD/MXN
plazo = 28  # intercambio cada 28 días por 2 anios


# FX Swap (Tipos de Cambio)
#
# Determinar Fx_Swap para cada periodo de tiempo para saber a que Tipo de Cambio estimamos que se van a estar considerando estos Intercambios.
# Lo denominan contrato engrapado.

d1 = data.frame(Plazo=seq(28,672, by=28),
                Fx_Swap=NA)
n = length(d1$Plazo)

# Calcular el valor del FX Swap (proyección de los tipos de cambio)
for (k in 1:n){ # Es como si hiciéramos 24 contratos Forward donde vamos a determinar para cada periodo de tiempo el intercambio de Flujo de efectivo usando el tipo de cambio FX Swap correspondiente. Contratos "Engrapados"
  d1$Fx_Swap[k] = s*exp((tasa_local-tasa_foranea)*d1$Plazo[k]/360) # Interes Continuo. El objetivo es llevar el tipo de cambio a Valor Futuro.
} 

# El objetivo es buscar un Tipo de Cambio al cual en lugar de estar variando el intercambio de flujos de efectivo por los diferentes FX Swap (tipos de cambio)
# queremos que sea un tipo de cambio Fijo para ambas partes, tanto para el que paga en fijo como el que paga en variable.


# Optimizacion
tc_referecia = 20 # tipo de cambio de referencia

# Funcion para minimos cuadrados
f1 <- function(tc_referecia, data){ # data es lo mismo que d1 pero pasado como argumento, por eso el cambio de nombre para evitar confusiones.
  # Lo que buscamos con el optimizer abajo es que la suma de los errores cuadráticos medios sea cero: monto*(tc_referencia - FXSwap)^2 = 0
  # vamos a variar "tc_referecia" para lograr que el resultado de a diferencia sea cero. 
  sum((monto*(tc_referecia-data$Fx_Swap))^2)
}

# optim()
resultado1 = optim(par=c(19), f1, data=d1) 
# par, es el valor Inicial de tc_referecia que va a ir variando, es decir la optimizacion empieza con tc_referecia = 19
# f1, es la funcion que va a estar optimizando
# data, de donde va a estar tomando la informacion
resultado1$par[1]
# es la tasa que se usaría en los 24 periodos, tasa equivalente!

# Ver como cambiando el tc_referecia (cambiando el tipo de cambio inicial par=c()) los flujos de efectivo seran diferentes
# en el barplot abajo. Es decir en lugar de usar la FxSwap tipo de cambio Optimizado usar un tipo de cambio elegido aleatoriamente.


#Equivalencia de la cobertura

#tc_referecia = 20
#tc_referecia = 19
tc_referecia <- resultado1$par[1]  # Ver como cambia la grafica si aqui le ponemos un valor como 19 o 20, es decir si se elige un tipo de cambio aleatorio alguien podria pagar mas o menos al inicio
# lo mejor es usar la tasa equivalente u optimizada, asi el punto de infleccion seria 'a la mitad' por ejemplo con tc_referecia <- 19.57079,
# pero si usamos tc_referecia = 20 la grafica se mueve a la derecha, el contrato no estaria en equilibrio, la persona estaria pagando de mas los primeros periodos
# y unicamente tendria un periodo de recuperacion en los ultimos flujos de efectivo, en este caso el Valor Presente del SWAP no quedara equilibrado y
# alguna de las contrapartes tendra que pagar un valor inicial en t=0 para compenzar a la contraparte que tendra la perdida pues estaria pagando de mas.
d1$Monto.MXN <- NA # Monto de transacción en MXN a Pagar por cada periodo. Parte Variable
d1$Monto.Ref <- NA # Monto de transacción con la tasa de referencia. Parte Fija
d1$Resultado <- NA # Perdida o Ganancia que pudiéramos tener


for(k in 1:n){
  d1$Monto.MXN[k] = d1$Fx_Swap[k]*monto # monto a pagar, varia en el tiempo por el FX swap
  d1$Monto.Ref[k] = tc_referecia*monto  # Monto Fijo
  d1$Resultado[k] = d1$Monto.Ref[k]-d1$Monto.MXN[k] # Resultado de la cobertura, la diferencia entre los montos con el tipo de cambio de referencia y los montos que se paguen con el tipo de cambio variable
}

# Plot:
barplot(d1$Resultado,col="blue")
# Uno de los dos estará pagando de mas en los primeros periodos y del punto de equilibrio en adelante estara pagando menos, va a tener un ahorro.
# para entender el ahorro ver la tabla cuando el Resultado es negativo. El comprador (de la parte fija) pagaria $195707.9 pero el valor marked to market seria mayor por ejemplo $200282.2 en t=560 dias


# Resultado de la Cobertura = 0 en t=0:

# Si sumo los flujos de efectivo del resultado, la cobertura debera ser cero, es decir tanto lo que page el comprador y el vendedor estara en equilibrio, y
# al dia de hoy ambos tendran un resultado de la cobertura que vale cero.
valor_cobertura_t0 = sum(d1$Resultado) # (MXN) deberia dar un valor cercano a cero.
valor_cobertura_t0 # MXN
# En este caso da $-13.29066 MXN lo cual tiende a cero considerando los pagos de ~$195707.9 MXN
perc = valor_cobertura_t0 / d1$Monto.Ref[1]
perc # % ~= 0%

# Revalorizaron en el tiempo:
# Al paso de los días conforme se vayan conociendo los tipos de cambio Spot en 28, 56, etc dias (los reales no los estimados por el FX swap) 
# se podría revalorizar el Derivado para ir viendo perdidas o ganancias.

# Aplicación de Tasa Equivalente en CCS y IRS:
# Este procedimiento que hicimos en CCS se puede aplicar a los IRS, donde en lugar de tener una tasa fija y una variable, se podria encontrar
# un valor de tasa fija con el cual el Valor Presente de los Flujos de efectivo sea equivalente (cero).

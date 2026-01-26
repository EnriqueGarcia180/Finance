options(scipen=999) # quitar la notacion Cientifica

# Cross Currency SWAP (CCS)

# Use Case: Una empresa que tiene 
# operaciones en dos paises, por 
# ejemplo Apple y Toyota.
# Quizas le convenga a ambas empresas
# hacer un Swap de flujos de efectivo
# para evitar costos transaccionales,
# y asi Apple de Japon no se tiene que
# llevar todos los JPY a U.S.A. y Toyota
# de U.S.A. no se tenga que llevar todos
# los USD a Japon.


# Ejercicio: Considerar que "soy" Apple y pago en USD a Toyota en USA,
# y Toyota nos paga JPY en Japon.

# Contrato de la Empresa Americana
Noc.USD <- 10000000 # $10M - Nocional
r.USD <- 0.08       # Tasa cupon de Intercambio de flujo de efectivo (la ponen las empresas en funcion de las necesidades de flujo de efectivo)
i.USD <- 0.03       # Tasa de Interes en Dolares (la pone el mercado)
s.USD <- 110        # Tipo de cambio - Precio Spot USD/JPY (la pone el mercado)

# Contrato de la Empresa Japonesa
Noc.JPY <- 1200000000 # $1,200M - Nocional
r.JPY <- 0.05         # Tasa cupon de intercambio de flujo de efectivo (la ponen las empresas en funcion de las necesidades de flujo de efectivo)
i.JPY <- 0.01         # Tasa de interes en Yenes (la pone el mercado, i.e. bonos de japon)
s.JPY <- 0.00909090   #Tipo de Cambio Spot JPY/USD (la pone el mercado)

# Nota: Revisar notas de FX Swap para ver lo del tipo de cambio directo o cruzado para el cambio de la paridad

# Algoritmo para Flujo de efectivo en cada periodo:
# 1. Pago USD (como cupon a la otra empresa)
# 2. Recibo JPY (de cupon de la otra empresa)
# 3. Calculo un tipo de cambio FXSwap de JPY a USD
# 4. Usando la tasa TXSwap Cambio esos JPY a USD y Recibo esos USD
# 5. Calculo la Diferencia de los USD que pague y los USD que recibi (Neto)
# 6. Traigo a Valor Presente
# 7. Se repite proceso para cada flujo


# Opcion 1: Valuacion por flujos de efectivo 

d1 <- data.frame(Periodo=c(1,2,3,3), # se repite el tercer periodo por el pago del Nocional al final
                 Pago.USD=NA, 
                 Recibo.JPY=NA, 
                 FXSwap.USD=NA, 
                 Recibo.USD=NA,
                 Neto=NA,
                 VP=NA)

# Llenar tabla d1 con los flujos de efectivo (aqui supondremos son anuales)
for(k in 1:4){
  if(k<=3){ # flujos de efectivo de los primeros 3 cupones (primeros 3 anios)
    # Nota para flujos de efectivo lo que se Paga va con (-), lo que se Recibe va con (+)
    d1$Pago.USD[k] = -Noc.USD*r.USD   # Como si fuera un Bono, calculamos el Cupon (Nocional por a tasa cupon) 
    d1$Recibo.JPY[k] = Noc.JPY*r.JPY  # La empresa japonesa pagaria el notional en JPY multiplicado por su tasa cupon
    d1$FXSwap.USD[k] = s.JPY*(1+i.USD*k)/(1+i.JPY*k)  # Calculo la tasa de cambio FXSwap de cada k periodo, el Spot price o tipo de cambio inicial solo serviria para el primer periodo, Esto lo hace un contrato "Engrapado". Interes Simple 
    d1$Recibo.USD[k] = d1$Recibo.JPY[k]*d1$FXSwap.USD[k] # Cambio los JPY a USD usando el FXSwap. Recibo USD ahora.
    d1$Neto[k] = d1$Pago.USD[k]+d1$Recibo.USD[k] # Diferencia entre los USD pagados y recibidos
    d1$VP[k] = d1$Neto[k]/(1+i.USD*k) # Valor Presente (VP) del Neto en USD, obviamente usando la tasa del USD.
  }else{ # Aqui es el ultimo periodo y se recibe el Nocional
    d1$Pago.USD[k] = -Noc.USD
    d1$Recibo.JPY[k] = Noc.JPY 
    d1$FXSwap.USD[k] = s.JPY*(1+i.USD*3)/(1+i.JPY*3)
    d1$Recibo.USD[k] = d1$Recibo.JPY[k]*d1$FXSwap.USD[k]
    d1$Neto[k] = d1$Pago.USD[k]+d1$Recibo.USD[k]
    d1$VP[k] = d1$Neto[k]/(1+i.USD*3)
  }
}

# Valor total del SWAP:
swap = sum(d1$VP) 
swap 
# Costo del Derivado.
# Costo inicial que alguna de las empresas tiene que pagar para un intercambio justo, 
# en este caso Apple tendria que pagar esto. Ver VP Neto donde apple recbe un saldo a favor positivo mayor.
# Aqui tuvo que ver el intercambio del Nocional, si se hubieran definido diferente el costo del derivado
# hubiera sido mayor.
# Si se quiere hacer un contrato en el que los dos empiecen con Valor Presente cero, es decir que ninguno
# empiece el contrato 'desfavorecido' (que ninguno tenga que pagar al inicio) se tendria que hacer un
# ejercicio de "Optimizacion" para encontrar ya sea la(s) tasa(s) de interes o el monto de los Nocionales para
# garantizar que ambos VP del Derivado sea cero para ambos en el dia cero (swap = $0), a esta tasa se le llama 
# "tasa de referencia SWAP". 
# EL ajuste se hace combinado en los tres factores de riesgo: i.USD,  i.JPY  y  s.JPY
# Nota: en el ultimo periodo se usa el tipo de Cambio Spot y la tasa de interes spot,
# (el del dia del finiquito) de mercado.
# Esta Valuacion con los Spot price y rate se tendria o podrua hacer todos los dias del derivado.





# Opcion 2: Valuacion mediante esquemas de Bonos

# Es como si se tuviera un Bono que se paga en USA y otro que se paga en JPN y
# nos interesa saber la diferencia entre ambos Bonos.

# Bono en USA
d2 <- data.frame(Periodo=c(1,2,3,3), # Al imprimir los periodos serian 1, 2, 3 y 3. Esto es util al usar "k" en el loop de abajo, k=4 en realidad seria en el cuarto renglon pero el periodo es igual a 3.
                 CF.USD=NA, # Cash Flow en USD
                 VP.USD=NA) # Valor Presente en USD

for(k in 1:4){
  if(k<=3){
    d2$CF.USD[k] = -Noc.USD*r.USD # Negativo porque es lo que estamos pagando (Apple)
    d2$VP.USD[k] = d2$CF.USD[k]/(1+i.USD*k) # traerlo a VP descontado con interes simple, este modelo se podria cambiar por compuesto o continuo.
  }else{
    d2$CF.USD[k] = -Noc.USD
    d2$VP.USD[k] = d2$CF.USD[k]/(1+i.USD*3)
  }
}
VP.USD <- sum(d2$VP.USD) # Esto es lo que valdria el Bono. Es lo que pagaria en t=0 la empresa americana (Apple) visto desde el punto de vista de un Bono que se paga a tasa fija.

# Bono en JPN
d3 <- data.frame(Periodo=c(1,2,3,3),
                 CF.JPY=NA,
                 VP.JPY=NA)

for(k in 1:4){
  if(k<=3){
    d3$CF.JPY[k] = Noc.JPY*r.JPY  # aqui es Positivo porque lo paga la empresa Japonesa (lo que supuestamente recibiriamos).
    d3$VP.JPY[k] = d3$CF.JPY[k]/(1+i.JPY*k)
  }else{
    d3$CF.JPY[k] = Noc.JPY
    d3$VP.JPY[k] = d3$CF.JPY[k]/(1+i.JPY*3)
  }
}
VP.JPY <- sum(d3$VP.JPY) # Valor del Bono en JPN, es lo que pagaria la empresa japonesa (Toyota)

# SWAP
swap2 = VP.USD + VP.JPY*s.JPY   # Valor del SWAP mediante esquemas de Bonos
swap2
pata.activa <- VP.USD         # Esto depende de donde tengamos Contablemente el Derivado, si en Activos o Pasivos.
pata.pasiva <- VP.JPY*s.JPY

# Comprobación
swap2 # Valuacion por flujos de efectivo
swap  # Valuacion por esquema de bonos
# El valor del swap es el mismo por ambos metodos.




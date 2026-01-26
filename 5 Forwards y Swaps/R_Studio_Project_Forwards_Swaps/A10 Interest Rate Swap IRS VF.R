# Interes Rate Swaps (IRS)


# Datos:
t <- c(28,56,84,112) # Tiempo expresado en dias. Intercambio de FLujos de Efectivo en estas fechas.
nocional_TF <- 100000000 # Tasa Fija
i_f <- 0.06 # Tasa Fija 6%. Tasa Anual.
nocional_TV <- 100000000 # Tasa Variable. Podria ser diferente pero generalmente es el mismo.
i_v <- c(0.081718718,0.078699906,0.075752006,0.073966434) # Curva de tasas de interes. TIIE. Tasa Variable.
# La tasa i_v (tasa variable) corresponde al interes efectivo entre t1,t2 mediante la tasa FRA o tasa Forward
# FRA = ((1+tasa_lp*T_lp/360)/(1+tasa_cp*T_cp/360)-1)*(360/(T_lp-T_cp)) # Formula slide 4 documento FRA pdf
# es la tasa efectiva por periodo para el tiempo de valuacion.
# Es el # 'cachito' de curva TIIE que tenemos en ese periodo, i.e. para el primer periodo desde el dia 0 al 28, 
# segundo periodo del dia 28 al 56, y asi sucesivamente.
# Esta curva se podria obtener calculando individualmente la FRA o tasa forward para cada periodo.



# Opcion 1: Valuacion por Flujos de Efectivo

d1 = data.frame(Tiempo=seq(28,112, by=28), #calendario lunar con un nodo cada 28 dias
                Cupon_TF=NA,
                VP_TF=NA,
                Cupon_TV=NA,
                VP_TV=NA,
                Neto=NA) # Neto de Flujo de Efectivo

for (k in 1:4){
  d1$Cupon_TF[k] = -nocional_TF*i_f*(28/360) # Pago Fijo (-) y Recibo Variable (+). 28/360 para Tasa Efectiva por periodo, el pago se va a dar cada 28 dias. Este pago es el mismo cada periodo.
  d1$VP_TF[k] = d1$Cupon_TF[k]/(1+i_f*d1$Tiempo[k]/360) # dividir entre 360 para hacer tasas efectivas por periodo. Interes Simple.
  d1$Cupon_TV[k] = nocional_TV*i_v[k]*28/360  # i_v[k] es la tasa FRA o Forward para ese periodo [k] en especifico. Estariamos recibiendo este cupon en cada periodo. Recibimos un pago FRA, un flujo de efectivo con la tasa FRA.
  d1$VP_TV[k] = d1$Cupon_TV[k]/(1+i_v[k]*d1$Tiempo[k]/360) 
  d1$Neto[k] = d1$VP_TF[k]+d1$VP_TV[k] # Se calcula el Neto en Valor presente, tiempo 0.
}

#Costo del derivado
swap = sum(d1$Neto) # Lo que pagaria la persona que esta pagando Tasa Fija (Compra de IRS) al dia de hoy. El que esta pagando tasa variable con estas condiciones de mercado estaria pagando mucho mas dinero.
pata_TF = sum(d1$VP_TF)
pata_TV = sum(d1$VP_TV)

# Comprobacion
pata_TF + pata_TV
swap






# Opcion 2:  Valuacion por Esquema de Bonos

# Bono Tasa Fija
d2 = data.frame(Tiempo=seq(28,112,by=28),
                Cupon_TF=NA,
                VP_TF=NA)
for (k in 1:4){
  d2$Cupon_TF[k] = -nocional_TF*i_f*28/360
  d2$VP_TF[k] = d2$Cupon_TF[k]/(1+i_f*d2$Tiempo[k]/360)
}

# Bono Tasa Variable
d3 <- data.frame(Tiempo=seq(28,112,by=28),
                 Cupon_TV=NA,VP_TV=NA)
for(k in 1:4){
  d3$Cupon_TV[k] = nocional_TV*i_v[k]*28/360
  d3$VP_TV[k] = d3$Cupon_TV[k]/(1+i_v[k]*d3$Tiempo[k]/360) # traer a VP con Interes simple y con tasa efectiva por periodo
}

Bono_TF = sum(d2$VP_TF)
Bono_TV = sum(d3$VP_TV)

#Costo del derivado:
swap2 = Bono_TF + Bono_TV # Igual que arriba. # Lo que pagaria la persona que esta pagando Tasa Fija (Compra de IRS) al dia de hoy. El que esta pagando tasa variable con estas condiciones de mercado estaria pagando mucho mas dinero.


# Comprobacion de ambos metodos
swap2
swap
# Resultados iguales con ambos metodos: flujos de efectivo y bonos.



# Notas:
# La TASA de interes FIJA la definen normalmente las contrapartes en funcion de las condiciones de mercado,
# por ejemplo politicas de Banco de Mexico para decidir que nos combiene, por ejemplo ahorita es una 
# epoca de bajadas de tasas de interes, convendria hacer un dervidado para que nos sigan pagando 
# el 9%, y esto se haria por un anio, considerando que Banxico va a estar bajando las tasas de interes 
# a traves del tiempo. 
# La tasa Fija tambien se define de acuerdo a lo que a uno le convenga, por ejemplo, si la operacion la
# tengo en el Pasivo (yo estoy pagando intereses) y estoy pagando un credito a tasa variable, nos 
# conviene que las tasas de interes vayan bajando ya que estaremos pagando una cuota menor.
# (estar en el activo siginifica que nosotros estariamos recibiendo los intereses).
# 
# La TASA VARIABLE la da el proveedor de precios. Estructura de curvas guvernamentales, o Curvas SOFOR si es 
# en otro pais. Esto sinsumos de mercado se obtienen de manera diaria.
# 
# Conforme a la Opcion 2 de valuacion mediante bonos, en este ejercicio esta relativamente 'facil' 
# porque los dias de valuacion estan elegidos intencionalmente en los nodos pero conforme vaya pasando 
# el tiempo vamos a caer en un problema de interpolacion de tasas de interes para saber dentro de
# por ejemplo 27 dias cuanto vale el cupon, y como estamos haciendo la valuacion de un bono tambien
# tendriamos el problema de valorizar ya sea precio limpio o precio sucio del instrumento financiero, en
# este caso como no hay intereses debengados el precio seria el mismo, pero si los hubiera el precio seria
# diferente.
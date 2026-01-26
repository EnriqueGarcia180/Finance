#Caso de estudio: Forward Rate Agreement (FRA)

# Tiempos: 
#   t0 - Contrato derivado, aqui se pactan las condiciones de tiempo y curvas de tasas de interes
#   t1 - Inicio de la inversión con la curva de tasas de interes de t1
#   t2 - Vencimiento de la inversión
# El riesgo que tenderiamos seria el no saber cual seria la tasa de interes que tendriamos en t1 dias.
# Este contrato derivado que cubre el riesgo de tasa de interes.

# Tendremos tiempos de corto y largo plazo dependiendo de como sea la estructura de tasas de interes.
T_cp = 90  # periodo en número de días del corto plazo: expresado en días (t1), Inicia la Inversion.
T_lp = 180 # periodo en número de días del largo plazo: expresado en días (t2), Fin de la Inversion.
# Inversion de 90 dias (180 - 90)
# Tasas de interes de los nodos de rendimiento de corto y largo plazo provenientes de la curva de tasas de interes
# De la Yield Curve, o Estructura de Tasas de Interes, es curva que genera la ganancia en intereses del tiempo 0 al tiempo t
tasa_cp = 0.101600 # corto plazo. Tasas Anualizadas, habra que dividir entre 360 para obtener la efectiva diaria.
tasa_lp = 0.106450 # largo plazo. Tasas Anualizadas, habra que dividir entre 360 para obtener la efectiva diaria.
# Caluclar la tasa Forward que me este dando lo sintereses ganados entre el tiempo t1 y t2
# FRA: Es una tasa forward. 
# Generando un comparativo de la ganancia de intereses de largo plazo con el corto plazo
# Formula en tiempo continuo: (1+ylp)^(t2)=(1+ycp)^(t1)*(1+FRA)^(t2-t1) ,slide 6. 
# de esta equacion despejaremos FRA para evaluar el derivado, el interes efectivo que voy a ganar entre el
# corto plazo y el largo plazo, en un periodo de inversion de 90 dias, slide 4.
# Tipicamente son contratos de Corto plazo (i.e. aqui 90 dias) por lo tanto se usa un modelo de Interes Simple
FRA = ((1+tasa_lp*T_lp/360)/(1+tasa_cp*T_cp/360)-1)*(360/(T_lp-T_cp)) # Formula slide 4
# Una tasa de interes efectiva que estamos contemplando para un interes efectivo ganado que empieza en el dia t1 y finaliza en el dia t2

# Comprobación 
# (Comprobar ambos lados de la igualdad de la ec. del silde 4)
(1+tasa_lp*T_lp/360) # La inversion a largo plazo
(1+tasa_cp*T_cp/360)*(1+FRA*(T_lp-T_cp)/360) # La inversion a corto plazo mas la reinversion desde t1 a t2 con la tasa de interes Forward (FRA)


#Valor de Liquidación
Nocional = 100000 # el monto por el cual estamos intercambiando el flujo de efectivo
y_var = 0.11 # Supuesta. Tasa de interes variable, tasa de interes efectiva de la inversion a 90 dias en el dia 90 (t1)
# que se analizo con la curva de interes que se genero en el dia 90, es decir una vez que pase el periodo diferido (t1)
# Se va a comparar cual seria la ganancia de intereses con la curva de interes variable al dia 90 (t1) vs la
# tasa de interes FRA que se pacto en el derivado 90 dias antes (t0) y esto va a generar una Ganancia
# o una Perdida dependiendo de como haya sido la curva, como se haya movido la tasa de interes,
# en este caso vemos que la tasa incremento del 10.85% al 11% pero tambien pudo haber disminuido.
plazo = (T_lp - T_cp) 
Liq = (Nocional*(FRA-y_var)*plazo/360)/(1+y_var*plazo/360) # Valor de Liquidacion del Derivado (ya pyesto en t0 o Valor Presente)
# Explicacion: (Nocional*(FRA-y_var)*plazo/360) es la diferencia de los intereses ganados con la tasa FRA y la tasa variable valuada al timpo 90 (t1) 
# que es donde se tiene el diferencial de tasas. Si se corre solo esta parte da -$36.42 que es una perdida porque
# estamos pagando dinero en terminos del FRA y recibiendo es en temrinos de la tasa variable.
# Dado que lo que nos interesa es valuar el Derivado al dia de hoy (t0) entonces lo regresamos 
# a Valor Presente dividiendo por (1+y_var*plazo/360), esto da -$35.4499
# Hay una perdida porque si hubieramos sabido en t0 que la tasa en t1 iba a ser 11% hubieramos mejor invertido a esa tasa,
# pero como usamos la FRA pues no generamos tanto dinero como lo hubiera hecho la tasa variable (desconocida hasta ese momento) del 11%

# Por ejemplo en pandemia subio mucho la tasa de 3-4% a 7-8% en meses.
# Ver politica monetariad de Banxico, por ejemplo ahorita se espera que baje.

# Alternativa
# PL generado por el Derivado (por separado con ambas tasas)
# (todo se usa interes simple porque son a "corto" plazo)
PL_FRA = Nocional*(1+FRA*plazo/360) # Este es el dinero que me estaria pagando la inversion en t2 bajo la tasa del Derivado pactada en t0 por invertir dinero del dia t1 al dia t2. (Valuada en t2)
PL_y = Nocional*(1+y_var*plazo/360) # Con la estructura yield rate. Es decir con las condiciones de mercado al dia t2 analizando cuanto dinero podria generar.
PL = (PL_FRA - PL_y) # Profit/Loss en t2
# Hay una "perdida" porque si hubieramos sabido en t0 que la tasa en t1 iba a ser 11% hubieramos mejor invertido a esa tasa,
# pero como usamos la FRA pues no generamos tanto dinero como lo hubiera hecho la tasa variable (desconocida hasta ese momento) del 11%
VP_PL = PL/(1+y_var*plazo/360) # PL traido a Valor Presente (t0). Usando tambien esquema de interes simple.

# Coincide con "Liq" Liquidacion con el diferencial de tasas.
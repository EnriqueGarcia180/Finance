# FX Swaps

monto = 1000 # $1000MXN monto a intercambiar
S0 = 20.1464
rl = 0.10 # tasa local (en mexico)
rf = 0.05 # tasa foranea (USA)
t = 90 # tiempo en dias

# Calcular el precio del Derivado, Precio Forward o FX Swap en tiempo t
F0_t_simple = S0*(1+rl*t/360) / (1+rf*t/360) # Modelo con Interes Simple (para periodos < 1 anio por lo general el mas usado en el mercado)
F0_t_compuesto = S0*(1+rl)^(t/360) / (1+rf)^(t/360) # Modelo con Interes Compuesto
F0_t_continuo = S0*exp((rl-rf)*(t/360)) # Modelo con Interes Continuo o Fuerza de Interes (se usa mas en los libros porque es mas sencillo derivar e integrar, por la matematica)

# Monto a pagar por $1000MXN en el Futuro (en t dias)
F0_t_simple*monto # transaccion final: $203998.1usd 

# Puntos Swaps: Diferencia que tenemos entre el tipo de Cambio Proyectado - Tipo de Cambio al dia de hoy
p_swaps = F0_t_continuo - S0



# Caso de Estudio: FX Swap considerando una Estructura de Tasas de Interes

  # (Source: Investing.com)
  # BONOS ESTADOS UNIDOS
  # https://mx.investing.com/rates-bonds/americas-government-bonds?maturity_from=10&maturity_to=290#:~:text=13/06-,Estados%20Unidos,-Nombre
  # BONOS MEXICO
  # https://mx.investing.com/rates-bonds/americas-government-bonds?maturity_from=10&maturity_to=290#:~:text=13/06-,M%C3%A9xico,-Nombre
  # FOREX
  # Cambio de Divisas
  # https://mx.investing.com/currencies/single-currency-crosses
  # Tabla Cruzada de Tipo de Cambio
  # https://mx.investing.com/currencies/exchange-rates-table

Curva <- data.frame(Dias=c(30,90,180,360,1080,1800,3600,7200,10800),
                    Y.MXN=c(8.230,8.090,8.130,8.300,8.207,8.755,9.340,9.975,9.920),
                    Y.USD=c(4.183,4.356,4.275,4.074,3.906,4.005,4.406,4.914,4.899))

TC <- data.frame(Bid=c(18.9475),Ask=c(18.9689))

# Ver como da mejor rendimiento a corto plazo que a mediano
plot(x=Curva$Dias, y=Curva$Y.MXN, type="b", col="green")
plot(x=Curva$Dias, y=Curva$Y.USD, type="b", col="red")


# FX Swap usando la curva de tasas de interes
  # Si yo lo quiero Comprar
S0 = TC$Ask[1] # Si yo quiero comprar la contraparte me tiene que vender, tomamos el precio de Ask, la postura opuesta.
t = 180 # plazo en dias
rl = Curva$Y.MXN[3]/100 # en la tercera observacion es a 180 dias / 100 porque vienen en porcentual en la tabla
rf = Curva$Y.USD[3]/100
F0_t_simple_comprar = S0*(1+rl*t/360) / (1+rf*t/360) # el Forward Price (swap) $19.32mxn por $1usd el tipo de cambio en 180 dias
  #  Si yo lo quiero Vender
S0 = TC$Bid[1]
t = 90 # plazo en dias
rl = Curva$Y.MXN[2]/100 # en la segunda observacion es a 90 dias / 100 porque vienen en porcentual en la tabla
rf = Curva$Y.USD[2]/100
F0_t_simple_vender = S0*(1+rl*t/360) / (1+rf*t/360) # el Forward Price (swap) $19.122mxn por $1usd el tipo de cambio en 90 dias


# FX Swap con interpolacion de curva de tasa de interes
# Paso 1: Interpolar la Tasa de Interes de MXN
t = 220 # dia en el que nos interesa calcular la interpolacion de tasa de interes
x = c(180, 360)
y = c(Curva$Y.MXN[3], Curva$Y.MXN[4])
yl = approx(x, y, xout=t) # esta funcion usa un modelo lineal. Quiero que me interpole el valor de la tasa de interes en el dia 220 (t). approx() regresa los dos valores, x,y donde hizo la interpolacion.
rl = yl$y / 100 
# Paso 2: Interpolar la Tasa de Interes de USD
x = c(180, 360)
y = c(Curva$Y.USD[3], Curva$Y.USD[4])
yf = approx(x, y, xout=t) # esta funcion usa un modelo lineal. Quiero que me interpole el valor de la tasa de interes en el dia 220 (t). approx() regresa los dos valores, x,y donde hizo la interpolacion.
rf = yf$y / 100 
# Paso 3: Calcular el precio del Swap USD/MXN en t = 220 (interpolado)
  # Si yo lo quiero Comprar
S0_compra = TC$Ask[1]
F0_t_simple_comprar_interpolado = S0_compra*(1+rl*t/360) / (1+rf*t/360)
  # Si yo lo quiero Vender
S0_venta = TC$Bid[1]
F0_t_simple_vender_interpolado = S0_venta*(1+rl*t/360) / (1+rf*t/360)

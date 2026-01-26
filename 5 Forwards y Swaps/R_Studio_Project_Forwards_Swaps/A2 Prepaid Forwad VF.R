
# A2. Prepaid Forward

# Vamos a adquirir un activo mediante el sistema de Pre-venta


# Caso 1. El activo no genera rendimiento 
# Caso 2. El activo genera plusvalía a una tasa r
# Caso 3. El activo genera un flujo de efectivo
# Caso 4. El activo genera plusvalía y genera un pago 


#Caso 1. El activo no genera rendimiento 
st <- 100000 #cuanto vale el subyacente en el tiempo t
t <- 180 #días
PFt <- st # Precio del prepago o Pre-paid Forward al tiempo t=0, debera
          # ser el mismo al del precio esperado del activo en t=T ya que
          # NO genera ninguna ganancia.



#Caso 2. El activo genera plusvalía a una tasa r
st <- 100000
t <- 180
r <- 0.20 # anualizada, i.e. plusvalía
# Traer a Valor Presente el valor esperado del subyacente en t=T
PFt_simple <- st/(1+r*t/360) #interes simple
PFt_compuesto <- st*(1+r)^(-t/360) #interes compuesto
PFt_continuo <- st*exp(-r*t/360) #fuerza de interes
# nota: en la Teoria se usa mas el interes continuo (es mas fácil derivar la exponencial),
# en la practica se usa mas el interes simple, especialmente en periodos cortos ya que el 
# interes es mayor en periodos menores a 1 anio.


#Caso 3. El activo no genera plusvalía pero si genera un flujo de efectivo

# El flujo de efectivo i.e. dividendo, ya es dinero por lo que no se
#puede traer a Valor Presente con la tasa de plusvalía 'r'
#pero si con 'i' que es la tasa de interes de referencia i.e. CETE, TIIE, etc.
#Dado que el subyacente NO genera plusvalia el VP es el mismo que el valor final.
st <- 100000
t <- 180
i <- 0.10 #tasa de interes
p <- 10000 # el pago generado del activo ($)
tp <- 90 # tiempo del pago del flujo de efectivo.
# En este ejercicio solo es un único pago en 90 días, traerlo a t=0 o VP, solo
# el dividendo ya que el subyacente vale lo mismo en t=0 y t=T porque no genera
# plusvalia
PFt_simple <- st-p/(1+i*tp/360)
PFt_compuesto <- st-p*(1+i)^(-tp/360)
PFt_continuo <- st-p*exp(-i*tp/360)


#Caso 4. El activo si genera plusvalía y genera un pago o flujo de efectivo
st <- 100000
t <- 180
r <- 0.20 #tasa de plusvalía
i <- 0.10 #tasa de interes
p <- 10000
tp <- 90
# En este ejercicio solo es un único pago en 90 días, traerlo a t=0 o VP con la tasa "i",
# aquí si hay plusvalía por lo que tambien hay que traer a VP el precio del
# subyacente con la tasa "r".
PFt_simple <- st/(1+r*t/360)-p/(1+i*tp/360)
PFt_compuesto <- st*(1+r)^(-t/360)-p*(1+i)^(-tp/360)
PFt_continuo <- st*exp(-r*t/360)-p*exp(-i*tp/360)



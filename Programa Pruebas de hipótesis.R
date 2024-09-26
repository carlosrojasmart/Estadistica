# Prueba de hipótesis e intervalo de confianza para la
# media con varianza conocida
# H0: Mu =  Mu0 Vs Ha: Mu diferente de Mu0

# Caso 1. Cuando me dan la varianza o Sigma Cuadrado

xbarra <-    # Se incluye el valor de la media
sigma2 <-    # Se incluye el valor de la varianza o Sigma Cuadrado
Mu0    <-    # Se incluye el valor de la H0
Alpha  <-    # Se incluye el valor de alpha o nivel de significancia
n      <-    # Se incluye el valor de la muestra
  
# Paso 1. Estadístico de prueba
Z <- (xbarra - Mu0)/(sqrt(sigma2/n)) 
Z  # Para obtener el valor del estadístico

# Paso 2. Tabla Normal
Z_alpha2 <- -qnorm(Alpha/2)

# Paso 3. Región de Rechazo
Resultado <- ifelse(Z>= 0, 
                    ifelse(Z>=Z_alpha2,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(Z<=-Z_alpha2,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza
lim_inf<- xbarra - Z_alpha2 * sqrt(sigma2/n) # No se toca
lim_sup<- xbarra + Z_alpha2 * sqrt(sigma2/n) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(Mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                    ifelse(Mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

# Caso 2. Cuando me dan la desviación típica o Sigma

xbarra <-    # Se incluye el valor de la media
sigma  <-    # Se incluye el valor de la desviación típica o Sigma
Mu0    <-    # Se incluye el valor de la H0
Alpha  <-    # Se incluye el valor de alpha o nivel de significancia
n      <-    # Se incluye el valor de la muestra
  
# Paso 1. estadístico de prueba
Z <- (xbarra - Mu0)/(sigma/sqrt(n)) 
Z  # Para obtener el valor del estadístico

# Paso 2. Tabla Normal
Z_alpha2 <- -qnorm(Alpha/2)

# Paso 3. Región de Rechazo
Resultado <- ifelse(Z>= 0, 
                    ifelse(Z>=Z_alpha2,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(Z<=-Z_alpha2,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza
lim_inf<- xbarra - Z_alpha2 * (sigma/sqrt(n)) # No se toca
lim_sup<- xbarra + Z_alpha2 * (sigma/sqrt(n)) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(Mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(Mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

###########################################################################
# Prueba de hipótesis e intervalo de confianza para la
# media con varianza desconocida
# H0: Mu =  Mu0 Vs Ha: Mu diferente de Mu0

# Caso 1. Cuando me dan la varianza muestral o S al Cuadrado

xbarra <-    # Se incluye el valor de la media
s2     <-    # Se incluye el valor de la varianza muestral o S al Cuadrado
Mu0    <-    # Se incluye el valor de la H0
Alpha  <-    # Se incluye el valor de alpha o nivel de significancia
n      <-    # Se incluye el valor de la muestra

# Paso 1. Estadístico de prueba  
t <- (xbarra - Mu0) / (sqrt(s2/n))
t  # Para obtener el valor del estadístico

# Paso 2. Tabla t-Student
t_alpha2<- qt((1-Alpha/2), n-1, lower.tail = T) # No se toca

# Paso 3. Región de Rechazo
Resultado <- ifelse(t>= 0, 
                    ifelse(t>=t_alpha2,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(t<=-t_alpha2,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza
lim_inf<- xbarra - t_alpha2 * (sqrt(s/n)) # No se toca
lim_sup<- xbarra + t_alpha2 * (sqrt(s/n)) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(Mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(Mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

# Caso 2. Cuando me dan la desviaci?n est?ndar o S 

xbarra <-    # Se incluye el valor de la media
s      <-    # Se incluye el valor de la desviaci?n est?ndar o S
Mu0    <-    # Se incluye el valor de la H0
Alpha  <-    # Se incluye el valor de alpha o nivel de significancia
n      <-    # Se incluye el valor de la muestra
  
# Paso 1. estadístico de prueba  
t <- (xbarra - Mu0) / (s/sqrt(n))
t  # Para obtener el valor del estadístico

# Paso 2. Tabla t-Student
t_alpha2<- qt((1-Alpha/2), n-1, lower.tail = T) # No se toca

# Paso 3. Región de Rechazo
Resultado <- ifelse(t>= 0, 
                    ifelse(t>=t_alpha2,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(t<=-t_alpha2,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza
lim_inf<- xbarra - t_alpha2 * (s/sqrt(n)) # No se toca
lim_sup<- xbarra + t_alpha2 * (s/sqrt(n)) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(Mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(Mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

# Caso 3. Cuando me dan datos individuales

datos  <- c() # Ac? se digitan los datos y se separan con comas
xbarra <- mean(datos)
desv   <- sqrt(var(datos))
n      <- length(datos)
Mu0    <-    # Se incluye el valor de la H0
alpha  <-    # Se incluye el valor de alpha o nivel de significancia

# Paso 1. estadístico de prueba  
est <- (xbarra - Mu0) / (desv / sqrt(n))
est  # Para obtener el valor del estadístico

# Paso 2. Tabla t-Student
cuantil <- qt((1-alpha/2), n-1, lower.tail = T) # No se toca
cuantil

# Paso 3. Región de Rechazo

Resultado <- ifelse(est>= 0, 
                    ifelse(est>=cuantil,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(est<=-cuantil,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza

lim_inf<- xbarra - cuantil * (desv/sqrt(n)) # No se toca
lim_sup<- xbarra + cuantil * (desv/sqrt(n)) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(Mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(Mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1


#####################################################################
###################################################################
######### P.H.para la dif de medias con varianzas conocidas ############

xbarra1 <-   # Media 1. Datos del problema
xbarra2 <-   # Media 2. Datos del problema
var1 <-      # Datos del problema (Sigma1 al cuadrado)
var2 <-      # Datos del problema (Sigma2 al cuadrado)
n1 <-        # Muestra de la población 1
n2 <-        # Muestra de la población 2
mu0 <-       # Media de referencia
alpha <-     # Nivel de significancia

# Paso 1. estadístico de prueba
est <- ((xbarra1 - xbarra2)  - mu0) / sqrt((var1/n1)+(var2/n2))
est  # Para obtener el valor del estadístico

# Paso 2. Tabla Normal
pvalor <- pnorm(est)  # Para obtener el valor-P
pvalor

# Paso 3. Región de Rechazo
ifelse(pvalor < alpha/2, "Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula" )

# Paso 4. Intervalo de confianza

lim_inf<- (xbarra1 - xbarra2) - ((-qnorm(alpha/2)) * (sqrt((var1/n1)+(var2/n2)))) # No se toca
lim_sup<- (xbarra1 - xbarra2) + ((-qnorm(alpha/2)) * (sqrt((var1/n1)+(var2/n2)))) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1


############ P.H.para la dif de medias con varianzas desconocidas ############

# Caso 1. Cuando me dan la información de medias y varianzas

xbarra1 <-   # Media 1. Datos del problema
xbarra2 <-   # Media 2. Datos del problema
var1 <-      # Datos del problema (S1 al cuadrado)
var2 <-      # Datos del problema (S2 al cuadrado)
n1 <-        # Muestra de la población 1
n2 <-        # Muestra de la población 2
mu0 <-       # Media de referencia
alpha <-     # Nivel de significancia

# Paso 1. estadístico de prueba
  
sp <- sqrt((((n1-1)*var1)+(n2-1)*var2)/(n1+n2-2))
est <- ((xbarra1-xbarra2) - mu0) / (sp * sqrt((1/n1)+(1/n2)))
est  # Para obtener el valor del estadístico

# Paso 2. Tabla t-student

cuantil<- qt((1-alpha/2), n1+n2-2, lower.tail = T) # No se toca
cuantil

# Paso 3. Región de rechazo

Resultado <- ifelse(est>= 0, ifelse(est>=cuantil,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(est<= -cuantil,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza.

lim_inf<- (xbarra1 - xbarra2) - (cuantil * (sp * sqrt((1/n1)+(1/n2)))) # No se toca
lim_sup<- (xbarra1 - xbarra2) + (cuantil * (sp * sqrt((1/n1)+(1/n2)))) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

# Caso 2. Cuando me dan los datos 

datos1 <- c() # Incluir los datos individuales
datos2 <- c() # Incluir los datos individuales
xbarra1 <-  mean(datos1)
xbarra2 <-  mean(datos2)
var1 <- var(datos1)
var2 <- var(datos2)
n1 <- length(datos1)
n2 <- length(datos2)
mu0 <-       # Media de referencia dada en la hipótesis
alpha <-     # Nivel de significancia
  
# Paso 1. estadístico de prueba
  
sp <- sqrt((((n1-1)*var1)+(n2-1)*var2)/(n1+n2-2))
est <- ((xbarra1-xbarra2) - mu0) / (sp * sqrt((1/n1)+(1/n2)))
est  # Para obtener el valor del estadístico

# Paso 2. Tabla t-student

cuantil<- qt((1-alpha/2), n1+n2-2, lower.tail = T) # No se toca
cuantil

# Paso 3. Región de rechazo

Resultado <- ifelse(est>= 0, ifelse(est>=cuantil,"Rechazar la Hipótesis Nula","No Rechazar la Hipótesis Nula"),
                    ifelse(est<= -cuantil,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado

# Paso 4. Intervalo de confianza.

lim_inf<- (xbarra1 - xbarra2) - (cuantil * (sp * sqrt((1/n1)+(1/n2)))) # No se toca
lim_sup<- (xbarra1 - xbarra2) + (cuantil * (sp * sqrt((1/n1)+(1/n2)))) # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(mu0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(mu0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

###############################################################
############ P.H.para la varianza ############

# Caso 1. Cuando me dan la información

var1 <-     # Varianza muestral
n <-        # Muestra
sigma0 <-   # pregunta
alpha <-    # Nivel de significancia

# Paso 1. estadístico de prueba
  
est<-((n-1)*var1)/sigma0
est

# Paso 2. Tabla Ji-Cuadrado

cuantil1 <- qchisq(alpha/2, n-1); cuantil1
cuantil2 <- qchisq(1-alpha/2, n-1); cuantil2

# Paso 3. Región de rechazo

Resultado1 <- est<= cuantil1
Resultado2 <- est>= cuantil2

ifelse(Resultado1 == Resultado2,"No Rechazar la Hipotesis Nula","Rechazar la Hipótesis Nula")

# Paso 4. Intervalo de confianza.

lim_inf<-  ((n-1)*var1)/cuantil2# No se toca
lim_sup<-  ((n-1)*var1)/cuantil1 # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(sigma0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(sigma0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

# Caso 2. Cuando me dan datos

datos  <- c() # Incluir los individuales
var1   <- var(datos)
n      <- length(datos)
sigma0 <-    # pregunta
alpha  <-    # Nivel de significancia

# Paso 1. estadístico de prueba
  
est<-((n-1)*var1)/sigma0
est

# Paso 2. Tabla Ji-Cuadrado

cuantil1 <- qchisq(alpha/2, n-1); cuantil1
cuantil2 <- qchisq(1-alpha/2, n-1); cuantil2

# Paso 3. Región de rechazo

Resultado1 <- est<= cuantil1
Resultado2 <- est>= cuantil2

ifelse(Resultado1 == Resultado2,"No Rechazar la Hipotesis Nula","Rechazar la Hipótesis Nula")

# Paso 4. Intervalo de confianza.

lim_inf<-  ((n-1)*var1)/cuantil2# No se toca
lim_sup<-  ((n-1)*var1)/cuantil1 # No se toca
IC <- c(lim_inf, lim_sup) # No se toca                 
IC # Resultado Final
Resultado1 <- ifelse(sigma0>=lim_sup,"Rechazar la Hipótesis Nula",
                     ifelse(sigma0<=lim_inf,"Rechazar la Hipótesis Nula", "No Rechazar la Hipótesis Nula"))
Resultado1

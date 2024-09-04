# 0. PAQUETES Y LIBRERÍAS

library(nortest)
require(car)
library(lmtest)


# 1. CONJUNTO DE DATOS

hospital <- as.factor(rep(1:4,each = 9,len = 36))
hospital

medico <- as.factor(rep(1:3,each= 3, len = 36))
medico

# Factor hospital ---- > 4 niveles
# Factor médico ---- > 3 niveles

dias <-c(7,10,8,11,9,12,12,11,15,9,9,12,14,11,12,14,13,10,
         8,12,13,11,14,14,17,20,19,6,7,6,9,8,9,8,10,6)

df <-data.frame(cbind(hospital,medico,dias)) 

head(df,15)


# 2. MODELOS 

modelo_fs <- lm(dias~hospital+medico)
summary(modelo_fs)
anova_fs <- aov(modelo_fs)
summary(anova_fs)

# Significatividad en ambos coeficientes.


# 3. ANIDADO

modelo_a<- lm(dias~hospital+medico%in%hospital)
summary(modelo_a)

anova_a <- aov(modelo_a)
summary(anova_a)

# p-valor es < 0.05
# Hay diferencias significativas entre los 4 hospitales 
# respecto a los días.
# p-valor para médicos dentro de cada hospital es < 0.05 



# 3. COMPROBACIONES DE LAS HIPÓTESIS DEL MODELO

# Definir residuos y ajustados.

residuos_fs <-  modelo_fs$residuals
ajustados_fs <-  modelo_fs$fitted.values

residuos_a<- modelo_a$residuals
ajustados_a <- modelo_a$fitted.values


# 3.1. NORMALIDAD

# 3.1.1 FACTORIAL


boxplot(residuos_fs)

# No es simétrico.
# Bigotes tampoco.
# Outliers.


qqnorm(residuos_fs) 
qqline(residuos_fs) 

# No se ajusta a la línea.


# Test

lillie.test(residuos_fs) # No hay normalidad.


# 3.1.2 ANIDADO


boxplot(residuos_a)

# A priori, es bastante simétrico. 
# Bigotes más o menos del mismo tamaño.
# Línea negra no está en medio de la caja.


qqnorm(residuos_a) 
qqline(residuos_a) 

# A priori, podría ajustarse a una normal. 
# Sigue más o menos una línea recta.


# Test

lillie.test(residuos_a) # Hay normalidad.


# 3.2. INDEPENDENCIA DE RESIDUOS

# 3.2.1 FACTORIAL


tiempo <- seq(1:length(residuos_fs))
plot(tiempo,residuos_fs);abline(h=0)

# Hay presencia de patrón.
# Los residuos apuntan a que no son independientes.


# Test

# H0: coef correlación = 0 , residuos independientes.

durbinWatsonTest(modelo_fs)
# p-valor < 0.02
# No hay residuos independientes.


# 3.2.1 ANIDADO

tiempo <- seq(1:length(residuos_a))
plot(tiempo,residuos_a);abline(h=0)

# Sigue una línea recta (no hay picos),la nube de puntos 
# está a horizontal.
# Residuos pueden ser independientes.



# Test

durbinWatsonTest(modelo_a)

# p-valor = 0,562 > 0,05
# Acepto H0, residuos independientes.


# 3.3 VARIANZA (Igualdad de Varianzas)

# 3.3.1 FACTORIAL 

plot(ajustados_fs,residuos_fs)
plot(residuos_fs,hospital) 

# Hay patrones. 
# Apunta a que no hay homocedasticidad.

bptest(modelo_fs)

# p-valor > 0.05 Hay homocedasticidad.


# 3.3.2 ANIDADO

plot(ajustados_a,residuos_a)
plot(residuos_a,hospital)

# No parece que haya patrones.
# Mismas alturas.

# Apunta a que hay homocedasticidad.

bptest(modelo_a)

# p-valor 0.347 > 0.05 Hay homocedasticidad.



# 4. CONTRASTE NO PARAMÉTRICO MODELO FACTORIAL

# Para el ANIDADO nos vale el ANOVA puesto que se cumplen 
# los supuestos.

# Para el FACTORIAL tendremos que realizar el contraste de 
# Kruskall-Wallis.


# 4.1.1 SIGNIFICATIVIDAD FACTOR HOSPITAL


kruskal_hospital <- kruskal.test(dias ~ hospital, data = df)
print(kruskal_hospital) 


# El factor hospital es significativo, p-valor < 0.05


# 4.1.2 SIGNIFICATIVIDAD FACTOR MÉDICO

kruskal_medico <- kruskal.test(dias ~ medico, data = df)
print(kruskal_medico)

# El factor médico no es significativo, p-valor > 0.05


getwd()
setwd("C:/Users/gabop/SEMILLERO-SEA-UN/Limpieza_tablas")
#### Objetivos:
# 1. Identificar los factores demográficos (edad,sexo, nivel socioeconómico, etc.) 
#    y geográficos asociados con el consumo de sustancias psicoactivas.
# 2. Evaluar la relación entre el consumo de sustancias psicoactivas y la 
#    percepción de salud física y mental.
# 3. Caracterizar a los consumidores de sustancias y la implicación del consumo 
#    para la salud pública y la prevención de adicciones.
# 4. Identificar los factores asociados con el desenlace de consumo de sustancias 
#    psicoactivas.



# Librerias ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggridges)
library(FactoClass)
library(fastDummies)

# Lectura de la base ------------------------------------------------------

drogas <- readRDS("Drogas.RDS")

dim(drogas)
str(drogas)
summary(drogas)
View(drogas)
sum(rowSums(is.na(drogas)) > 0)

sum(is.na(drogas))

# Resumen Básico ----------------------------------------------------------

drogas[drogas[,"SEXO"] == 1,"SEXO"] <- "Male"
drogas[drogas[,"SEXO"] == 2,"SEXO"] <- "Female"

ggplot(drogas, aes(x= EDAD, fill = SEXO)) +
  geom_histogram(alpha = 0.5)+
  scale_color_manual(values = c("pink", "blue")) +
  labs(y = "Count", x = "Age") +
  theme_minimal()

## Percepción del consumo -------------------------------------------------

# D11 opinion riesgo que corre la persona
# 1 Ningun riesgo | 2 Riesgo Leve | 3 Riesgo Moderado
# 4 Gran riesgo   |5 No sé qué riesgo corre 
#   a. fuma cigarrillos frecuentemente
#   b. toma bebidas alcoholicas frecuentemente
#   c. tomara tranquilizantes sin prescripción médica una o dos veces en los últimos 3 meses
#   d. "                                            " algunas veces (ocacionalmente) 
#   e. "                                            " frecuentemente
#   f. fumara marihuana una o dos veces en los últimos tres meses
#   g. "              " algunas veces
#   h. "              " frecuentemente
#   i. consumiera cocaina una o dos veces en los últimos 3 meses
#   j. "                " algunas veces
#   k. "                " frecuentemnte
#   l. consumiera basuco una o dos veces en los último 3 meses
#   m. "               " algunas veces
#   n. "               " frecuentemente

# variables de interés y creación del data.frame
cinteres <- c(2:4, 15:28)
interes <- drogas[, cinteres]
D_11 <- interes[,1:4]
for(i in 5:17){
  new_col <- interes[, c(1:3, i)]
  colnames(new_col)[4] <- colnames(D_11)[4]  
  D_11 <- rbind.data.frame(D_11, new_col)
}

d11 <- rep(names(drogas[15:28]), each = dim(drogas)[1])
D_11 <- cbind(D_11, d11)

general <- D_11 %>% group_by(d11,  D_11_A) %>%
  summarise(categorias = n()/dim(drogas)[1])

ggplot(D_11, aes(y= d11, fill =D_11_A )) +
  geom_bar() +
  theme_minimal()

## Nueva tabla -------------------------------------------------------------
df.nuevo <- new_drogs <- drogas[,-cinteres]

for (i in 1:13){
  df.nuevo <- rbind(df.nuevo, new_drogs)
}
dim(D_11)
dim(df.nuevo)

df.nuevo <- cbind(D_11, df.nuevo)



### 5 No conocimiento del riesgo -------------------------------------------

daño.5 <- df.nuevo %>% filter(D_11_A == 5)

ggplot(daño.5, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.5 <- daño.5 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                       TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)

ggplot(sociedad.5, aes(x = SEXO, fill = ESTRATO)) +
  geom_bar() + 
  theme_minimal()

ggplot(sociedad.5, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_hist_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

ggplot(sociedad.5, aes(x = EDAD, fill = SEXO)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("pink", "blue")) +
  theme_minimal()


### 1 Ningún riesgo -----------------------------------------------

daño.1 <- df.nuevo %>% filter(D_11_A == 1)

ggplot(daño.1, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.1 <- daño.1 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)

ggplot(sociedad.1, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.1, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()



### 2 Riesgo leve -------------------------------------------------
daño.2 <- df.nuevo %>% filter(D_11_A == 2)

ggplot(daño.2, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.2 <- daño.2 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)

ggplot(sociedad.2, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.2, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

### 3 Riesgo Moderado ---------------------------------------------
daño.3 <- df.nuevo %>% filter(D_11_A == 3)

ggplot(daño.3, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.3 <- daño.3 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)

ggplot(sociedad.3, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.3, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()


### 4 Gran Riesgo -------------------------------------------------

daño.4 <- df.nuevo %>% filter(D_11_A == 4)

ggplot(daño.4, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.4 <- daño.4 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)

ggplot(sociedad.4, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.4, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()


# Descriptivo multivariado ------------------------------------------------

interes <- c("DIRECTORIO", "EDAD", "Depmuni", "ESTRATO", "TIPO", "SERVICIO", 
             "TOTAL_PERSONAS", "SEXO", "PARENTESCO", names(drogas)[15:28])
# Parentesco
# 1 jefe del hogar | 2 pareja | 3 hijo, hijastro | 4 yerno, nuera | 5 nieto
# 6 padre, madre, suegro/a | 7 Hermano | 8 Otro pariente | 9 empleado del 
# servicio doméstico y sus parientes | 9 Otro no pariente

# Servicio 
# https://www.minsalud.gov.co/proteccionsocial/Paginas/cicloVida.aspx#:~:text=6%20%2D%2011%20a%C3%B1os)-,Adolescencia%20(12%20%2D%2018%20a%C3%B1os),o%20mas)%20envejecimiento%20y%20vejez

# Adolescencia (12 - 18 años)
# Juventud (14 - 26 años)
# Adultez (27- 59 años)
# Persona Mayor (60 años o mas) envejecimiento y vejez

drogas.desc <- drogas %>% select(one_of(interes) ) %>%
  mutate_at(vars(-c(EDAD, DIRECTORIO)), as.factor) %>%
  mutate(G_ETARIO = case_when(EDAD < 18 ~ "ADOLESCENCIA", 
                              18 <= EDAD & EDAD < 27 ~ "JOVEN", 
                              27 <= EDAD & EDAD < 30 ~ "ADULTO JOVEN", 
                              30 <= EDAD & EDAD < 40 ~ "ADULTO",))

for(i in names(drogas)[15:28]){
  drogas.desc[[i]] <- factor(drogas.desc[[i]], levels = c("1":"5"), 
                             labels = c("NR", "LR", "MR", "BR", "NKR"))
}
# añadir edad como factor
max(drogas.desc$TOTAL_PERSONAS)
str(drogas.desc)

## Tablas de Contingencia -------------------------------------------------

# 1 Ningun riesgo | 2 Riesgo Leve | 3 Riesgo Moderado
# 4 Gran riesgo   |5 No sé qué riesgo corre 

# Estrato vs percepción del consumo de drogas

for(i in names(drogas)[15:28]){
  k1 <- unclass(table(drogas.desc$ESTRATO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Estrato ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Estrato ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Estrato ", names(drogas.desc[, i]), " 3" ))
}

# Tipo vs percepción del consumo de drogas

for(i in names(drogas)[15:28]){
  k1 <- unclass(table(drogas.desc$TIPO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Tipo ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Tipo ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Tipo ", names(drogas.desc[, i]), " 3" ))
}

# Tipo vs percepción del consumo de drogas

for(i in names(drogas)[15:28]){
  k1 <- unclass(table(drogas.desc$TIPO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Tipo ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Tipo ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Tipo ", names(drogas.desc[, i]), " 3" ))
}

# Servicio vs percepción del consumo de drogas

for(i in names(drogas)[15:28]){
  k1 <- unclass(table(drogas.desc$SERVICIO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Servicio ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Servicio ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Servicio ", names(drogas.desc[, i]), " 3" ))
}

# depmuni vs percepción del consumo de drogas

for(i in names(drogas)[15:28]){
  k1 <- unclass(table(drogas.desc$Depmuni, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Municipio ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Municipio ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Municipio ", names(drogas.desc[, i]), " 3" ))
}


# https://rpubs.com/ENRIQUE_PEREZ/AFCM

z <- acm.disjonctif(drogas.desc)
acm.disjonctif(drogas.desc)
acm.burt(drogas.desc, drogas.desc)
intento <- dummy_cols(drogas.desc)
intento2 <- apply(intento, 2, sum)

TDC<-select(intento,8:26)
col.sums<-apply(TDC, 2, sum)

# MATRIZ EN TERMINOS DE PROPORCIONES
n<-sum(TDC)
P<-TDC/n
## OBTENEMOS LOS MARGINALES FILAS Y COLUMNAS
P<-as.matrix(P)
rr<-margin.table(P,1)
cc<-margin.table(P,2)
### OBTENCIÓN DE LA MATRIZ ESTANDARIZADA (S)
S<-diag(rr^(-0.5)) %*% (P-rr %*%t (cc))%*%diag(cc^(-0.5))
## REALIZAMOS LA DESCOMPOSICIÓN DE LA MATRIZ (S)
u<-svd(S)$u
v<-svd(S)$v
Da<-diag(svd(S)$d)
# OBTENEMOS LAS COORDENADAS PRINCIPALES PARA LOS PUNTOS INDIVIDUOS  LOS PUNTOS VARIABLES
FF<-diag(rr^(-0.5))%*% u %*%Da
GG<-diag(cc^(-0.5))%*% v %*%Da
# CÁLCULO DE LA INERCIA
cumsum(svd(S)$d)/sum(svd(S)$d)
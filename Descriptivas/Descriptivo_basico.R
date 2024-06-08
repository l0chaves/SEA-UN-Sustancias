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
library(readxl)
library(ggplot2)
library(ggridges)
library(FactoClass)
library(fastDummies)
library(gplots)
library(pacman)
library(FactoMineR)
library(readr)

# https://rpubs.com/StefanoH/929624



# Lectura de la base ------------------------------------------------------

drogas <- readRDS("Drogas.RDS")
names(drogas)
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

ggplot(D_11, aes(x= d11, fill =D_11_A )) +
  geom_bar() +
  labs(fill = "Perception", title = "Drugs vs Perception") +
  xlab("Drugs")+
  ylab("Count")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 12),  # Tamaño de las etiquetas en los ejes
    axis.title = element_text(size = 14),  # Tamaño del texto en los títulos de los ejes
    legend.text = element_text(size = 12),  # Tamaño del texto en la leyenda
    legend.title = element_text(size = 14)  # Tamaño del texto del título de la leyenda
  )

## Nueva tabla -------------------------------------------------------------
df.nuevo <-new_drogs <- drogas[,-cinteres]

for (i in 1:13){
  df.nuevo <- rbind(df.nuevo, new_drogs)
}
dim(D_11)
dim(df.nuevo)

df.nuevo <- cbind(D_11, df.nuevo)


# Define la ruta donde quieres guardar el archivo
ruta_archivo <- "C:/Users/gabop/SEMILLERO-SEA-UN/Limpieza_tablas/df_nuevo.RDS"

# Guarda el data frame como un archivo RDS en la ruta especificada
saveRDS(df.nuevo, file = ruta_archivo)

df.nuevo <- readRDS("df_nuevo.rds")


### 5 No conocimiento del riesgo -------------------------------------------

daño.5 <- df.nuevo %>% filter(D_11_A == 5)

ggplot(daño.5, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.5 <- daño.5 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                       TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO) %>%
  filter(ESTRATO != "NA")

ggplot(sociedad.5, aes(x = SEXO, fill = ESTRATO)) +
  geom_bar() + 
  theme_minimal()

ggplot(sociedad.5, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges (alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  labs(x = "Age", y = "Stratum", fill = "Sex", title = "No knowledge of risk")+
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
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)  %>%
  filter(ESTRATO != "NA")

ggplot(sociedad.1, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.1, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  labs(x = "Age", y = "Stratum", fill = "Sex", title = "No risk")+
  theme_minimal()



### 2 Riesgo leve -------------------------------------------------
daño.2 <- df.nuevo %>% filter(D_11_A == 2)

ggplot(daño.2, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.2 <- daño.2 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)  %>%
  filter(ESTRATO != "NA")

ggplot(sociedad.2, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.2, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  labs(x = "Age", y = "Stratum", fill = "Sex", title = "Low risk")+
  theme_minimal()

### 3 Riesgo Moderado ---------------------------------------------
daño.3 <- df.nuevo %>% filter(D_11_A == 3)

ggplot(daño.3, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.3 <- daño.3 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)  %>%
  filter(ESTRATO != "NA")

ggplot(sociedad.3, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.3, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  labs(x = "Age", y = "Stratum", fill = "Sex", title = "Moderate risk")+
  theme_minimal()


### 4 Gran Riesgo -------------------------------------------------

daño.4 <- df.nuevo %>% filter(D_11_A == 4)

ggplot(daño.4, aes(y= d11, fill = SEXO )) +
  geom_bar() +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal()

sociedad.4 <- daño.4 %>% select(DIRECTORIO, Depmuni, ESTRATO, TIPO, SERVICIO, 
                                TOTAL_PERSONAS, SEXO, EDAD, PARENTESCO)  %>%
  filter(ESTRATO != "NA")

ggplot(sociedad.4, aes(x = EDAD, fill = ESTRATO)) +
  geom_histogram(alpha =0.7) + 
  theme_minimal()

ggplot(sociedad.4, aes(x = EDAD, y = ESTRATO, fill = SEXO)) +
  geom_density_ridges(alpha = 0.5) +
  scale_color_manual(values = c("pink", "blue")) +
  labs(x = "Age", y = "Stratum", fill = "Sex", title = "No risk")+
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
  mutate(G_ETARIO = case_when(EDAD < 18 ~ "T", # adolescente
                              18 <= EDAD & EDAD < 25 ~ "Y", # joven
                              25 <= EDAD & EDAD < 35 ~ "YA", # adulto joven
                              35 <= EDAD & EDAD < 45 ~ "A", # adulto
                              45 <= EDAD & EDAD < 64 ~ "E", # adulto mayor
                              64 <= EDAD ~ "TA")) %>% # tercera edad
  mutate_at(vars(-c(EDAD, DIRECTORIO)), as.factor)

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

namesDrugs <- c("Cigarettes", "Alcoholic Beverage", "Tranquilizer once or twice", 
                "Tranquilizer occasionally", "Tranquilizer frequently",
                "Marijuana once or twice", "Marijuana occasionally", 
                "Marijuana frequently", "Cocaine once or twice", 
                "Cocaine occasionally", "Cocaine frequently", 
                "Basuco once or twice", "Basuco occasionally", 
                "Basuco frequently")
length(namesDrugs)
length(names(drogas.desc)[10:23])
# Estrato vs percepción del consumo de drogas
graficos <- list()

names(drogas.desc)[10:23] <- namesDrugs

for(i in  names(drogas.desc)[10:23]){
  k1 <- unclass(table(drogas.desc$ESTRATO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Estrato ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Estrato ", names(drogas.desc[, i]), " 2" ))
  G_EP <- plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Stratum ",  names(drogas.desc[, i])))
  
  graficos[[paste0("G.Est.", names(drogas.desc[, i]) )]] <- recordPlot()
}

table(drogas.desc$ESTRATO)


# Tipo vs percepción del consumo de drogas

for(i in names(drogas.desc)[10:23]){
  k1 <- unclass(table(drogas.desc$TIPO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Tipo ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Tipo ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Tipo ", names(drogas.desc[, i])))
  
  graficos[[paste0("G.Tipo.", names(drogas.desc[, i]) )]] <- recordPlot()
}


## Servicio vs percepción del consumo de drogas --------------------

for(i in names(drogas.desc)[10:23]){
  k1 <- unclass(table(drogas.desc$SERVICIO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Servicio ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Servicio ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Servicio ", names(drogas.desc[, i])))
  
  graficos[[paste0("G.Serv.", names(drogas.desc[, i]) )]] <- recordPlot()
}

# depmuni vs percepción del consumo de drogas

# no es necesario
for(i in names(drogas.desc)[10:23]){
  k1 <- unclass(table(drogas.desc$Depmuni, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Municipio ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Municipio ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Municipio ", names(drogas.desc[, i]) ))
  
  graficos[[paste0("G.Dep.", names(drogas.desc[, i]) )]] <- recordPlot()
}

# Grupo Etario vs percepción del consumo de drogas

for(i in names(drogas.desc)[10:23]){
  k1 <- unclass(table(drogas.desc$G_ETARIO, drogas.desc[[i]]))
  
  acs1 <- dudi.coa(k1, scannf = F)
  
  # plot(acs1, Tcol = F, cframe = 1, main = paste0("Grupo Etario ", names(drogas.desc[, i]) ))
  # plot(acs1, Trow =F, col.col = "black", main = paste0("Grupo Etario ", names(drogas.desc[, i]), " 2" ))
  plot(acs1, cframe = 1, col.col = "blue",
       main = paste0("Plano Factorial Grupo Etario ", names(drogas.desc[, i]) ))
  
  graficos[[paste0("G.Eta.", names(drogas.desc[, i]) )]] <- recordPlot()
}
### Guardanto Gráficos ----------------------------------------------------

# G.Est.
# G.Tipo.
# G.Eta.

names_graficos <- names(graficos)
pattern <- "^(G\\.Est\\.|G\\.Tipo\\.|G\\.Eta\\.)"
graficos_filtrados <- names_graficos[grep(pattern, names_graficos)]

# Crear y cambiar al nuevo directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
old_wd <- getwd()

# Guardar los gráficos filtrados
for (nombre in graficos_filtrados) {
  grafico <- graficos[[nombre]]
  file_name <- paste0("C:/Users/gabop/SEMILLERO-SEA-UN/imágenes/Descriptiva/", nombre, ".png")
  png(file_name)
  print(grafico)
  dev.off()
}

# Volver al directorio original
setwd(old_wd)
names(graficos)
names(graficos)

# Análisis de correspondencias múltiples ----------------------------------


## Teniendo en cuenta Depmuni ---------------------------------------------
y <- drogas.desc[,-(1:2)]
sum(rowSums(is.na(y)>0))
# Eliminamos los na
# y <- y[rowSums(is.na(y)>0) == 0,] 
tabla_disyuntiva <- dummy_cols(y, names(y))
sum(is.na(tabla_disyuntiva))

matriz_disyuntiva <- as.matrix(tabla_disyuntiva)
sum(is.na(matriz_disyuntiva))
tabla_disyuntiva <- apply(tabla_disyuntiva, 2, as.numeric)

matriz_burt <- t(tabla_disyuntiva) %*% (tabla_disyuntiva)
View(matriz_burt)
sum(is.na(matriz_burt))

matriz_burt[is.na(matriz_burt)] <- 0

centro_gravedad<- colSums(matriz_burt)/nrow(matriz_burt) / length(names(y))



acm <- dudi.acm(y, scannf = F, nf = 3)
barplot(acm$eig, cex.axis = 0.8)

eiglst <- data.frame( vp = acm$eig, porce = acm$eig *100 / sum(acm$eig),
                      acupor = cumsum(acm$eig)*100/sum(acm$eig))

interacm <- inertia.dudi(acm, T, T)


plot(sample(acm, 50), Tcol = F, cframe = 1, cex.row = 0.6,
     cex.global =  0.8, gg = T)

acm$eig[1]/sum(acm$eig)

## Sin incluir Depmuni ------------------------------------------------------

y <- drogas.desc[,-(1:3)]
sum(rowSums(is.na(y)>0))
# Eliminamos los na
# y <- y[rowSums(is.na(y)>0) == 0,] 
tabla_disyuntiva <- dummy_cols(y, names(y))
sum(is.na(tabla_disyuntiva))

matriz_disyuntiva <- as.matrix(tabla_disyuntiva)
sum(is.na(matriz_disyuntiva))
tabla_disyuntiva <- apply(tabla_disyuntiva, 2, as.numeric)

matriz_burt <- t(tabla_disyuntiva) %*% (tabla_disyuntiva)
View(matriz_burt)
sum(is.na(matriz_burt))

matriz_burt[is.na(matriz_burt)] <- 0

centro_gravedad<- colSums(matriz_burt)/nrow(matriz_burt) / length(names(y))

acm <- dudi.acm(y[sample(1:dim(y)[1], 800), ],  scannf = F, nf = 3)
barplot(acm$eig, cex.axis = 0.8)

eiglst <- data.frame( vp = acm$eig, porce = acm$eig *100 / sum(acm$eig),
                      acupor = cumsum(acm$eig)*100/sum(acm$eig))

interacm <- inertia.dudi(acm, T, T)


plot(acm, Tcol = F, cframe = 1, cex.row = 0.6,
     cex.global =  0.8, gg = T)

acm$eig[1]/sum(acm$eig)


# codigo figura 6.1 -------------------------------------------------------
y <- drogas.desc[,-(1:3)]

names(drogas.desc)
acm <- dudi.acm(y, scannf = FALSE, nf = 3)
barplot(acm$eig, cex.axis = 0.6)

eiglst <- data.frame(vp = acm$eig, porce = acm$eig*100/sum(acm$eig), 
                     acupor=cumsum(acm$eig)*100/sum(acm$eig))



plot(acm, Trow = FALSE, gg = TRUE, col.col = "black", cex.global = 0.8, cframe = 1.1)

plot(acm, Trow = FALSE, gg = TRUE, col.col = "black", cex.global = 0.8, cframe = 1.1,
     xlim = c(-1,1), ylim = c(-1,1))

plot(acm, Trow = FALSE, gg = TRUE, col.col = "black", cex.global = 0.8, cframe = 1.1,
     xlim = c(-0.5,0.5), ylim = c(-0.5,.5))

plot(acm, Trow = FALSE, gg = TRUE, col.col = "black", cex.global = 0.8, cframe = 1.1,
     xlim = c(-0.3,0.3), ylim = c(-0.3,.3))

# Criterio de Benzecri ----------------------------------------------------
s = dim(y)[2]
1/s
eiglst$vp>1/s
eig36 <- acm$eig[1:36]
tau <- (s/(s-1))^2*(eig36-(1/s))^2

ayuda <- inertia.dudi(acm,, T)

names(ayuda)
dim(acm$co[1:36,])
table( cbind(peso = acm$cw*100, acm$co, ayuda$col.abs/100, abs(ayuda$col.rel)/100), 
       digits = c(0,1, rep(3,3), rep(1, 7)))
table(cbind(peso = acm$cw*100, acm$co, ayuda$col.abs/100, abs(ayuda$col.rel)/100))
# no sé que estoy haciendo con mi vida ------------------------------------

# https://rpubs.com/ocamilocardona/813536

library(FactoMineR)
library(ggplot2)
library(FactoClass)
# install.packages(factoextra)
library(factoextra)
library(Rcpp)
library(broom)
# install.packages("pander")
library(pander)
library(corrplot)
library(gridExtra)

uni.mca <- MCA(y, graph = FALSE)
print(uni.mca)

eigenval <- get_eigenvalue(uni.mca)
pander(head(eigenval))

fviz_screeplot(uni.mca, addlabels = TRUE, ylim = c(0, 15)) + 
  geom_hline(yintercept = 7.14, linetype = 2, color = "red")

heatmap.2(as.matrix(matriz_burt), trace = "none", col = bluered(100), margins = c(10, 10))

fviz_mca_biplot(uni.mca, repel = TRUE, 
                ggtheme = theme_grey())+labs(
                  title ="           Representación simultanea de los individuos y las categorías")


var <- get_mca_var(uni.mca)
var

fviz_mca_var(uni.mca, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_grey())
fviz_mca_var(uni.mca, col.var = "purple", shape.var = 10, repel = TRUE,
             ggtheme = theme_grey())+labs(title = "                     Nube de puntos de las Modalidades/Categorías")




# fajlsdfj ----------------------------------------------------------------



acm <- CA(matriz_burt,graph=F);acm
?CA()


y[] <- lapply(y, function(x) if (is.factor(x)) as.character(x) else x)

y[] <- lapply(y, function(x) {
  if (is.na(x)) {
    x <- as.character(x)
  }
  as.factor(x)
})

z <- acm.disjonctif(y)
?acm.disjonctif
acm.burt(y,y)

colSums(is.na(drogas.desc))
rowSums(is.na(drogas.desc))
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



# Mapas -------------------------------------------------------------------

library(stringr)
Municipios <- read_excel("C:/Users/gabop/SEMILLERO-SEA-UN/Descriptivas/Mapas/Municipios.xlsx")
Municipios <- within(Municipios,Depmun <- str_sub(Depmun,1,5))

Municipios <- within(Municipios,{
  Municipio2 <- str_to_lower(Municipio)
  Municipio2 <- str_replace_all(Municipio2,"[^a-záéíóúüñ ]","")
  Municipio2 <- str_squish(Municipio2)
  Municipio2 <- str_to_title(Municipio2)
})
head(Municipios[,c("Municipio","Municipio2")],n=10)

Municipios <- within(Municipios,{
  Municipio <- Municipio2
  rm(Municipio2)
})

Municipios <- within(Municipios,{
  Departamento2 <- str_to_lower(Departamento)
  Departamento2 <- str_replace_all(Departamento2,"[^a-záéíóúüñ ]","")
  Departamento2 <- str_squish(Departamento2)
  Departamento2 <- str_to_title(Departamento2)
})
head(Municipios[,c("Departamento","Departamento2")],n=10)

Municipios <- within(Municipios,{
  Departamento <- Departamento2
  rm(Departamento2)
})

Municipios <- within(Municipios,{
  Region2 <- str_to_lower(Region)
  Region2 <- str_replace_all(Region2,"[^a-záéíóúüñ ]","")
  Region2 <- str_squish(Region2)
  Region2 <- str_to_title(Region2)
})
head(Municipios[,c("Region","Region2")],n=10)

Municipios <- within(Municipios,{
  Region <- Region2
  rm(Region2)
})

Municipios <- within(Municipios,{
  Tipo <- ifelse(str_sub(Depmun,3,5)=="001","Capital","Otro")
  Tipo2 <- ifelse(str_c(Dep,"001")==Depmun,"Capital","Otro")
  if(all(Tipo==Tipo2)) rm(Tipo2)
  Tipo <- ifelse(Depmun=="25001","Otro",Tipo)
})

Municipios <- within(Municipios,{
  denspobl <- Poblacion/Superficie
  Zona <- ifelse(Irural <= 40,"Urbano","Rural")
})


#install.packages("sf")
library(sf)
help(st_read)
deptos <- Municipios %>%
  group_by(Departamento,Dep) %>%
  summarise(Irural=sum(Poblacion*Irural)/sum(Poblacion)) %>%
  as.data.frame()
str(deptos)

deptoshp <- st_read("C:/Users/gabop/SEMILLERO-SEA-UN/Descriptivas/Mapas/MGN_DPTO_POLITICO.shp", quiet=TRUE)

mapdeptos <- deptoshp %>% left_join(deptos,by=c("DPTO_CCDGO"="Dep"))
str(mapdeptos)

mundoshp <- st_read("C:/Users/gabop/SEMILLERO-SEA-UN/Descriptivas/Mapas/admin00.shp",quiet=TRUE)
mundocol <- mundoshp %>% 
  filter(CNTRY_NAME %in% c("Peru","Brazil","Venezuela","Ecuador","Panama"))
str(mundocol)
help(geom_sf)
box <- st_bbox(mapdeptos)
box
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=Irural),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapdeptos,aes(label=ifelse(Irural > 70,Departamento,"")),col="black",
               fontface="bold",size=4,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Índice de\nRuralidad") +
  scale_fill_gradient(low="white",high="red",n.breaks=5) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"))

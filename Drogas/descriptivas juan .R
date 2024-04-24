rm(list=ls())
library("readr")
library("dplyr")
library("stringr")

# CARGA Y LIMPIEZA DE LAS TABLAS ----
## Basicos ----
setwd("C:/Users/jufem/OneDrive/Documentos/Semillero SEA/SEMILLERO-SEA-UN")
personas <- read_csv("Drogas/Datos originales/personas.csv")

personas_seleccionadas <- read_csv("Drogas/Datos originales/personas_seleccionadas.csv")
personas_seleccionadas <- personas_seleccionadas %>%
  select(DIRECTORIO, SEXO, EDAD, PARENTESCO) %>%
  mutate_all(as.character) %>%
  mutate(EDAD = as.integer(EDAD))

d <- read_csv("Drogas/Datos originales/d_capitulos.csv")
d <- d %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`,
         -`D_08`, -`D_09`, -`D_10`) %>%
  mutate_all(as.character) %>%
  # se remplaza por "na" todos los no aplica
  mutate(D_03 = case_when(D_01 == "2" ~  "na", TRUE ~ D_03),
         D_04 = case_when(D_01 == "2" ~  "na", TRUE ~ D_04),
         D_05 = case_when(D_01 == "2" ~  "0", TRUE ~ D_05),
         D_07 = case_when(D_06 == "2" ~  "na", D_06 == "9" ~  "na", TRUE ~ D_07),
         D_12_A_A = case_when(D_12_A == "2" ~  "na", TRUE ~ D_12_A_A),
         D_12_B_A = case_when(D_12_B == "2" ~  "na", TRUE ~ D_12_B_A), 
         D_12_C_A = case_when(D_12_C == "2" ~  "na", TRUE ~ D_12_C_A)) %>%
  mutate(D_05 = as.integer(D_05))
colSums(is.na(d))

d2 <- read_csv("Drogas/Datos originales/d2_capitulos.csv")
d2 <- d2 %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`D2_05_A`) %>%
  mutate_all(as.character) %>%
  mutate(D2_04 = as.integer(D2_04))
colSums(is.na(d2))

encuestas <- read_csv("Drogas/Datos originales/encuestas.csv")
encuestas <- encuestas %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`ESTADO_ENCUESTA`, -`RECO_DIC`) %>%
  mutate_all(as.character) %>%
  mutate(TOTAL_PERSONAS = as.integer(TOTAL_PERSONAS))
colSums(is.na(encuestas))

entorno <- read_csv("Drogas/Datos originales/g_capitulos.csv")
entorno <- entorno %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  # se remplaza por "na" todos los no aplica
  mutate(G_01_A = case_when(G_01 == "2" ~  "na", TRUE ~ G_01_A),
         G_02_A = case_when(G_02 == "2" ~  "na", TRUE ~ G_02_A),
         G_05_A = case_when(G_05 == "2" ~  "na", TRUE ~ G_05_A),
         G_08_A = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_A),
         G_08_B = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_B),
         G_08_C = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_C),
         G_08_D = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_D),
         G_08_E = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_E),
         G_08_F = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_F),
         G_08_G = case_when(G_07 == "2" ~  "na", TRUE ~ G_08_G),
         # se define consumo con que lo haya probado al menos 1 vez en su vida
         Y = case_when(G_11_A == "1" ~ "1", G_11_B == "1" ~ "1", G_11_C == "1" ~ "1", 
                       G_11_D == "1" ~ "1", G_11_E == "1" ~ "1", G_11_F == "1" ~ "1", 
                       G_11_G == "1" ~ "1", G_11_H == "1" ~ "1", G_11_I == "1" ~ "1", 
                       G_11_J == "1" ~ "1", G_11_K == "1" ~ "1", G_11_L == "1" ~ "1", 
                       G_11_M == "1" ~ "1", G_11_N == "1" ~ "1", G_11_O == "1" ~ "1", 
                       G_11_P == "1" ~ "1", G_11_Q == "1" ~ "1", G_11_R == "1" ~ "1", 
                       G_11_S == "1" ~ "1", G_11_T == "1" ~ "1", G_11_U == "1" ~ "1",
                       G_11_V == "1" ~ "1", TRUE ~ "2"))

## Especificos ----
#### Alcohol ----
C_f <- read_csv("Drogas/Datos originales/f_capitulos.csv")
C_f <- C_f %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  filter(F_03 == 1)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


# CREACION DE DATA FRAMES ----

## ID sustancias ----

### legales ----

### ilegales ----
DF_ilicitas <- personas_seleccionadas %>%
  left_join(d,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(entorno,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  select(-`G_11_A`, -`G_11_B`, -`G_11_C`, -`G_11_D`, -`G_11_E`, -`G_11_F`, -`G_11_G`,
         -`G_11_H`, -`G_11_I`, -`G_11_J`, -`G_11_K`, -`G_11_L`, -`G_11_M`, -`G_11_N`, 
         -`G_11_O`, -`G_11_P`, -`G_11_Q`, -`G_11_R`, -`G_11_S`, -`G_11_T`, -`G_11_U`, -`G_11_V`,
         -`G_11_A_ANIOS`, -`G_11_B_ANIOS`, -`G_11_C_ANIOS`, -`G_11_D_ANIOS`, -`G_11_E_ANIOS`, 
         -`G_11_F_ANIOS`, -`G_11_G_ANIOS`, -`G_11_H_ANIOS`, -`G_11_I_ANIOS`, -`G_11_J_ANIOS`,
         -`G_11_K_ANIOS`, -`G_11_L_ANIOS`, -`G_11_M_ANIOS`, -`G_11_N_ANIOS`, -`G_11_O_ANIOS`, 
         -`G_11_P_ANIOS`, -`G_11_Q_ANIOS`, -`G_11_R_ANIOS`, -`G_11_S_ANIOS`, -`G_11_T_ANIOS`,
         -`G_11_U_ANIOS`, -`G_11_V_ANIOS`, -`G_10`,
         -`G_12_A`, -`G_12_B`, -`G_12_C`, -`G_12_D`, -`G_12_E`, -`G_12_F`, -`G_12_G`, -`G_12_H`, -`G_12_I`, -`G_12_J`,
         -`G_13`, -`G_14_A`, -`G_14_B`, -`G_14_C`, -`G_14_D`, -`G_14_E`, -`G_14_F`, -`G_14_G`)

prop.table(table(DF_ilicitas$Y))

#### categóricas mutuamente excluyentes ----
Tipo_consumo <- entorno %>%
  filter(Y == "1") %>%
  select(DIRECTORIO, A = `G_11_A`, B = `G_11_B`, C = `G_11_C`, D = `G_11_D`, 
         E = `G_11_E`, `F` = `G_11_F`, G = `G_11_G`, H = `G_11_H`, I = `G_11_I`,
         J = `G_11_J`, K = `G_11_K`, L = `G_11_L`, M = `G_11_M`, N = `G_11_N`, 
         O = `G_11_O`, P = `G_11_P`, Q = `G_11_Q`, R = `G_11_R`, S = `G_11_S`, 
         `T` = `G_11_T`, U = `G_11_U`, V = `G_11_V`)

# Se cambian las categorías de la variable original para T y F
Tipo_consumo <- Tipo_consumo %>%
  mutate_at(vars(-DIRECTORIO), ~ifelse(. == "1", TRUE, FALSE))

# Función para obtener las categorías de cada individuo
obtener_categorias <- function(fila) {
  categorias <- names(fila)[fila]
  return(paste(categorias, collapse = ", "))
}

categorias_por_individuo <- apply(Tipo_consumo[, -1], 1, obtener_categorias)
categorias_por_individuo_edit <- as.data.frame(cbind(DIRECTORIO =Tipo_consumo$DIRECTORIO,
                                                     categorias = categorias_por_individuo,
                                                     cantidad = str_count(categorias_por_individuo, ",")+1))

categorias_por_individuo_edit <- categorias_por_individuo_edit %>%
  mutate_at(vars(-1), ~gsub(".*G_11_.*", "", .))

categorias <- unique(categorias_por_individuo)
# ACM / cluster ----
library(FactoMineR)
library(factoextra)
library(fastDummies)
Z=(fastDummies::dummy_cols(categorias))[,-1]
dat= prcomp(Z, scale = TRUE)
fviz_contrib(dat, choice = "var", axes = 1, top = 10)
fviz_screeplot(dat, addlabels = TRUE, ylim = c(0, 20))
fviz_pca_var(dat)
#Descriptiva ----
boxplot(Z[,-1],las=2)
library(pyramid)
head(personas_seleccionadas)
tabla<-  table(personas_seleccionadas$SEXO,personas_seleccionadas$EDAD)
datos<-data.frame(Hombres=tabla[2,],Mujeres=tabla[1,] , Edad=colnames(tabla))
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="Edad",main="Consumo")
?pyramid
#Graficos marginales
ggplot(personas_seleccionadas, aes(x=SEXO)) + geom_bar(fill= "#DDB4EB")

personas_seleccionadas$grupo_edad <- cut(personas_seleccionadas$EDAD, breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))

# Graficar por grupos de eedad de 10 años
  library(viridis)
  ggplot(personas_seleccionadas, aes(x = EDAD, fill = grupo_edad)) + 
    geom_bar() +
    scale_fill_manual(values = c(viridis(6))) +
    labs(title = "Distribución de edades", x = "Edad", y = "Frecuencia")+
    theme(legend.position = "bottom")
#Grafico por percentiles de edad
  min_edad <- min(personas_seleccionadas$EDAD, na.rm = TRUE)
  
  # Calcular percentiles de la edad incluyendo el mínimo en el primer percentil
  percentiles_edad <- c(min_edad, quantile(personas_seleccionadas$EDAD, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE))
  # Crear intervalos de edades basados en percentiles
  personas_seleccionadas$percentiles_edad <- cut(personas_seleccionadas$EDAD, breaks = percentiles_edad, labels = c("0-25%", "25-50%", "50-75%", "75-100%"))
  # Graficar
  ggplot(personas_seleccionadas, aes(x = EDAD, fill = percentiles_edad)) + 
    geom_bar() +
    scale_fill_viridis_d(name="percentiles de la edad",option = "magma") +
    labs(title = "Distribución de edades", x = "Edad", y = "Frecuencia")+
    theme(legend.position = "bottom")
  
#  Graficos de parentezco
  # Convertir PARENTESCO a factor y ordenar por frecuencia
  personas_seleccionadas$PARENTESCO <- factor(personas_seleccionadas$PARENTESCO, levels = names(sort(table(personas_seleccionadas$PARENTESCO), decreasing = TRUE)))
  # Trazar el gráfico
  ggplot(personas_seleccionadas, aes(x = PARENTESCO)) + 
    geom_bar(fill = viridis(10)) +
    labs(title = "Frecuencia de Parentesco", x = "Parentesco", y = "Frecuencia")
##################################################################################  
  
#difrencia entre hombres - mujeres y edad significativa?
  #chisq.test(tabla)
#Por drogas
library(ggplot2)
ggplot(categorias_por_individuo_edit[,-1], aes(x = categorias, y = cantidad)) +
  geom_col()+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#Las de valor maximo
max_value <- max(categorias_por_individuo_edit$cantidad)
maxvaal<-categorias_por_individuo_edit[categorias_por_individuo_edit$cantidad == max_value, ]
library(ggplot2)
ggplot(maxvaal[,-1], aes(x = categorias, y = cantidad)) +
  geom_col()+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#########ACP solo de las categorias con valor maximo:
# ACM / cluster ----
library(FactoMineR)
library(factoextra)
library(fastDummies)
Z=(fastDummies::dummy_cols(maxvaal$categorias))[,-1]
dat= CA(Z, graph = TRUE)
fviz_contrib(dat, choice = "var", axes = 1, top = 10)
fviz_screeplot(dat, addlabels = TRUE, ylim = c(0, 20))
fviz_pca_var(dat)
#mac CON EDAD,SEXO Y PARENTESCO:
str(personas_seleccionadas)
Datos<-subset(personas_seleccionadas,select=c("SEXO","PARENTESCO","grupo_edad"))
uni.mca <- MCA(Datos, graph = FALSE)
print(uni.mca)
##EIgenvalues
library(pander)
eigenval <- get_eigenvalue(uni.mca)
pander(head(eigenval))
#Screelot
fviz_screeplot(uni.mca, addlabels = TRUE) + geom_hline(yintercept = 7.14, linetype = 2, color = "red")
#Biplot
fviz_mca_biplot(uni.mca, repel = TRUE, 
                ggtheme = theme_grey())+labs(
                  title ="           Representación simultanea de los individuos y las categorías")

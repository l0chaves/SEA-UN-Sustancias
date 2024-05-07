rm(list=ls())
library("readr")
library("dplyr")
library("stringr")

# CARGA Y LIMPIEZA DE LAS TABLAS ----
## Basicos ----
setwd("C:/Users/jufem/OneDrive/Documentos/Semillero SEA/SEMILLERO-SEA-UNAL_Juan")
personas <- read_csv("Drogas/Datos originales/personas.csv")

personas_seleccionadas <- read_csv("Drogas/Datos originales/personas_seleccionadas.csv")
personas_seleccionadas <- personas_seleccionadas %>%
  select(DIRECTORIO, SEXO, EDAD, PARENTESCO) %>%
  mutate_all(as.character) %>%
  mutate(EDAD = as.integer(EDAD))
personas_seleccionadas$grupo_edad <- cut(personas_seleccionadas$EDAD, breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
#personas_seleccionadas$PARENTESCO <- factor(personas_seleccionadas$PARENTESCO, levels = names(sort(table(personas_seleccionadas$PARENTESCO), decreasing = TRUE)))
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

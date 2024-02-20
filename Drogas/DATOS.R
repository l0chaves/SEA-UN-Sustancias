library("readr")
library("dplyr")

# CARGA Y LIMPIEZA DE LAS TABLAS ----
personas <- read_csv("Drogas/personas.csv")

personas_seleccionadas <- read_csv("Drogas/personas_seleccionadas.csv")
personas_seleccionadas <- personas_seleccionadas %>%
  select(DIRECTORIO, SEXO, EDAD, PARENTESCO) %>%
  mutate_all(as.character) %>%
  mutate(EDAD = as.integer(EDAD))

d <- read_csv("Drogas/d_capitulos.csv")
d <- d %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  mutate(D_03 = case_when(D_01 == "2" ~  "9", TRUE ~ D_03),
         D_04 = case_when(D_01 == "2" ~  "9", TRUE ~ D_04),
         D_05 = case_when(D_01 == "2" ~  "0", TRUE ~ D_05),
         D_07 = case_when(D_06 == "2" ~  "9", D_06 == "9" ~  "9", TRUE ~ D_07),
         D_12_A_A = case_when(D_12_A == "2" ~  "9", TRUE ~ D_12_A_A),
         D_12_B_A = case_when(D_12_B == "2" ~  "9", TRUE ~ D_12_B_A), 
         D_12_C_A = case_when(D_12_C == "2" ~  "9", TRUE ~ D_12_C_A)) %>%
  mutate(D_05 = as.integer(D_05))
colSums(is.na(d))
lapply(d[,-31], table)

d2 <- read_csv("Drogas/d2_capitulos.csv")
d2 <- d2 %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`D2_05_A`) %>%
  mutate_all(as.character) %>%
  mutate(D2_04 = as.integer(D2_04))
colSums(is.na(d2))

encuestas <- read_csv("Drogas/encuestas.csv")
encuestas <- encuestas %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`ESTADO_ENCUESTA`, -`RECO_DIC`) %>%
  mutate_all(as.character) %>%
  mutate(TOTAL_PERSONAS = as.integer(TOTAL_PERSONAS))
colSums(is.na(encuestas))

entorno <- read_csv("Drogas/g_capitulos.csv")
entorno <- entorno %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`,
         -`G_12_A`, -`G_12_B`, -`G_12_C`, -`G_12_D`, -`G_12_E`, -`G_12_F`, -`G_12_H`, -`G_12_I`, -`G_12_J`,
         -`G_13`, -`G_14_A`, -`G_14_B`, -`G_14_C`, -`G_14_D`, -`G_14_E`, -`G_14_F`, -`G_14_G`) %>%
  mutate_all(as.character) %>%
  mutate(G_01_A = case_when(G_01 == "2" ~  "9", TRUE ~ G_01_A),
         G_02_A = case_when(G_02 == "2" ~  "9", TRUE ~ G_02_A),
         G_05_A = case_when(G_05== "2" ~  "9", TRUE ~ G_05_A),
         G_08_A = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_A),
         G_08_B = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_B),
         G_08_C = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_C),
         G_08_D = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_D),
         G_08_E = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_E),
         G_08_F = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_F),
         G_08_G = case_when(G_07 == "2" ~  "9", TRUE ~ G_08_G),
         G_10 = case_when(G_09 == "2" ~  "2", TRUE ~ G_10), # REVISAR
         G_11_A_ANIOS = case_when(G_11_A == "2" ~  "0", TRUE ~ G_11_A_ANIOS),
         G_11_B_ANIOS = case_when(G_11_B == "2" ~  "0", TRUE ~ G_11_B_ANIOS),
         G_11_C_ANIOS = case_when(G_11_C == "2" ~  "0", TRUE ~ G_11_C_ANIOS),
         G_11_D_ANIOS = case_when(G_11_D == "2" ~  "0", TRUE ~ G_11_D_ANIOS),
         G_11_E_ANIOS = case_when(G_11_E == "2" ~  "0", TRUE ~ G_11_E_ANIOS),
         G_11_F_ANIOS = case_when(G_11_F == "2" ~  "0", TRUE ~ G_11_F_ANIOS),
         G_11_G_ANIOS = case_when(G_11_G == "2" ~  "0", TRUE ~ G_11_G_ANIOS),
         G_11_H_ANIOS = case_when(G_11_H == "2" ~  "0", TRUE ~ G_11_H_ANIOS),
         G_11_I_ANIOS = case_when(G_11_I == "2" ~  "0", TRUE ~ G_11_I_ANIOS),
         G_11_J_ANIOS = case_when(G_11_J == "2" ~  "0", TRUE ~ G_11_J_ANIOS),
         G_11_K_ANIOS = case_when(G_11_K == "2" ~  "0", TRUE ~ G_11_K_ANIOS),
         G_11_L_ANIOS = case_when(G_11_L == "2" ~  "0", TRUE ~ G_11_L_ANIOS),
         G_11_M_ANIOS = case_when(G_11_M == "2" ~  "0", TRUE ~ G_11_M_ANIOS),
         G_11_N_ANIOS = case_when(G_11_N == "2" ~  "0", TRUE ~ G_11_N_ANIOS),
         G_11_O_ANIOS = case_when(G_11_O == "2" ~  "0", TRUE ~ G_11_O_ANIOS),
         G_11_P_ANIOS = case_when(G_11_P == "2" ~  "0", TRUE ~ G_11_P_ANIOS),
         G_11_Q_ANIOS = case_when(G_11_Q == "2" ~  "0", TRUE ~ G_11_Q_ANIOS),
         G_11_R_ANIOS = case_when(G_11_R == "2" ~  "0", TRUE ~ G_11_R_ANIOS),
         G_11_S_ANIOS = case_when(G_11_S == "2" ~  "0", TRUE ~ G_11_S_ANIOS),
         G_11_T_ANIOS = case_when(G_11_T == "2" ~  "0", TRUE ~ G_11_T_ANIOS),
         G_11_U_ANIOS = case_when(G_11_U == "2" ~  "0", TRUE ~ G_11_U_ANIOS),
         G_11_V_ANIOS = case_when(G_11_V == "2" ~  "0", TRUE ~ G_11_V_ANIOS),)
colSums(is.na(entorno))

Drogas <- personas_seleccionadas %>%
  left_join(d,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(entorno,by=c("DIRECTORIO"="DIRECTORIO"))

saveRDS(Drogas, file="Drogas.RDS")

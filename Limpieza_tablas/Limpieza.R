library("readr")
library("dplyr")
library("stringr")
library("tidyr")

# CARGA Y LIMPIEZA DE LAS TABLAS ----

## Básicos ----
personas <- read_csv("Datos_originales/personas.csv")

personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
personas_seleccionadas <- personas_seleccionadas %>%
  select(DIRECTORIO, SEXO, EDAD, PARENTESCO) %>%
  mutate_all(as.character) %>%
  mutate(EDAD = as.integer(EDAD))

### Socio-Demográficas ----
d <- read_csv("Datos_originales/d_capitulos.csv")
d <- d %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  # se remplaza por "na" todos los no aplica
  mutate(D_03 = case_when(!(D_02 == "1") ~  "na", TRUE ~ D_03),
         D_04 = case_when(!(D_02 == "1") ~  "na", TRUE ~ D_04),
         D_05 = case_when(!(D_02 == "1") ~  "0", TRUE ~ D_05),
         D_07 = case_when(!(D_06 == "1") ~  "na", TRUE ~ D_07),
         D_12_A_A = case_when(D_12_A == "2" ~  "na", TRUE ~ D_12_A_A),
         D_12_B_A = case_when(D_12_B == "2" ~  "na", TRUE ~ D_12_B_A), 
         D_12_C_A = case_when(D_12_C == "2" ~  "na", TRUE ~ D_12_C_A)) %>%
  mutate(D_05 = as.integer(D_05))
colSums(is.na(d))

d2 <- read_csv("Datos_originales/d2_capitulos.csv")
d2 <- d2 %>%
  mutate_all(as.character) %>%
  left_join(personas_seleccionadas, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`D2_05_A`) %>%
  mutate(D2_04 = as.integer(D2_04),
         EDAD = as.integer(EDAD),
         D2_06 = case_when(!(EDAD > 18) ~  "na", TRUE ~ D2_06),
         D2_07 = case_when(!(EDAD > 18) ~  "na", TRUE ~ D2_07))
colSums(is.na(d2))

encuestas <- read_csv("Datos_originales/encuestas.csv")
encuestas <- encuestas %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`ESTADO_ENCUESTA`, -`RECO_DIC`) %>%
  mutate_all(as.character) %>%
  mutate(TOTAL_PERSONAS = as.integer(TOTAL_PERSONAS))
colSums(is.na(encuestas))

### Entorno ----
entorno <- read_csv("Datos_originales/g_capitulos.csv")
entorno <- entorno %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  #se remplaza por "na" todos los no aplica
  mutate(G_01_A = case_when(G_01 == "2" ~  "na", TRUE ~ G_01_A),
         G_02_A = case_when(G_02 == "2" ~  "na", TRUE ~ G_02_A),
         G_05_A = case_when(G_05 == "2" ~  "na", TRUE ~ G_05_A),
         G_08_A = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_A),
         G_08_B = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_B),
         G_08_C = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_C),
         G_08_D = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_D),
         G_08_E = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_E),
         G_08_F = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_F),
         G_08_G = case_when(!(G_07 == "1") ~ "na", TRUE ~ G_08_G),
         G_10 = case_when(!(G_09 == "1") ~ "na", TRUE ~ G_10)) %>%
  select(`G_01`, `G_01_A`, `G_02`, `G_02_A`, `G_03`, `G_04`, `G_05`, `G_05_A`, 
         `G_06_A`, `G_06_B`, `G_06_C`, `G_06_D`, `G_06_E`, `G_06_F`, `G_06_G`, 
         `G_06_H`, `G_06_I`, `G_06_J`, `G_06_K`, `G_06_L`, `G_06_M`, `G_06_N`,
         `G_07`, `G_08_A`, `G_08_B`, `G_08_C`, `G_08_D`, `G_08_E`, `G_08_F`, 
         `G_08_G`, `G_09`, `G_10`, `DIRECTORIO`)

colSums(is.na(entorno))

### Tratamiento ----
tratamiento <- read_csv("Datos_originales/q_capitulos.csv")
tratamiento <- tratamiento %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`Q_01_E`) %>%
  mutate_all(as.character) %>%
  mutate(Q_01_A = replace_na(Q_01_A, "2"),
         Q_01_B = replace_na(Q_01_B, "2"),
         Q_01_C = replace_na(Q_01_C, "2"),
         Q_01_D = replace_na(Q_01_D, "2"),
         Q_03 = case_when(!(Q_02 == "1") ~ "na", TRUE ~ Q_03),
         Q_04 = case_when(!(Q_03 == "1" | Q_03 == "2" | Q_03 == "3") ~ "na", TRUE ~ Q_04))

colSums(is.na(tratamiento))

### Trabajo ----
trabajo <- read_csv("Datos_originales/r_capitulos.csv")
trabajo <- trabajo %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  mutate(R_02 = case_when(!(R_01 == "1") ~ "na", TRUE ~ R_02),
         R_04 = case_when((R_03 == "0") ~ "na", TRUE ~ R_04))
colSums(is.na(trabajo))

# ---------------------------------------------------------------------------- #

## Específicos ----
#### E - Tabaco ----
C_e <- read_csv("Datos_originales/e_capitulos.csv")
C_e <- C_e %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  #Solo se tienen en cuenta los que han fumado cigarrillos o cigarrillos electrónicos
  filter(E_01 == "1" | E_10 == "1") %>%
  #se remplaza por "na" todos los no aplica
  #para que la variable quede numérica se pone en vez de "na", no hay 0 en la variable original.
  mutate(E_02 = case_when(!(E_01 == "1") ~ "0", TRUE ~ E_02),  #num
         E_03 = case_when(!(E_01 == "1") ~ "na", TRUE ~ E_03),
         E_04 = case_when(!(E_01 == "1") ~ "na", TRUE ~ E_04),
         E_05 = case_when(!(E_04 == "1") ~ "na", TRUE ~ E_05),
         E_06 = case_when(!(E_05 == "1") ~ "0", TRUE ~ E_06),  #num 
         E_07 = case_when(!(E_05 == "1") ~ "na", TRUE ~ E_07),
         E_08 = case_when(!(E_07 == "1") ~ "na", TRUE ~ E_08),
         E_09 = case_when(!(E_08 == "1") ~ "0", TRUE ~ E_09),  #num 
         E_11 = case_when(!(E_10 == "1") ~ "0", TRUE ~ E_11),  #num 
         E_12 = case_when(!(E_10 == "1") ~ "na", TRUE ~ E_12)) %>%
  mutate(E_02 = as.integer(E_02),
         E_06 = as.integer(E_06),
         E_09 = as.integer(E_09),
         E_11 = as.integer(E_11))

colSums(is.na(C_e))

#### F - Alcohol ----
C_f <- read_csv("Datos_originales/f_capitulos.csv")
C_f <- C_f %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  #se toman en cuenta solo los que han tomado alcohol
  filter(F_03 == "1") %>%
  #se remplaza por "na" todos los no aplica
  mutate(F_01_CUAL = case_when(F_01 == "2" ~  "na", TRUE ~ F_01_CUAL),
         F_02_CUAL = case_when(F_02 == "2" ~  "na", TRUE ~ F_02_CUAL),
         F_07 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_07),
         #para que la variable quede numérica se pone en vez de "na", no hay 0 en la variable original.
         F_08 = case_when(!(F_07 == "1") ~ "0", TRUE ~ F_08), #num 
         #Hay 0 en la variable original.
         F_09 = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_09), #num 
         F_10_A = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_A),
         F_10_B = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_B),
         F_10_C = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_C),
         F_10_D = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_D),
         F_10_E = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_E),
         F_10_F = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_F),
         F_10_G = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_G),
         F_10_H = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_H),
         F_10_I = case_when(!(F_07 == "1") ~ "na", TRUE ~ F_10_I),
         F_11 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_11),
         F_12 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_12),
         F_13 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_13),
         F_14 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_14),
         F_15 = case_when(!(F_06 == "1") ~ "na", F_14 == "1" ~  "na", TRUE ~ F_15),
         F_16 = case_when(!(F_06 == "1") ~ "na", F_14 == "1" ~  "na", TRUE ~ F_16),
         F_17 = case_when(!(F_06 == "1") ~ "na", F_14 == "1" ~  "na", TRUE ~ F_17),
         F_18 = case_when(!(F_06 == "1") ~ "na", F_14 == "1" ~  "na", TRUE ~ F_18),
         F_19 = case_when(!(F_06 == "1") ~ "na", F_14 == "1" ~  "na", TRUE ~ F_19),
         F_20 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_20),
         F_20_CUAL = case_when(!(F_20 == "1") ~  "na", TRUE ~ F_20_CUAL),
         F_21 = case_when(!(F_06 == "1") ~ "na", TRUE ~ F_21),
         F_21_CUAL = case_when(!(F_21 == "1") ~  "na", TRUE ~ F_21_CUAL)) %>%
  mutate(F_04 = as.integer(F_04),
         F_08 = as.integer(F_08))
  
colSums(is.na(C_f))

#### G - Sustancias Psicoactivas ----
C_g <- read_csv("Datos_originales/g_capitulos.csv")
C_g <- C_g %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, 
         -`G_01`, -`G_01_A`, -`G_02`, -`G_02_A`, -`G_03`, -`G_04`, -`G_05`, -`G_05_A`, 
         -`G_06_A`, -`G_06_B`, -`G_06_C`, -`G_06_D`, -`G_06_E`, -`G_06_F`, -`G_06_G`, 
         -`G_06_H`, -`G_06_I`, -`G_06_J`, -`G_06_K`, -`G_06_L`, -`G_06_M`, -`G_06_N`,
         -`G_07`, -`G_08_A`, -`G_08_B`, -`G_08_C`, -`G_08_D`, -`G_08_E`, -`G_08_F`, 
         -`G_08_G`, -`G_09`, -`G_10`) %>%
  mutate_all(as.character) %>%
  # se une el alcohol para responder las preguntas G_12 a G_14
  left_join(C_f[, c("DIRECTORIO", "F_03")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  #"Y" mira que lo haya probado al menos una vez en la vida: "1" si, "2" no. 
  mutate(Y = case_when(G_11_A == "1" ~ "1", G_11_B == "1" ~ "1", G_11_C == "1" ~ "1", 
                        G_11_D == "1" ~ "1", G_11_E == "1" ~ "1", G_11_F == "1" ~ "1", 
                        G_11_G == "1" ~ "1", G_11_H == "1" ~ "1", G_11_I == "1" ~ "1", 
                        G_11_J == "1" ~ "1", G_11_K == "1" ~ "1", G_11_L == "1" ~ "1", 
                        G_11_M == "1" ~ "1", G_11_N == "1" ~ "1", G_11_O == "1" ~ "1", 
                        G_11_P == "1" ~ "1", G_11_Q == "1" ~ "1", G_11_R == "1" ~ "1", 
                        G_11_S == "1" ~ "1", G_11_T == "1" ~ "1", G_11_U == "1" ~ "1",
                        G_11_V == "1" ~ "1", TRUE ~ "2"),
         #para que la variable quede numérica se pone en vez de "na", no hay 0 en la variable original.
          G_11_A_ANIOS = case_when(G_11_A == "2" ~ "0", TRUE ~ G_11_A_ANIOS),
          G_11_B_ANIOS = case_when(G_11_B == "2" ~ "0", TRUE ~ G_11_B_ANIOS),
          G_11_C_ANIOS = case_when(G_11_C == "2" ~ "0", TRUE ~ G_11_C_ANIOS),
          G_11_D_ANIOS = case_when(G_11_D == "2" ~ "0", TRUE ~ G_11_D_ANIOS),
          G_11_E_ANIOS = case_when(G_11_E == "2" ~ "0", TRUE ~ G_11_E_ANIOS),
          G_11_F_ANIOS = case_when(G_11_F == "2" ~ "0", TRUE ~ G_11_F_ANIOS),
          G_11_G_ANIOS = case_when(G_11_G == "2" ~ "0", TRUE ~ G_11_G_ANIOS),
          G_11_H_ANIOS = case_when(G_11_H == "2" ~ "0", TRUE ~ G_11_H_ANIOS),
          G_11_I_ANIOS = case_when(G_11_I == "2" ~ "0", TRUE ~ G_11_I_ANIOS),
          G_11_J_ANIOS = case_when(G_11_J == "2" ~ "0", TRUE ~ G_11_J_ANIOS),
          G_11_K_ANIOS = case_when(G_11_K == "2" ~ "0", TRUE ~ G_11_K_ANIOS),
          G_11_L_ANIOS = case_when(G_11_L == "2" ~ "0", TRUE ~ G_11_L_ANIOS),
          G_11_M_ANIOS = case_when(G_11_M == "2" ~ "0", TRUE ~ G_11_M_ANIOS),
          G_11_N_ANIOS = case_when(G_11_N == "2" ~ "0", TRUE ~ G_11_N_ANIOS),
          G_11_O_ANIOS = case_when(G_11_O == "2" ~ "0", TRUE ~ G_11_O_ANIOS),
          G_11_P_ANIOS = case_when(G_11_P == "2" ~ "0", TRUE ~ G_11_P_ANIOS),
          G_11_Q_ANIOS = case_when(G_11_Q == "2" ~ "0", TRUE ~ G_11_Q_ANIOS),
          G_11_R_ANIOS = case_when(G_11_R == "2" ~ "0", TRUE ~ G_11_R_ANIOS),
          G_11_S_ANIOS = case_when(G_11_S == "2" ~ "0", TRUE ~ G_11_S_ANIOS),
          G_11_T_ANIOS = case_when(G_11_T == "2" ~ "0", TRUE ~ G_11_T_ANIOS),
          G_11_U_ANIOS = case_when(G_11_U == "2" ~ "0", TRUE ~ G_11_U_ANIOS),
          G_11_V_ANIOS = case_when(G_11_V == "2" ~ "0", TRUE ~ G_11_V_ANIOS),
          G_12_A = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_A),
          G_12_B = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_B),
          G_12_C = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_C),
          G_12_D = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_D),
          G_12_E = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_E),
          G_12_F = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_F),
          G_12_G = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_G),
          G_12_H = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_H),
          G_12_I = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_I),
          G_12_J = case_when(Y == "2" ~ "na", !(F_03 == "1") ~ "na", TRUE ~ G_12_J), 
          G_13 = case_when(!(G_12_A == "1"|G_12_B == "1"|G_12_C == "1"|G_12_D == "1"|
                             G_12_E == "1"|G_12_F == "1"|G_12_G == "1"|G_12_H == "1"|
                             G_12_I == "1"|G_12_J == "1") ~ "na", TRUE ~ G_13),
          G_14_A = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_A),
          G_14_B = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_B),
          G_14_C = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_C),
          G_14_D = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_D),
          G_14_E = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_E),
          G_14_F = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_F),
          G_14_G = case_when(!(G_12_A == "1" | G_12_B == "1" | G_12_C == "1" | G_12_D == "1" | G_12_E == "1" | G_12_F == "1" | G_12_G == "1") ~ "na", TRUE ~ G_14_G)) %>%
  mutate(G_11_A_ANIOS = as.integer(G_11_A_ANIOS), G_11_B_ANIOS = as.integer(G_11_B_ANIOS),
         G_11_C_ANIOS = as.integer(G_11_C_ANIOS), G_11_D_ANIOS = as.integer(G_11_D_ANIOS),
         G_11_E_ANIOS = as.integer(G_11_E_ANIOS), G_11_F_ANIOS = as.integer(G_11_F_ANIOS),
         G_11_G_ANIOS = as.integer(G_11_G_ANIOS), G_11_H_ANIOS = as.integer(G_11_H_ANIOS),
         G_11_I_ANIOS = as.integer(G_11_I_ANIOS), G_11_J_ANIOS = as.integer(G_11_J_ANIOS),
         G_11_K_ANIOS = as.integer(G_11_K_ANIOS), G_11_L_ANIOS = as.integer(G_11_L_ANIOS),
         G_11_M_ANIOS = as.integer(G_11_M_ANIOS), G_11_N_ANIOS = as.integer(G_11_N_ANIOS),
         G_11_O_ANIOS = as.integer(G_11_O_ANIOS), G_11_P_ANIOS = as.integer(G_11_P_ANIOS),
         G_11_Q_ANIOS = as.integer(G_11_Q_ANIOS), G_11_R_ANIOS = as.integer(G_11_R_ANIOS),
         G_11_S_ANIOS = as.integer(G_11_S_ANIOS), G_11_T_ANIOS = as.integer(G_11_T_ANIOS),
         G_11_U_ANIOS = as.integer(G_11_U_ANIOS), G_11_V_ANIOS = as.integer(G_11_V_ANIOS)) %>%
  select(-`F_03`)

colSums(is.na(C_g))


#### H - Tranquilizantes ----
table(C_g$G_11_A) 

C_h <- read_csv("Datos_originales/h_capitulos.csv")
C_h <- C_h %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  # Solo se marca cuando respondieron "1":si se pone "2" para no
  mutate(H_02_A = replace_na(H_02_A, "2"),
         H_02_B = replace_na(H_02_B, "2"),
         H_02_C = replace_na(H_02_C, "2"),
         H_02_D = replace_na(H_02_D, "2"),
         H_02_E = replace_na(H_02_E, "2"),
         H_02_F = replace_na(H_02_F, "2"),
         H_02_G = replace_na(H_02_G, "2"),
         H_02_H = replace_na(H_02_H, "2"),
         H_02_I = replace_na(H_02_I, "2"),
         H_04 = case_when(!(H_03 == "1") ~ "na", TRUE ~ H_04),
         H_05 = case_when(!(H_04 == "1") ~ "0", TRUE ~ H_05), #num
         # Solo se marca cuando respondieron "1":si se pone "2" para no
         H_06_A = case_when(!(H_04 == "1") ~ "na", is.na(H_06_A) ~ "2", TRUE ~ H_06_A),
         H_06_B = case_when(!(H_04 == "1") ~ "na", is.na(H_06_B) ~ "2", TRUE ~ H_06_B),
         H_06_C = case_when(!(H_04 == "1") ~ "na", is.na(H_06_C) ~ "2", TRUE ~ H_06_C),
         H_06_D = case_when(!(H_04 == "1") ~ "na", is.na(H_06_D) ~ "2", TRUE ~ H_06_D),
         H_06_E = case_when(!(H_04 == "1") ~ "na", is.na(H_06_E) ~ "2", TRUE ~ H_06_E),
         H_06_F = case_when(!(H_04 == "1") ~ "na", is.na(H_06_F) ~ "2", TRUE ~ H_06_F),
         H_06_G = case_when(!(H_04 == "1") ~ "na", is.na(H_06_G) ~ "2", TRUE ~ H_06_G),
         H_06_H = case_when(!(H_04 == "1") ~ "na", is.na(H_06_H) ~ "2", TRUE ~ H_06_H),
         H_06_I = case_when(!(H_04 == "1") ~ "na", is.na(H_06_I) ~ "2", TRUE ~ H_06_I),
         H_06_J = case_when(!(H_04 == "1") ~ "na", is.na(H_06_J) ~ "2", TRUE ~ H_06_J),
         H_06_K = case_when(!(H_04 == "1") ~ "na", is.na(H_06_K) ~ "2", TRUE ~ H_06_K),
         H_07 = case_when(!(H_04 == "1") ~ "na", TRUE ~ H_07),
         H_07_A = case_when(!(H_07 == "1") ~ "na", TRUE ~ H_07_A),
         H_07_B = case_when(!(H_07 == "1") ~ "na", TRUE ~ H_07_B),
         H_07_C = case_when(!(H_07 == "1") ~ "na", TRUE ~ H_07_C),
         H_07_D = case_when(!(H_07 == "1") ~ "na", TRUE ~ H_07_D),
         H_07_E = case_when(!(H_07 == "1") ~ "na", TRUE ~ H_07_E)) %>%
  mutate(H_05 = as.integer(H_05))

colSums(is.na(C_h))

#### I - Estimulantes  ----
table(C_g$G_11_B)

C_i <- read_csv("Datos_originales/i_capitulos.csv")
C_i <- C_i %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  # Solo se marca cuando respondieron "1":si se pone "2" para no
  mutate(I_02_A = replace_na(I_02_A, "2"),
         I_02_B = replace_na(I_02_B, "2"),
         I_02_C = replace_na(I_02_C, "2"),
         I_02_D = replace_na(I_02_D, "2"),
         I_02_E = replace_na(I_02_E, "2"),
         I_02_F = replace_na(I_02_F, "2"),
         I_02_G = replace_na(I_02_G, "2"),
         I_02_H = replace_na(I_02_H, "2"),
         I_02_I = replace_na(I_02_I, "2"),
         I_04 = case_when(!(I_03 == "1") ~ "na", TRUE ~ I_04),
         I_05 = case_when(!(I_04 == "1") ~ "0", TRUE ~ I_05), #num
         I_06_A = case_when(!(I_04 == "1") ~ "na", is.na(I_06_A) ~ "2", TRUE ~ I_06_A),
         I_06_B = case_when(!(I_04 == "1") ~ "na", is.na(I_06_B) ~ "2", TRUE ~ I_06_B),
         I_06_C = case_when(!(I_04 == "1") ~ "na", is.na(I_06_C) ~ "2", TRUE ~ I_06_C),
         I_07 = case_when(!(I_04 == "1") ~ "na", TRUE ~ I_07),
         I_07_A = case_when(!(I_07 == "1") ~ "na", is.na(I_07_A) ~ "2", TRUE ~ I_07_A),
         I_07_B = case_when(!(I_07 == "1") ~ "na", is.na(I_07_B) ~ "2", TRUE ~ I_07_B),
         I_07_C = case_when(!(I_07 == "1") ~ "na", is.na(I_07_C) ~ "2", TRUE ~ I_07_C),
         I_07_D = case_when(!(I_07 == "1") ~ "na", is.na(I_07_D) ~ "2", TRUE ~ I_07_D),
         I_07_E = case_when(!(I_07 == "1") ~ "na", is.na(I_07_E) ~ "2", TRUE ~ I_07_E)) %>%
  mutate(I_05 = as.integer(I_05))
  
colSums(is.na(C_i))

#### J - Inhalables  ----
table(C_g$G_11_C) #Todos (1)
table(C_g$G_11_D) #Dick (2)
table(C_g$G_11_E) #Popper (3)

C_j <- read_csv("Datos_originales/j_capitulos.csv")
colSums(is.na(C_j))

C_j_1 <- C_j %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`J_07`, -`J_08`, -`J_09`, -`J_10`, -`J_11`, -`J_12`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_C")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_C == "1") %>%
  mutate(J_03 = case_when(!(J_02 == "1") ~ "na", TRUE ~ J_03),
         J_04 = case_when(!(J_02 == "1") ~ "na", TRUE ~ J_04),
         J_05_A = case_when(!(J_02 == "1") ~ "na", is.na(J_05_A) ~ "2", TRUE ~ J_05_A),
         J_05_B = case_when(!(J_02 == "1") ~ "na", is.na(J_05_B) ~ "2", TRUE ~ J_05_B),
         J_05_C = case_when(!(J_02 == "1") ~ "na", is.na(J_05_C) ~ "2", TRUE ~ J_05_C),
         J_05_D = case_when(!(J_02 == "1") ~ "na", is.na(J_05_D) ~ "2", TRUE ~ J_05_D),
         J_05_E = case_when(!(J_02 == "1") ~ "na", is.na(J_05_E) ~ "2", TRUE ~ J_05_E),
         J_05_F = case_when(!(J_02 == "1") ~ "na", is.na(J_05_F) ~ "2", TRUE ~ J_05_F),
         J_06_A = case_when(!(J_02 == "1") ~ "na", is.na(J_06_A) ~ "2", TRUE ~ J_06_A),
         J_06_B = case_when(!(J_02 == "1") ~ "na", is.na(J_06_B) ~ "2", TRUE ~ J_06_B),
         J_06_C = case_when(!(J_02 == "1") ~ "na", is.na(J_06_C) ~ "2", TRUE ~ J_06_C),
         J_06_D = case_when(!(J_02 == "1") ~ "na", is.na(J_06_D) ~ "2", TRUE ~ J_06_D),
         J_06_E = case_when(!(J_02 == "1") ~ "na", is.na(J_06_E) ~ "2", TRUE ~ J_06_E),
         J_06_F = case_when(!(J_02 == "1") ~ "na", is.na(J_06_F) ~ "2", TRUE ~ J_06_F),
         J_06_G = case_when(!(J_02 == "1") ~ "na", is.na(J_06_G) ~ "2", TRUE ~ J_06_G),
         J_06_H = case_when(!(J_02 == "1") ~ "na", is.na(J_06_H) ~ "2", TRUE ~ J_06_H),
         J_06_I = case_when(!(J_02 == "1") ~ "na", is.na(J_06_I) ~ "2", TRUE ~ J_06_I)) %>%
  select(-`G_11_C`)
colSums(is.na(C_j_1))

C_j_2 <- C_j %>%
  select(`J_07`, `J_08`, `J_09`, `J_10`, `DIRECTORIO`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_D")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_D == "1") %>%
  mutate(J_09 = case_when(!(J_08 == "1") ~ "na", TRUE ~ J_09),
         J_10 = case_when(!(J_08 == "1") ~ "na", TRUE ~ J_10)) %>%
  select(-`G_11_D`)

colSums(is.na(C_j_2))

C_j_3 <- C_j %>%
  select(`J_11`, `J_12`, `DIRECTORIO`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_E")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_E == "1") %>%
  mutate(J_12 = case_when(!(J_11 == "1") ~ "na", TRUE ~ J_12)) %>%
  select(-`G_11_E`)

colSums(is.na(C_j_3))

rm(C_j)
#### K - Marihuana  ----
table(C_g$G_11_F)

C_k <- read_csv("Datos_originales/k_capitulos.csv")
C_k <- C_k %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  # Solo se marca cuando respondieron "1":si se pone "2" para no
  mutate(K_01_A = replace_na(K_01_A, "2"),
         K_01_B = replace_na(K_01_B, "2"),
         K_01_C = replace_na(K_01_C, "2"),
         K_01_D = replace_na(K_01_D, "2"),
         K_04 = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_04),
         K_05 = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_05), 
         K_06 = case_when(!(K_05 == "1") ~ "0", TRUE ~ K_06), #num
         K_07 = case_when(!(K_05 == "1") ~ "0", TRUE ~ K_07), #num
         K_08 = case_when(!(K_03 == "1") ~ "0", TRUE ~ K_08), #num
         K_09 = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_09),
         K_09_VALOR = case_when(!(K_09 == "1") ~ "0", TRUE ~ K_09_VALOR), #num
         K_09_TEXTO = case_when(!(K_09 == "1") ~ "na", TRUE ~ K_09_TEXTO),
         K_10_A = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_A),
         K_10_B = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_B),
         K_10_C = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_C),
         K_10_D = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_D),
         K_10_E = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_E),
         K_10_F = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_F),
         K_10_G = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_G),
         K_10_H = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_H),
         K_10_I = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_10_I),
         K_10_A = replace_na(K_10_A, "2"),
         K_10_B = replace_na(K_10_B, "2"),
         K_10_C = replace_na(K_10_C, "2"),
         K_10_D = replace_na(K_10_D, "2"),
         K_10_E = replace_na(K_10_E, "2"),
         K_10_F = replace_na(K_10_F, "2"),
         K_10_G = replace_na(K_10_G, "2"),
         K_10_H = replace_na(K_10_H, "2"),
         K_10_I = replace_na(K_10_I, "2"),
         K_11 = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_11),
         K_11_A = case_when(!(K_11 == "1") ~ "na", TRUE ~ K_11_A),
         K_11_B = case_when(!(K_11 == "1") ~ "na", TRUE ~ K_11_B),
         K_11_C = case_when(!(K_11 == "1") ~ "na", TRUE ~ K_11_C),
         K_11_D = case_when(!(K_11 == "1") ~ "na", TRUE ~ K_11_D),
         K_11_E = case_when(!(K_11 == "1") ~ "na", TRUE ~ K_11_E),
         K_11_A = replace_na(K_11_A, "2"),
         K_11_B = replace_na(K_11_B, "2"),
         K_11_C = replace_na(K_11_C, "2"),
         K_11_D = replace_na(K_11_D, "2"),
         K_11_E = replace_na(K_11_E, "2"),
         K_12_A = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_A),
         K_12_B = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_B),
         K_12_C = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_C),
         K_12_D = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_D),
         K_12_E = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_E),
         K_12_F = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_F),
         K_12_G = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_G),
         K_12_H = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_H),
         K_12_I = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_I),
         K_12_J = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_J),
         K_12_K = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_K),
         K_12_L = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_L),
         K_12_M = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_M),
         K_12_N = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_N),
         K_12_O = case_when(!(K_03 == "1") ~ "na", TRUE ~ K_12_O)) %>%
  mutate(K_06 = as.numeric(K_06),
         K_07 = as.numeric(K_07),
         K_08 = as.numeric(K_08),
         K_09_VALOR = as.numeric(K_09_VALOR))
    
colSums(is.na(C_k))

#### L - Cocaína  ----
table(C_g$G_11_G)

C_l <- read_csv("Datos_originales/l_capitulos.csv")
C_l <- C_l %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  mutate(L_03 = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_03),
         L_04 = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_04),
         L_05 = case_when(!(L_04 == "1") ~ "0", TRUE ~ L_05), #num
         L_06 = case_when(!(L_04 == "1") ~ "0", TRUE ~ L_06), #num
         L_07 = case_when(!(L_02 == "1") ~ "0", TRUE ~ L_07), #num
         L_08 = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_08),
         L_08_VALOR = case_when(!(L_08 == "1") ~ "0", TRUE ~ L_08_VALOR), #num
         L_08_TEXTO = case_when(!(L_08 == "1") ~ "na", TRUE ~ L_08_TEXTO),
         L_08 = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_08),
         L_09_A = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_A),
         L_09_B = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_B),
         L_09_C = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_C),
         L_09_D = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_D),
         L_09_E = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_E),
         L_09_F = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_F),
         L_09_G = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_G),
         L_09_H = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_H),
         L_09_I = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_09_I),
         L_09_A = replace_na(L_09_A, "2"),
         L_09_B = replace_na(L_09_B, "2"),
         L_09_C = replace_na(L_09_C, "2"),
         L_09_D = replace_na(L_09_D, "2"),
         L_09_E = replace_na(L_09_E, "2"),
         L_09_F = replace_na(L_09_F, "2"),
         L_09_G = replace_na(L_09_G, "2"),
         L_09_H = replace_na(L_09_H, "2"),
         L_09_I = replace_na(L_09_I, "2"),
         L_10 = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_10),
         L_10_A = case_when(!(L_10 == "1") ~ "na", TRUE ~ L_10_A),
         L_10_B = case_when(!(L_10 == "1") ~ "na", TRUE ~ L_10_B),
         L_10_A = replace_na(L_10_A, "2"),
         L_10_B = replace_na(L_10_B, "2"),
         L_11_A = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_A),
         L_11_B = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_B),
         L_11_C = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_C),
         L_11_D = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_D),
         L_11_E = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_E),
         L_11_F = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_F),
         L_11_G = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_G),
         L_11_H = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_H),
         L_11_I = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_I),
         L_11_J = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_J),
         L_11_K = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_K),
         L_11_L = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_L),
         L_11_M = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_M),
         L_11_N = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_N),
         L_11_O = case_when(!(L_02 == "1") ~ "na", TRUE ~ L_11_O)) %>%
  mutate(L_05 = as.integer(L_05),
         L_06 = as.integer(L_06),
         L_07 = as.integer(L_07),
         L_08_VALOR = as.integer(L_08_VALOR))

colSums(is.na(C_l))

#### M - Basuco  ----
table(C_g$G_11_H)

C_m <- read_csv("Datos_originales/m_capitulos.csv")
C_m <- C_m %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  mutate(M_03 = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_03),
         M_04 = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_04),
         M_05 = case_when(!(M_04 == "1") ~ "0", TRUE ~ M_05), #num
         M_06 = case_when(!(M_04 == "1") ~ "0", TRUE ~ M_06), #num
         M_07 = case_when(!(M_02 == "1") ~ "0", TRUE ~ M_07), #num
         M_08 = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_08),
         M_08_VALOR = case_when(!(M_08 == "1") ~ "0", TRUE ~ M_08_VALOR), #num
         M_08_TEXTO = case_when(!(M_08 == "1") ~ "na", TRUE ~ M_08_TEXTO),
         M_09_A = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_A),
         M_09_B = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_B),
         M_09_C = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_C),
         M_09_D = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_D),
         M_09_E = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_E),
         M_09_F = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_F),
         M_09_G = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_G),
         M_09_H = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_H),
         M_09_I = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_09_I),
         M_09_A = replace_na(M_09_A, "2"),
         M_09_B = replace_na(M_09_B, "2"),
         M_09_C = replace_na(M_09_C, "2"),
         M_09_D = replace_na(M_09_D, "2"),
         M_09_E = replace_na(M_09_E, "2"),
         M_09_F = replace_na(M_09_F, "2"),
         M_09_G = replace_na(M_09_G, "2"),
         M_09_H = replace_na(M_09_H, "2"),
         M_09_I = replace_na(M_09_I, "2"),
         M_10_A = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_A),
         M_10_B = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_B),
         M_10_C = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_C),
         M_10_D = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_D),
         M_10_E = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_E),
         M_10_F = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_F),
         M_10_G = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_G),
         M_10_H = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_H),
         M_10_I = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_I),
         M_10_J = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_J),
         M_10_K = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_K),
         M_10_L = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_L),
         M_10_M = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_M),
         M_10_N = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_N),
         M_10_O = case_when(!(M_02 == "1") ~ "na", TRUE ~ M_10_O)) %>%
  mutate(M_05 = as.integer(M_05),
         M_06 = as.integer(M_06),
         M_07 = as.integer(M_07),
         M_08_VALOR = as.integer(M_08_VALOR))

colSums(is.na(C_m))

#### N - Éxtasis  ----
table(C_g$G_11_I)

C_n <- read_csv("Datos_originales/n_capitulos.csv")
C_n <- C_n %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  mutate(N_03 = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_03),
         N_04 = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_04),
         N_05 = case_when(!(N_04 == "1") ~ "0", TRUE ~ N_05), #num
         N_06_A = case_when(!(N_04 == "1") ~ "na", TRUE ~ N_06_A),
         N_06_B = case_when(!(N_04 == "1") ~ "na", TRUE ~ N_06_B),
         N_06_C = case_when(!(N_04 == "1") ~ "na", TRUE ~ N_06_C),
         N_06_A = replace_na(N_06_A, "2"),
         N_06_B = replace_na(N_06_B, "2"),
         N_06_C = replace_na(N_06_C, "2"),
         N_07 = case_when(!(N_06_A == "1") ~ "0", TRUE ~ N_07), #num
         N_08 = case_when(!(N_04 == "1") ~ "na", TRUE ~ N_08),
         N_09 = case_when(!(N_02 == "1") ~ "0", TRUE ~ N_09), #num
         N_09_TEXTO = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_09_TEXTO),
         N_10_A = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_A),
         N_10_B = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_B),
         N_10_C = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_C),
         N_10_D = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_D),
         N_10_E = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_E),
         N_10_F = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_F),
         N_10_G = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_G),
         N_10_H = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_H),
         N_10_I = case_when(!(N_02 == "1") ~ "na", TRUE ~ N_10_I),
         N_10_A = replace_na(N_10_A, "2"),
         N_10_B = replace_na(N_10_B, "2"),
         N_10_C = replace_na(N_10_C, "2"),
         N_10_D = replace_na(N_10_D, "2"),
         N_10_E = replace_na(N_10_E, "2"),
         N_10_F = replace_na(N_10_F, "2"),
         N_10_G = replace_na(N_10_G, "2"),
         N_10_H = replace_na(N_10_H, "2"),
         N_10_I = replace_na(N_10_I, "2")) %>%
  mutate(N_05 = as.numeric(N_05),
         N_07 = as.numeric(N_07),
         N_09 = as.numeric(N_09))

colSums(is.na(C_n))

#### O - Heroína  ----
table(C_g$G_11_J)

C_o <- read_csv("Datos_originales/o_capitulos.csv")
C_o <- C_o %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  mutate(O_03 = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_03),
         O_04_A = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_04_A),
         O_04_A = replace_na(O_04_A, "2"),
         O_04_B = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_04_B),
         O_04_B = replace_na(O_04_B, "2"),
         O_04_C = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_04_C),
         O_04_C = replace_na(O_04_C, "2"),
         O_04_D = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_04_D),
         O_04_D = replace_na(O_04_D, "2"),
         O_04_FUMADA = case_when(!(O_04_A == "1") ~ "na", TRUE ~ O_04_FUMADA),
         O_05 = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_05),
         O_06 = case_when(!(O_02 == "1") ~ "0", TRUE ~ O_06), #num
         O_07 = case_when(!(O_02 == "1") ~ "0", TRUE ~ O_07), #num
         O_08 = case_when(!(O_02 == "1") ~ "0", TRUE ~ O_08), #num
         O_09 = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_09),
         O_09_VALOR = case_when(!(O_09 == "1") ~ "0", TRUE ~ O_09_VALOR), #num
         O_09_TEXTO = case_when(!(O_09 == "1") ~ "na", TRUE ~ O_09_TEXTO),
         O_10_A = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_A),
         O_10_B = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_B),
         O_10_C = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_C),
         O_10_D = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_D),
         O_10_E = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_E),
         O_10_F = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_F),
         O_10_G = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_G),
         O_10_H = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_H),
         O_10_I = case_when(!(O_02 == "1") ~ "na", TRUE ~ O_10_I),
         O_10_A = replace_na(O_10_A, "2"),
         O_10_B = replace_na(O_10_B, "2"),
         O_10_C = replace_na(O_10_C, "2"),
         O_10_D = replace_na(O_10_D, "2"),
         O_10_E = replace_na(O_10_E, "2"),
         O_10_F = replace_na(O_10_F, "2"),
         O_10_G = replace_na(O_10_G, "2"),
         O_10_H = replace_na(O_10_H, "2"),
         O_10_I = replace_na(O_10_I, "2")) %>%
  mutate(O_06 = as.numeric(O_06),
         O_07 = as.numeric(O_07),
         O_08 = as.numeric(O_08),
         O_09_VALOR = as.numeric(O_09_VALOR))

colSums(is.na(C_o))

#### P - Otras  ----
C_p <- read_csv("Datos_originales/p_capitulos.csv")
colSums(is.na(C_p))

# ANALGÉSICOS OPIOIDES
table(C_g$G_11_M)
C_p1 <- C_p %>%
  select(`P_01`, `P_02`, `DIRECTORIO`, `SECUENCIA_ENCUESTA`, `SECUENCIA_P`, `ORDEN`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_M")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_M == "1") %>%
  mutate(P_02 = case_when(!(P_01 == "1") ~ "na", TRUE ~ P_02)) %>%
  select(-`G_11_M`)
colSums(is.na(C_p1))

# LSD
table(C_g$G_11_N)
C_p2 <- C_p %>%
  select(`P_03`, `P_04`, `DIRECTORIO`, `SECUENCIA_ENCUESTA`, `SECUENCIA_P`, `ORDEN`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_N")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_N == "1") %>%
  mutate(P_04 = case_when(!(P_03 == "1") ~ "na", TRUE ~ P_04)) %>%
  select(-`G_11_N`)
colSums(is.na(C_p2))

# HONGOS - YAGE - CACAO SABANERO
table(C_g$G_11_O); table(C_g$G_11_P); table(C_g$G_11_Q)
C_p3 <- C_p %>%
  select(`P_05`, `P_06`, `DIRECTORIO`, `SECUENCIA_ENCUESTA`, `SECUENCIA_P`, `ORDEN`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_O", "G_11_P", "G_11_Q")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_O == "1" | G_11_P == "1" | G_11_Q == "1") %>%
  mutate(P_06 = case_when(!(P_05 == "1") ~ "na", TRUE ~ P_06)) %>%
  select(-`G_11_O`, -`G_11_P`, -`G_11_Q`)
colSums(is.na(C_p3))

rm(C_p)


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#Se guardan todas las tablas limpias
save.image(file='Limpieza_tablas/tablas.RData')

#Para cargarlas:
#load("Limpieza_tablas/tablas.RData")

library("readr")
library("dplyr")
library("stringr")
library("tidyr")

# CARGA Y LIMPIEZA DE LAS TABLAS ----

## Básicos ----
personas <- read_csv("Datos originales/personas.csv")

personas_seleccionadas <- read_csv("Datos originales/personas_seleccionadas.csv")
personas_seleccionadas <- personas_seleccionadas %>%
  select(DIRECTORIO, SEXO, EDAD, PARENTESCO) %>%
  mutate_all(as.character) %>%
  mutate(EDAD = as.integer(EDAD))

d <- read_csv("Datos originales/d_capitulos.csv")
d <- d %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`,
         -`D_08`, -`D_09`, -`D_10`) %>%
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

d2 <- read_csv("Datos originales/d2_capitulos.csv")
d2 <- d2 %>%
  mutate_all(as.character) %>%
  left_join(personas_seleccionadas, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`D2_05_A`) %>%
  mutate(D2_04 = as.integer(D2_04),
         EDAD = as.integer(EDAD),
         D2_06 = case_when(!(EDAD > 18) ~  "na", TRUE ~ D2_06),
         D2_07 = case_when(!(EDAD > 18) ~  "na", TRUE ~ D2_07))
colSums(is.na(d2))

encuestas <- read_csv("Datos originales/encuestas.csv")
encuestas <- encuestas %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`ESTADO_ENCUESTA`, -`RECO_DIC`) %>%
  mutate_all(as.character) %>%
  mutate(TOTAL_PERSONAS = as.integer(TOTAL_PERSONAS))
colSums(is.na(encuestas))

entorno <- read_csv("Datos originales/g_capitulos.csv")
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
         `G_08_G`, `G_09`, `G_10`)

colSums(is.na(entorno))

## Específicos ----
#### E - Tabaco ----
C_e <- read_csv("Datos originales/e_capitulos.csv")
C_e <- C_e %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  #Solo se tienen en cuenta los que han fumado cigarrillos o cigarrillos electrónicos
  filter(E_01 == "1" | E_10 == "1") %>%
  #se remplaza por "na" todos los no aplica
  mutate(E_02 = case_when(!(E_01 == "1") ~ "na", TRUE ~ E_02),
         E_03 = case_when(!(E_01 == "1") ~ "na", TRUE ~ E_03),
         E_04 = case_when(!(E_01 == "1") ~ "na", TRUE ~ E_04),
         E_05 = case_when(!(E_04 == "1") ~ "na", TRUE ~ E_05),
         E_06 = case_when(!(E_05 == "1") ~ "na", TRUE ~ E_06),
         E_07 = case_when(!(E_05 == "1") ~ "na", TRUE ~ E_07),
         E_08 = case_when(!(E_07 == "1") ~ "na", TRUE ~ E_08),
         E_09 = case_when(!(E_08 == "1") ~ "na", TRUE ~ E_09),
         E_11 = case_when(!(E_10 == "1") ~ "na", TRUE ~ E_11),
         E_12 = case_when(!(E_10 == "1") ~ "na", TRUE ~ E_12))

colSums(is.na(C_e))

#### F - Alcohol ----
C_f <- read_csv("Datos originales/f_capitulos.csv")
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
         F_08 = case_when(!(F_07 == "1") ~ "0", TRUE ~ F_08),
         F_09 = case_when(!(F_07 == "1") ~ "0", TRUE ~ F_09),
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
  mutate(F_08 = as.integer(F_08),
         F_09 = as.integer(F_09))
  
colSums(is.na(C_f))

#### G - Sustancias Psicoactivas ----
C_g <- read_csv("Datos originales/g_capitulos.csv")
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

C_h <- read_csv("Datos originales/h_capitulos.csv")
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
         H_05 = case_when(!(H_04 == "1") ~ "0", TRUE ~ H_05),
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

C_i <- read_csv("Datos originales/i_capitulos.csv")
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
         I_05 = case_when(!(I_04 == "1") ~ "na", TRUE ~ I_05),
         I_06_A = case_when(!(I_04 == "1") ~ "na", is.na(I_06_A) ~ "2", TRUE ~ I_06_A),
         I_06_B = case_when(!(I_04 == "1") ~ "na", is.na(I_06_B) ~ "2", TRUE ~ I_06_B),
         I_06_C = case_when(!(I_04 == "1") ~ "na", is.na(I_06_C) ~ "2", TRUE ~ I_06_C),
         I_07 = case_when(!(I_04 == "1") ~ "na", TRUE ~ I_07),
         I_07_A = case_when(!(I_07 == "1") ~ "na", is.na(I_07_A) ~ "2", TRUE ~ I_07_A),
         I_07_B = case_when(!(I_07 == "1") ~ "na", is.na(I_07_B) ~ "2", TRUE ~ I_07_B),
         I_07_C = case_when(!(I_07 == "1") ~ "na", is.na(I_07_C) ~ "2", TRUE ~ I_07_C),
         I_07_D = case_when(!(I_07 == "1") ~ "na", is.na(I_07_D) ~ "2", TRUE ~ I_07_D),
         I_07_E = case_when(!(I_07 == "1") ~ "na", is.na(I_07_E) ~ "2", TRUE ~ I_07_E))
  
  
colSums(is.na(C_i))

#### J - Inhalables  ----
table(C_g$G_11_C) #Todos (1)
table(C_g$G_11_D) #Dick
table(C_g$G_11_E) #Popper

C_j <- read_csv("Datos originales/j_capitulos.csv")

C_j_1 <- C_j %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_C")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%
  filter(G_11_C == "1") %>%
  mutate(J_01 == case_when(!(G_11_C == "1") ~ "na", TRUE ~ J_01),
         J_02 == case_when(!(G_11_C == "1") ~ "na", TRUE ~ J_02),
         J_03 == case_when((J_02 == "2") ~ "na", TRUE ~ J_03),
         J_04 == case_when((J_02 == "2") ~ "na", TRUE ~ J_04),
         J_05_A = case_when(!(J_02 == "1") ~ "na", is.na(J_05_A) ~ "2", TRUE ~ J_05_A),
         J_05_B = case_when(!(J_02 == "1") ~ "na", is.na(J_05_B) ~ "2", TRUE ~ J_05_B),
         J_05_C = case_when(!(J_02 == "1") ~ "na", is.na(J_05_C) ~ "2", TRUE ~ J_05_C),
         J_05_D = case_when(!(J_02 == "1") ~ "na", is.na(J_05_D) ~ "2", TRUE ~ J_05_D),
         J_05_E = case_when(!(J_02 == "1") ~ "na", is.na(J_05_E) ~ "2", TRUE ~ J_05_E),
         J_05_F = case_when(!(J_02 == "1") ~ "na", is.na(J_05_F) ~ "2", TRUE ~ J_05_F),
         J_05_A = case_when(!(J_02 == "1") ~ "na", is.na(J_06_A) ~ "2", TRUE ~ J_06_A),
         J_06_B = case_when(!(J_02 == "1") ~ "na", is.na(J_06_B) ~ "2", TRUE ~ J_06_B),
         J_06_C = case_when(!(J_02 == "1") ~ "na", is.na(J_06_C) ~ "2", TRUE ~ J_06_C),
         J_06_D = case_when(!(J_02 == "1") ~ "na", is.na(J_06_D) ~ "2", TRUE ~ J_06_D),
         J_06_E = case_when(!(J_02 == "1") ~ "na", is.na(J_06_E) ~ "2", TRUE ~ J_06_E),
         J_06_F = case_when(!(J_02 == "1") ~ "na", is.na(J_06_F) ~ "2", TRUE ~ J_06_F),
         J_06_G = case_when(!(J_02 == "1") ~ "na", is.na(J_06_F) ~ "2", TRUE ~ J_06_G),
         J_06_H = case_when(!(J_02 == "1") ~ "na", is.na(J_06_F) ~ "2", TRUE ~ J_06_H),
         J_06_I = case_when(!(J_02 == "1") ~ "na", is.na(J_06_F) ~ "2", TRUE ~ J_06_I))
colSums(is.na(C_j_1))
C_j_2 <- C_j %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  left_join(C_g[, c("DIRECTORIO", "G_11_D")], by = c("DIRECTORIO" = "DIRECTORIO")) %>%



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
categorias_por_individuo_edit <- categorias_por_individuo %>%
  mutate_at(vars(-1), ~gsub(".*G_11_.*", "", .))

categorias <- unique(categorias_por_individuo$categorias_por_individuo)

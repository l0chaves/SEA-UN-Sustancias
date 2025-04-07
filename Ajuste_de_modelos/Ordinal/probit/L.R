load("Limpieza_tablas/tablas.RData")
source("Ajuste_de_modelos/variables de control.R")
rm(list = setdiff(ls(), c("C_l", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")
library("ordinal")
library("car")
library("PResiduals")
library("ggplot2")
library("sure")
library("sqldf")
library("sandwich")
library("lmtest")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Co-variables ----
X <- d %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`DIRECTORIO`, `D_01`, `D_02`, `D_07`, `D_08`, `D_10`,
                `D2_01`, `D2_03`, `D2_05`, SEXO, EDAD, TOTAL_PERSONAS, `D_05`) %>%
  mutate_at(vars(2,10), as.factor) %>%
  mutate_at(vars(11, 13), as.numeric)

summary(X)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

MD_L <- C_l %>% 
  filter(L_03 != "na") %>% #Y. borrando los registros de na
  mutate(L_03 = factor(L_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(L_03, L_08_VALOR, L_09_A, L_09_B, L_09_C, L_09_D, L_09_E, L_09_F, L_09_G, L_09_H, L_09_I, L_10_A, L_10_B, L_11_A, L_11_B, L_11_C, L_11_D, L_11_E, L_11_F, L_11_G, L_11_H, L_11_I, L_11_J, L_11_K, L_11_L, L_11_M, L_11_N, L_11_O, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate_at(vars(3:28), as.factor)

summary(MD_L) #Los que contestaron 9 en la pregunta original quedan como NA's
MD_L <- MD_L[complete.cases(MD_L),]

# ---------------------------------------------------------------------------- #
# Ajuste de modelos ----
# ---------------------------------------------------------------------------- #

fit0L <- polr(factor(L_03) ~ 1, data = MD_L, Hess = TRUE, method = "probit")
summary(fit0L) #AIC: 752.7631 

fitCL <- polr(factor(L_03) ~ FG_01+G_02+D_11_P+G_11+D_09, data = MD_L, Hess = TRUE, method = "probit")
summary(fitCL) #AIC: 681.5379 

# ---------------------------------------------------------------------------- #
## Categorizando Edad ----
# ---------------------------------------------------------------------------- #

#se categoriza la variable edad
MD_Lc<- sqldf("select *,
             case when EDAD <= 17 then 'Teenagers'
                  when EDAD <= 24 then 'Young'
                  when EDAD <= 34 then 'Young Adult'
                  when EDAD <= 44 then 'Adult'
                  when EDAD <= 63 then 'Elderly'
                  else 'Third Age'
             end as CEDAD
             from MD_L")

# Correcciones por estandariza
MD_L2 <- MD_Lc %>% 
  mutate(L_08_VALOR = scale(L_08_VALOR)) %>%
  mutate(D_11_P = scale(D_11_P)) %>%
  mutate(G_11 = scale(G_11)) %>%
  mutate(EDAD = scale(EDAD)) %>%
  mutate(TOTAL_PERSONAS = scale(TOTAL_PERSONAS)) %>%
  mutate(D_05 = scale(D_05))

### Seleccion ----
#Se hace selección automática cambiando edad por su versión categórica
fitCL <- polr(factor(L_03) ~ FG_01+G_02+D_11_P+G_11+D_09, data = MD_L2, Hess = TRUE, method = "probit")
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09, 
              upper=~ L_03+ L_08_VALOR+ L_09_A+ L_09_B+ L_09_C+ L_09_D+ L_09_E+ L_09_F+ L_09_G+ L_09_H+ L_09_I+ L_10_A+ L_10_B+ L_11_A+L_11_B+L_11_C+L_11_D+L_11_E+L_11_F+L_11_G+L_11_H+L_11_I+L_11_J+L_11_K+L_11_L+L_11_M+L_11_N+L_11_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+CEDAD+SEXO+
                FG_01+G_02+D_11_P+G_11+D_09)

stepAIC(fitCL, scope=scope, direction = "forward")

### 4 AIC: 630.514 ----
fit4L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + D_09 + L_11_H +
                   L_09_C + L_11_O + CEDAD + L_11_A + D_10, 
                   data = MD_L2, Hess = TRUE, method = "probit")

fit4L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + D_09 + L_11_H +
                 L_09_C + L_11_O + CEDAD + L_11_A + D_10, data = MD_L2, link = "probit")

summary(fit4L_mass); vif(fit4L_mass)
summary(fit4L_ord)

### 5 AIC: 632.81 ----
# Mejor modelo seleccionado sin usar pesos, estimado sin pesos
fit5L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + D_09 + L_11_H +
                   L_09_C + L_11_O + CEDAD + L_11_A, data = MD_L2, Hess = TRUE, method = "probit")

fit5L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + D_09 + L_11_H +
                 L_09_C + L_11_O + CEDAD + L_11_A, data = MD_L2, link = "probit")

summary(fit5L_mass); vif(fit5L_mass)
summary(fit5L_ord)

# ---------------------------------------------------------------------------- #
## Pesos muestrales ----
# ---------------------------------------------------------------------------- #

personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
pk <- personas_seleccionadas %>% 
  dplyr::select(DIRECTORIO, FEX_C) %>% 
  mutate(DIRECTORIO = as.character(DIRECTORIO))

MD_Lpk <- MD_L2 %>% left_join(pk, by=c("DIRECTORIO"="DIRECTORIO"))
rm(personas_seleccionadas, pk)

### 6 AIC:  307013.80 ----
# Mejor modelo seleccionado sin usar pesos, estimado usando pesos
fit6L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + D_09 + L_11_H +
                     L_09_C + L_11_O + CEDAD + L_11_A, 
                   start =  c(fit5L_mass$coefficients, fit5L_mass$zeta),
                   data = MD_Lpk, Hess = TRUE, method = "probit", weights = FEX_C)

fit6L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + D_09 + L_11_H +
                   L_09_C + L_11_O + CEDAD + L_11_A, 
                 data = MD_Lpk, link = "probit", weights = FEX_C)

summary(fit6L_mass); vif(fit6L_mass)
summary(fit6L_ord)

### Seleccion ----
#Se hace selección automática usando los pesos muestrales
fitLpk <- polr(factor(L_03) ~ FG_01+G_02+D_11_P+G_11+D_09, data = MD_Lpk, Hess = TRUE, method = "probit", weights = FEX_C)
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09, 
              upper=~ L_03+ L_08_VALOR+ L_09_A+ L_09_B+ L_09_C+ L_09_D+ L_09_E+ L_09_F+ L_09_G+ L_09_H+ L_09_I+ L_10_A+ L_10_B+ L_11_A+L_11_B+L_11_C+L_11_D+L_11_E+L_11_F+L_11_G+L_11_H+L_11_I+L_11_J+L_11_K+L_11_L+L_11_M+L_11_N+L_11_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+CEDAD+SEXO+
                FG_01+G_02+D_11_P+G_11+D_09)

stepAIC(fitLpk, scope=scope, direction = "forward")

### 7 AIC:  268486.55  ----
fit7L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + L_11_H + L_11_O + CEDAD + D_02 + D2_05 + D_08 + D2_03 + 
                     D_05 + D2_01 + L_11_B + L_11_N + SEXO + L_11_K + L_09_F + 
                     L_11_D + L_11_G + L_11_A + L_09_H + L_09_A + L_11_E + D_01 + 
                     L_09_E + TOTAL_PERSONAS + L_08_VALOR + L_09_I + L_11_I + 
                     L_11_M, data = MD_Lpk, Hess = TRUE, method = "probit", weights = FEX_C)

fit7L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + L_11_H + L_11_O + CEDAD + D_02 + D2_05 + D_08 + D2_03 + 
                   D_05 + D2_01 + L_11_B + L_11_N + SEXO + L_11_K + L_09_F + 
                   L_11_D + L_11_G + L_11_A + L_09_H + L_09_A + L_11_E + D_01 + 
                   L_09_E + TOTAL_PERSONAS + L_08_VALOR + L_09_I + L_11_I + 
                   L_11_M, data = MD_Lpk, link = "probit", weights = FEX_C)

summary(fit7L_mass); vif(fit7L_mass)
summary(fit7L_ord)

### 8 AIC: 279765.62  ----
# Mejor modelo seleccionado usando pesos, estimado usando pesos
fit8L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + L_11_H + L_11_O + CEDAD + D_08 + D2_03 + 
                     D_05 + D2_01 + L_11_B + L_11_N + SEXO + L_11_K + L_09_F + 
                     L_11_D + L_11_G + L_11_A + L_09_H + L_09_A + L_11_E + D_01 + 
                     L_09_E + TOTAL_PERSONAS + L_08_VALOR + L_09_I + L_11_I + 
                     L_11_M, data = MD_Lpk, Hess = TRUE, method = "probit", weights = FEX_C)

fit8L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + L_11_H + L_11_O + CEDAD + D_08 + D2_03 + 
                   D_05 + D2_01 + L_11_B + L_11_N + SEXO + L_11_K + L_09_F + 
                   L_11_D + L_11_G + L_11_A + L_09_H + L_09_A + L_11_E + D_01 + 
                   L_09_E + TOTAL_PERSONAS + L_08_VALOR + L_09_I + L_11_I + 
                   L_11_M, data = MD_Lpk, link = "probit", weights = FEX_C)

summary(fit8L_mass); vif(fit8L_mass)
summary(fit8L_ord)

### 9 AIC: 668.69   ----
# Mejor modelo seleccionado usando pesos, estimado sin pesos
fit9L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + L_11_H + L_11_O + CEDAD + D_08 + D2_03 + 
                     D_05 + D2_01 + L_11_B + L_11_N + SEXO + L_11_K + L_09_F + 
                     L_11_D + L_11_G + L_11_A + L_09_H + L_09_A + L_11_E + D_01 + 
                     L_09_E + TOTAL_PERSONAS + L_08_VALOR + L_09_I + L_11_I + 
                     L_11_M, data = MD_Lpk, Hess = TRUE, method = "probit")

fit9L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + L_11_H + L_11_O + CEDAD + D_08 + D2_03 + 
                   D_05 + D2_01 + L_11_B + L_11_N + SEXO + L_11_K + L_09_F + 
                   L_11_D + L_11_G + L_11_A + L_09_H + L_09_A + L_11_E + D_01 + 
                   L_09_E + TOTAL_PERSONAS + L_08_VALOR + L_09_I + L_11_I + 
                   L_11_M, data = MD_Lpk, link = "probit")

summary(fit9L_mass); vif(fit9L_mass)
summary(fit9L_ord)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Validación ----
source('Ajuste_de_modelos/Ordinal/probit/Validacion.R')

# ---------------------------------------------------------------------------- #
#### fit5 - fit6 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X5 <- MD_Lpk %>% dplyr::select(L_03, FG_01 ,G_02 ,D_11_P ,G_11 ,
                                   D_09, L_11_H, L_09_C, L_11_O, CEDAD, L_11_A)
hat_X5 <- model.matrix(L_03 ~ ., hat_X5)
hat_X5 <- hat_X5[,-1] # - el intercepto

val_f5 <- validacion(fit_mass = fit5L_mass, fit_ord = fit5L_ord, hat_X = hat_X5, y = MD_Lpk$L_03)
val_f6 <- validacion(fit_mass = fit6L_mass, fit_ord = fit6L_ord, hat_X = hat_X5, y = MD_Lpk$L_03)

                     #### fit8 - fit9 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X8 <- MD_Lpk %>% dplyr::select(L_03, FG_01, G_02, D_11_P, G_11, 
                                   D_09, L_11_H, L_11_O, CEDAD, D_08, D2_03, 
                                   D_05, D2_01, L_11_B, L_11_N, SEXO, L_11_K, L_09_F, 
                                   L_11_D, L_11_G, L_11_A, L_09_H, L_09_A, L_11_E, D_01, 
                                   L_09_E, TOTAL_PERSONAS, L_08_VALOR, L_09_I, L_11_I, 
                                   L_11_M)
hat_X8 <- model.matrix(L_03 ~ ., hat_X8)
hat_X8 <- hat_X8[,-1] # - el intercepto

val_f8 <- validacion(fit_mass = fit8L_mass, fit_ord = fit8L_ord, hat_X = hat_X8, y = MD_Lpk$L_03)
val_f9 <- validacion(fit_mass = fit9L_mass, fit_ord = fit9L_ord, hat_X = hat_X8, y = MD_Lpk$L_03)

#### fitC ----
fitCK_mass <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
                   data = MD_K, Hess = TRUE, method = "probit")
fitCK_ord  <- clm(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
                  data = MD_K, link = "probit")

hat_Xc <- MD_K %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, D_09)
hat_Xc <- model.matrix(K_04 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

val_fC <- validacion(fit_mass = fitCK_mass, fit_ord = fitCK_ord, hat_X = hat_Xc)




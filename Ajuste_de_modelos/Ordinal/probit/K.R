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

load("Limpieza_tablas/tablas.RData")
source('Ajuste_de_modelos/variables de control.R')
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Co-variables ----
#Se tienen en cuenta variables socio-demográficas para usar como covariables
X <- d %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_f,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_e,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(tratamiento,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`DIRECTORIO`, `D_01`, `D_02`, `D_06`, `D_07`, `D_08`, `D_10`,
                `D2_01`, `D2_03`, `D2_05`, `D2_06`, SEXO, TIPO, ESTRATO,
                `F_12`, `E_04`, `Q_02`, `Q_03`,
                `D_05`, EDAD, TOTAL_PERSONAS) %>%
  mutate_at(vars(2:18), as.factor) %>%
  mutate_at(vars(19,21), as.numeric)

summary(X)

rm(list = setdiff(ls(), c("C_k", "control", "X")))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#Se añaden las variables relacionas con el consumo de marihuana

MD_K <- C_k %>% 
  filter(K_04 != "na") %>% #Y. borrando los registros de na
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  dplyr::select(K_04, K_09_VALOR, K_10_A, K_10_B, K_10_C, K_10_D, K_10_E, K_10_F, K_10_G, K_10_H, K_10_I, K_11, 
                K_12_A, K_12_B, K_12_C, K_12_D, K_12_E, K_12_F, K_12_G, K_12_H, K_12_I, K_12_J, K_12_K, K_12_L, K_12_M, K_12_N, K_12_O, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate_at(vars(3:27), as.factor) %>%
  mutate(across(c(3:11, 13:27), ~relevel(., ref = "2")))

  
summary(MD_K) #Los que contestaron 9 en la pregunta original quedan como NA's
MD_K <- MD_K[complete.cases(MD_K),]


# ---------------------------------------------------------------------------- #
# Ajuste de modelos ----
# ---------------------------------------------------------------------------- #

fit0K <- polr(factor(K_04) ~ 1, data = MD_K, Hess = TRUE, method = "probit")
summary(fit0K) #AIC: 3800.169

fitCK <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
              data = MD_K, Hess = TRUE, method = "probit")
summary(fitCK) #AIC:3231.601


# ---------------------------------------------------------------------------- #
## Categorizando Edad ----
# ---------------------------------------------------------------------------- #

#se categoriza la variable edad
MD_Kc<- sqldf("select *,
             case when EDAD <= 17 then 'Teenagers'
                  when EDAD <= 24 then 'Young'
                  when EDAD <= 34 then 'Young Adult'
                  when EDAD <= 44 then 'Adult'
                  when EDAD <= 63 then 'Elderly'
                  else 'Third Age'
             end as CEDAD
             from MD_K")


# Correcciones por multicolinealidad
MD_K3 <- MD_Kc %>% 
  mutate(D2_05 = case_when(
    D2_05 %in% c(1, 2, 3,  9) ~ "1",
    TRUE ~ as.character(D2_05)
  ),
  D2_05 = factor(D2_05))

### Seleccion ----
#Se hace selección automática cambiando edad por su versión categórica
fitCKc <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09+CEDAD, data = MD_K3, Hess = TRUE, method = "probit")
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+CEDAD, 
              upper=~ FG_01 + G_02 + D_11_P + G_11 + D_09 + DIRECTORIO + 
                D_01 + D_02 + D_06 + D_07 + D_08 + D_10 + D2_01 + D2_03 + D2_05 + D2_06 + 
                SEXO + TIPO + ESTRATO + F_12 + E_04 + Q_02 + Q_03 + D_05 + EDAD + TOTAL_PERSONAS + 
                K_04 + K_09_VALOR + K_10_A + K_10_B + K_10_C + K_10_D + K_10_E + K_10_F + K_10_G + K_10_H + K_10_I + 
                K_11 + K_12_A + K_12_B + K_12_C + K_12_D + K_12_E + K_12_F + K_12_G + K_12_H + K_12_I + K_12_J + K_12_K + K_12_L + K_12_M + K_12_N + K_12_O + 
                CEDAD)
stepAIC(fitCKc, scope=scope, direction = "forward")

### 5 AIC: 2267.17 ----
# Mejor modelo seleccionado sin usar pesos, estimado sin pesos
fit4K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                     D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                     E_04 + K_12_K + Q_03, data = MD_K3, Hess = TRUE, method = "probit")

fit4K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                   D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                   E_04 + K_12_K + Q_03, data = MD_K3, link = "probit")

summary(fit4K_mass); vif(fit4K_mass)
summary(fit4K_ord)

### 5 AIC: 2267.17  ----
fit5K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                     D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                     E_04 + K_12_K + Q_03, data = MD_K3, Hess = TRUE, method = "probit")

fit5K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                   D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                   E_04 + K_12_K + Q_03, data = MD_K3, link = "probit")

summary(fit5K_mass); vif(fit5K_mass)
summary(fit5K_ord)

## Pesos muestrales ----
# ---------------------------------------------------------------------------- #
personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
pk <- personas_seleccionadas %>% 
  dplyr::select(DIRECTORIO, FEX_C) %>% 
  mutate(DIRECTORIO = as.character(DIRECTORIO))

MD_Kpk <- MD_K3 %>% left_join(pk, by=c("DIRECTORIO"="DIRECTORIO"))
rm(personas_seleccionadas, pk)

### 6 AIC: 1477814.78 ----
# Mejor modelo seleccionado sin usar pesos, estimado usando pesos
fit6K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                     D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                     E_04 + K_12_K + Q_03, data = MD_Kpk, Hess = TRUE, method = "probit",
                     weights = FEX_C, start = c(fit5K_mass$coefficients, fit5K_mass$zeta))

fit6K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                   D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                   E_04 + K_12_K + Q_03, data = MD_Kpk, link = "probit", weights = FEX_C,
                   start = c(fit5K_ord$alpha, fit5K_ord$beta))

summary(fit6K_mass); vif(fit6K_mass)
summary(fit6K_ord)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Validación ----
source('Ajuste_de_modelos/Ordinal/probit/Validacion.R')

# ---------------------------------------------------------------------------- #
#### fit5 - fit6 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X5 <- MD_Kpk %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, 
                                     D_09, CEDAD, K_12_O, K_10_C, K_12_H, SEXO, K_12_I, 
                                     D2_05, K_10_D, K_10_E, K_11, K_12_C, K_10_I, D_01, 
                                     E_04, K_12_K, Q_03)
hat_X5 <- model.matrix(K_04 ~ ., hat_X5)
hat_X5 <- hat_X5[,-1] # - el intercepto

val_f5 <- validacion(fit_mass = fit5K_mass, fit_ord = fit5K_ord, hat_X = hat_X5, y = MD_Kpk$K_04)
val_f6 <- validacion(fit_mass = fit6K_mass, fit_ord = fit6K_ord, hat_X = hat_X5, y = MD_Kpk$K_04)

fit5K_mass$coefficients/fit6K_mass$coefficients

#### fitC ----
fitCK_mass <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09+CEDAD,
              data = MD_K3, Hess = TRUE, method = "probit")
fitCK_ord  <- clm(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09+CEDAD,
             data = MD_K3, link = "probit")

hat_Xc <- MD_K3 %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, D_09, CEDAD)
hat_Xc <- model.matrix(K_04 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

val_fC <- validacion(fit_mass = fitCK_mass, fit_ord = fitCK_ord, hat_X = hat_Xc, y = MD_K3$K_04)


# P valores ----
# Making Sandwiches with Bread and Meat
vcov_ord <- sandwich(fit6K_ord)

sqrt(diag(vcov_ord))
coeftest(fit6K_ord, vcov = vcov_ord)

# Re evalular modelos ----
MD_K <- C_k %>% 
  filter(K_04 != "na") %>% #Y. borrando los registros de na
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  dplyr::select(K_04, K_09_VALOR, K_10_A, K_10_B, K_10_C, K_10_D, K_10_E, K_10_F, K_10_G, K_10_H, K_10_I, K_11, 
                K_12_A, K_12_B, K_12_C, K_12_D, K_12_E, K_12_F, K_12_G, K_12_H, K_12_I, K_12_J, K_12_K, K_12_L, K_12_M, K_12_N, K_12_O, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate_at(vars(3:27), as.factor) %>%
  mutate(across(c(3:11, 13:27), ~relevel(., ref = "2")))

MD_K <- sqldf("select *,
             case when EDAD <= 17 then 'Teenagers'
                  when EDAD <= 24 then 'Young'
                  when EDAD <= 34 then 'Young Adult'
                  when EDAD <= 44 then 'Adult'
                  when EDAD <= 63 then 'Elderly'
                  else 'Third Age'
             end as CEDAD
             from MD_K")

MD <- MD_K %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, 
                         D_09, CEDAD, K_12_O, K_10_C, K_12_H, SEXO, K_12_I, 
                         D2_05, K_10_D, K_10_E, K_11, K_12_C, K_10_I, D_01, 
                         E_04, K_12_K, Q_03, DIRECTORIO)
MD <- MD[complete.cases(MD),]

personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
pk <- personas_seleccionadas %>% 
  dplyr::select(DIRECTORIO, FEX_C) %>% 
  mutate(DIRECTORIO = as.character(DIRECTORIO))

MD <- MD %>% left_join(pk, by=c("DIRECTORIO"="DIRECTORIO"))
rm(personas_seleccionadas, pk)

fitK_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                    D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                    D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                    E_04 + K_12_K + Q_03, data = MD, Hess = TRUE, method = "probit",
                  weights = FEX_C, start = c(fit6K_mass$coefficients, fit6K_mass$zeta))

fitK_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + CEDAD + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + 
                   D2_05 + K_10_D + K_10_E + K_11 + K_12_C + K_10_I + D_01 + 
                   E_04 + K_12_K + Q_03, data = MD, link = "probit", weights = MD_Kpk$FEX_C,
                 start = c(fit6K_ord$alpha, fit6K_ord$beta))

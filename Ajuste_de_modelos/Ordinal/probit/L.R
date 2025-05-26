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
  mutate_at(vars(19,21), as.numeric) %>%
  mutate(D2_05 = case_when(D2_05 %in% c(1, 2, 3,  9) ~ "1",
    TRUE ~ as.character(D2_05)), D2_05 = factor(D2_05))

summary(X)

rm(list = setdiff(ls(), c("C_l", "control", "X")))
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

### Seleccion ----
#Se hace selección automática cambiando edad por su versión categórica
fitCL <- polr(factor(L_03) ~ FG_01+G_02+D_11_P+G_11+D_09+CEDAD, data = MD_Lc, Hess = TRUE, method = "probit")
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09+CEDAD, 
              upper=~ L_08_VALOR+ L_09_A+ L_09_B+ L_09_C+ L_09_D+ L_09_E+ L_09_F+ L_09_G+ L_09_H+ L_09_I+ L_10_A+ L_10_B+ L_11_A+L_11_B+L_11_C+L_11_D+L_11_E+L_11_F+L_11_G+L_11_H+L_11_I+L_11_J+L_11_K+L_11_L+L_11_M+L_11_N+L_11_O+
                FG_01 + G_02 + D_11_P + G_11 + D_09 +
                D_01 + D_02 + D_06 + D_07 + D_08 + D_10 + D2_01 + D2_03 + D2_05 + D2_06 + 
                SEXO + TIPO + ESTRATO + F_12 + E_04 + Q_02 + Q_03 + D_05 + CEDAD + TOTAL_PERSONAS)

stepAIC(fitCL, scope=scope, direction = "forward")

### 5 AIC: 537.92  ----
# Mejor modelo seleccionado sin usar pesos, estimado sin pesos

fit5L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + CEDAD + L_11_H + D2_05 + L_11_O +
                     L_11_A + Q_02 + F_12 + D_08 + 
                     L_11_B + L_09_A, data = MD_Lc, Hess = TRUE, method = "probit")

fit5L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + CEDAD + L_11_H + D2_05 + L_11_O +
                   L_11_A + Q_02 + F_12 + D_08 + 
                   L_11_B + L_09_A, data = MD_Lc, link = "probit")

summary(fit5L_mass); vif(fit5L_mass)
summary(fit5L_ord)

# ---------------------------------------------------------------------------- #
## Pesos muestrales ----
# ---------------------------------------------------------------------------- #

personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
pk <- personas_seleccionadas %>% 
  dplyr::select(DIRECTORIO, FEX_C) %>% 
  mutate(DIRECTORIO = as.character(DIRECTORIO))

MD_Lpk <- MD_Lc %>% left_join(pk, by=c("DIRECTORIO"="DIRECTORIO"))
rm(personas_seleccionadas, pk)

### 6 AIC:  307013.80 ----
# Mejor modelo seleccionado sin usar pesos, estimado usando pesos
fit6L_mass <- polr(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + CEDAD + L_11_H + D2_05 + L_11_O +
                     L_11_A + Q_02 + F_12 + D_08 + L_11_B + L_09_A, 
                   start =  c(fit5L_mass$coefficients, fit5L_mass$zeta),
                   data = MD_Lpk, Hess = TRUE, method = "probit", weights = FEX_C)

fit6L_ord <- clm(factor(L_03) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + CEDAD + L_11_H + D2_05 + L_11_O +
                   L_11_A + Q_02 + F_12 + D_08 + L_11_B + L_09_A,
                 data = MD_Lpk, link = "probit", weights = FEX_C)

summary(fit6L_mass); vif(fit6L_mass)
summary(fit6L_ord)

# Validación ----
source('Ajuste_de_modelos/Ordinal/probit/Validacion.R')

# ---------------------------------------------------------------------------- #
#### fit5 - fit6 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X5 <- MD_Lpk %>% dplyr::select(L_03, FG_01, G_02, D_11_P, G_11, 
                                     D_09, CEDAD, L_11_H, D2_05, L_11_O,
                                     L_11_A, Q_02, F_12, D_08, L_11_B, L_09_A)
hat_X5 <- model.matrix(L_03 ~ ., hat_X5)
hat_X5 <- hat_X5[,-1] # - el intercepto

val_f5 <- validacion(fit_mass = fit5L_mass, fit_ord = fit5L_ord, hat_X = hat_X5, y = MD_Lpk$L_03)
val_f6 <- validacion(fit_mass = fit6L_mass, fit_ord = fit6L_ord, hat_X = hat_X5, y = MD_Lpk$L_03)

 
#### fitC ----
fitCL_mass <- polr(factor(L_03) ~ FG_01+G_02+D_11_P+G_11+D_09+CEDAD, data = MD_Lc, Hess = TRUE, method = "probit")
fitCL_ord  <- clm(factor(L_03) ~ FG_01+G_02+D_11_P+G_11+D_09+CEDAD, data = MD_Lc, link = "probit")

hat_Xc <- MD_Lc %>% dplyr::select(L_03, FG_01, G_02, D_11_P, G_11, D_09, CEDAD)
hat_Xc <- model.matrix(L_03 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

val_fC <- validacion(fit_mass = fitCL_mass, fit_ord = fitCL_ord, hat_X = hat_Xc, y = MD_Lpk$L_03)

# P valores ----
# Making Sandwiches with Bread and Meat
vcov_ord <- sandwich(fit6L_ord)

sqrt(diag(vcov_ord))
coeftest(fit6L_ord, vcov = vcov_ord)

fit5L_mass$coefficients/fit6L_mass$coefficients

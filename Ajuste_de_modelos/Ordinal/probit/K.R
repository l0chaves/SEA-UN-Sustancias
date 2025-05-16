load("Limpieza_tablas/tablas.RData")
source('Ajuste_de_modelos/variables de control.R')

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
  dplyr::select(K_04, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))

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

# Correcciones por estandariza
MD_K2 <- MD_Kc %>% 
  mutate(D_11_P = scale(D_11_P)) %>%
  mutate(G_11 = scale(G_11)) %>%
  mutate(EDAD = scale(EDAD)) %>%
  mutate(TOTAL_PERSONAS = scale(TOTAL_PERSONAS)) %>%
  mutate(D_05 = scale(D_05))

### Seleccion ----
#Se hace selección automática cambiando edad por su versión categórica
fitCKc <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09, data = MD_K2, Hess = TRUE, method = "probit")
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09, 
              upper=~ FG_01+G_02+D_11_P+G_11+D_09+DIRECTORIO+D_01+D_02+D_06+D_07+
                D_08+D_10+D2_01+D2_03+D2_05+D2_06+SEXO+TIPO+ESTRATO+
                F_12+E_04+Q_02+Q_03+D_05+EDAD+TOTAL_PERSONAS)
stepAIC(fitCKc, scope=scope, direction = "forward")


### 4 AIC: 2473.521  ----
#!!! Multicolinealidad, no converge
fit4K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + D2_05 + SEXO + D2_06 + E_04 + Q_03 + D_02 + F_12, 
                   data = MD_K2, Hess = TRUE, method = "probit")

fit4K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + D2_05 + SEXO + D2_06 + E_04 + Q_03 + D_02 + F_12, data = MD_K2, link = "probit")

summary(fit4K_mass); vif(fit4K_mass)
summary(fit4K_ord)

### 5 AIC: 3333.79  ----
# Mejor modelo seleccionado sin usar pesos, estimado sin pesos
fit5K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + SEXO + D2_06 + E_04 + Q_03 + D_02 + F_12, data = MD_K2, Hess = TRUE, method = "probit")

fit5K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + SEXO + D2_06 + E_04 + Q_03 + D_02 + F_12, data = MD_K2, link = "probit")

summary(fit5K_mass); vif(fit5K_mass)
summary(fit5K_ord)


# ---------------------------------------------------------------------------- #
## Pesos muestrales ----
# ---------------------------------------------------------------------------- #

personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
pk <- personas_seleccionadas %>% 
  dplyr::select(DIRECTORIO, FEX_C) %>% 
  mutate(DIRECTORIO = as.character(DIRECTORIO))

MD_Kpk <- MD_K2 %>% left_join(pk, by=c("DIRECTORIO"="DIRECTORIO"))
rm(personas_seleccionadas, pk)

### 6 AIC: 1280640.15 ----
# Mejor modelo seleccionado sin usar pesos, estimado usando pesos
fit6K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + SEXO + D2_06 + E_04 + Q_03 + D_02 + F_12, data = MD_Kpk, Hess = TRUE, method = "probit",
                     weights = FEX_C)

fit6K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + SEXO + D2_06 + E_04 + Q_03 + D_02 + F_12, data = MD_Kpk, link = "probit", weights = FEX_C)

summary(fit6K_mass); vif(fit6K_mass)
summary(fit6K_ord)


### Seleccion ----
fitCKpk <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09, 
                data = MD_Kpk, Hess = TRUE, method = "probit", weights = FEX_C)
stepAIC(fitCKpk, scope=scope, direction = "forward")

### 7 AIC: 1241268.18   ----
#NO converge
fit7.1K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + D_02 + D2_05 + SEXO + D2_06 + D_07 + F_12 + Q_03 + 
                     ESTRATO + D2_03 + E_04 + Q_02, 
                   data = MD_Kpk, Hess = TRUE, method = "probit", weights = FEX_C)

fit7.1K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + D_02 + D2_05 + SEXO + D2_06 + D_07 + F_12 + Q_03 + 
                   ESTRATO + D2_03 + E_04 + Q_02, 
                 data = MD_Kpk, link = "probit", weights = FEX_C)

summary(fit7.1K_mass); vif(fit7.1K_mass)

### 8 AIC: 1265776.55   ----
# Mejor modelo seleccionado usando pesos, estimado usando pesos
fit8K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + D_02 + SEXO + D2_06 + D_07 + F_12 + Q_03 + 
                     D2_03 + E_04, 
                     data = MD_Kpk, Hess = TRUE, method = "probit", weights = FEX_C)

fit8K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + D_02 + SEXO + D2_06 + D_07 + F_12 + Q_03 + 
                   D2_03 + E_04, data = MD_Kpk, link = "probit", weights = FEX_C)

summary(fit8K_mass); vif(fit8K_mass)

### 9 AIC: 2504.308  ----
# Mejor modelo seleccionado usando pesos, estimado sin pesos

fit9K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + D_02 + SEXO + D2_06 + D_07 + F_12 + Q_03 + 
                     D2_03 + E_04, data = MD_Kpk, Hess = TRUE, method = "probit")

fit9K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + D_02 + SEXO + D2_06 + D_07 + F_12 + Q_03 + 
                   D2_03 + E_04, data = MD_Kpk, link = "probit")

summary(fit9K_mass); vif(fit9K_mass)
summary(fit9K_ord)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Validación ----
source('Ajuste_de_modelos/Ordinal/probit/Validacion.R')

# ---------------------------------------------------------------------------- #
#### fit5 - fit6 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X5 <- MD_Kpk %>% dplyr::select(K_04,FG_01, G_02, D_11_P, G_11, 
                                     D_09, SEXO, D2_06, E_04, Q_03, D_02, F_12)
hat_X5 <- model.matrix(K_04 ~ ., hat_X5)
hat_X5 <- hat_X5[,-1] # - el intercepto

val_f5 <- validacion(fit_mass = fit5K_mass, fit_ord = fit5K_ord, hat_X = hat_X5, y = MD_Kpk$K_04)
val_f6 <- validacion(fit_mass = fit6K_mass, fit_ord = fit6K_ord, hat_X = hat_X5, y = MD_Kpk$K_04)

#### fit8 - fit9 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X8 <- MD_Kpk %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, 
                                     D_09, D_02, SEXO, D2_06, D_07, F_12, Q_03, 
                                     D2_03, E_04)
hat_X8 <- model.matrix(K_04 ~ ., hat_X8)
hat_X8 <- hat_X8[,-1] # - el intercepto

val_f8 <- validacion(fit_mass = fit8K_mass, fit_ord = fit8K_ord, hat_X = hat_X8, y = MD_Kpk$K_04)
val_f9 <- validacion(fit_mass = fit9K_mass, fit_ord = fit9K_ord, hat_X = hat_X8, y = MD_Kpk$K_04)

#### fitC ----
fitCK_mass <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
              data = MD_K, Hess = TRUE, method = "probit")
fitCK_ord  <- clm(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
             data = MD_K, link = "probit")

hat_Xc <- MD_K %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, D_09)
hat_Xc <- model.matrix(K_04 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

val_fC <- validacion(fit_mass = fitCK_mass, fit_ord = fitCK_ord, hat_X = hat_Xc)


# P valores ----
# Making Sandwiches with Bread and Meat
vcov_ord <- sandwich(fit8K_ord)

sqrt(diag(vcov_ord))
coeftest(fit6K_ord, vcov = vcov_ord)

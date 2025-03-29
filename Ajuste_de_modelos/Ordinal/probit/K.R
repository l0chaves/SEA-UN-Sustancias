load("Limpieza_tablas/tablas.RData")
source('Ajuste_de_modelos/variables de control.R')
rm(list = setdiff(ls(), c("C_k", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")
library("ordinal")
library("car")
library("PResiduals")
library("ggplot2")
library("sure")
library("sqldf")


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Co-variables ----
#Se tienen en cuenta variables socio-demográficas para usar como covariables
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
#Se añaden las variables relacionas con el consumo de marihuana

MD_K <- C_k %>% 
  filter(K_04 != "na") %>% #Y. borrando los registros de na
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  dplyr::select(K_04, K_09_VALOR, K_10_A, K_10_B, K_10_C, K_10_D, K_10_E, K_10_F, K_10_G, K_10_H, K_10_I, K_11, 
                K_12_A, K_12_B, K_12_C, K_12_D, K_12_E, K_12_F, K_12_G, K_12_H, K_12_I, K_12_J, K_12_K, K_12_L, K_12_M, K_12_N, K_12_O, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate_at(vars(3:27), as.factor)
  
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

## selección
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09,
              upper=~ K_04+K_09_VALOR+K_10_A+K_10_B+K_10_C+K_10_D+K_10_E+K_10_F+K_10_G+K_10_H+K_10_I+K_11+K_12_A+K_12_B+K_12_C+K_12_D+K_12_E+K_12_F+K_12_G+K_12_H+K_12_I+K_12_J+K_12_K+K_12_L+K_12_M+K_12_N+K_12_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11_P+G_11+D_09)
stepAIC(fitCK, scope=scope, direction = "forward")

### 1 AIC: 2861.911  ----
fit1K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + EDAD + 
                     K_10_E + D2_05 + K_11 + K_10_D + D_01 + K_12_C + K_10_I + 
                     TOTAL_PERSONAS, data = MD_K, Hess = TRUE, method = "probit")

fit1K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + EDAD + 
                   K_10_E + D2_05 + K_11 + K_10_D + D_01 + K_12_C + K_10_I + 
                   TOTAL_PERSONAS, data = MD_K, link = "probit")

summary(fit1K_ord)
summary(fit1K_mass); vif(fit1K_mass)
PseudoR2(fit1K_mass)

### 2 AIC: 2884.23  ----
fit2K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + EDAD + 
                     K_10_E + K_11 + K_10_D + D_01 + K_12_C + K_10_I + 
                     TOTAL_PERSONAS, data = MD_K, Hess = TRUE, method = "probit")

fit2K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_O + K_10_C + K_12_H + SEXO + K_12_I + EDAD + 
                   K_10_E + K_11 + K_10_D + D_01 + K_12_C + K_10_I + 
                   TOTAL_PERSONAS, data = MD_K, link = "probit")

summary(fit2K_ord)
summary(fit2K_mass); vif(fit2K_mass)
PseudoR2(fit2K_mass)

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

#Se hace selección automática cambiando edad por su versión categórica
fitCKc <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09, data = MD_Kc, Hess = TRUE, method = "probit")
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09, 
              upper=~ K_04+K_09_VALOR+K_10_A+K_10_B+K_10_C+K_10_D+K_10_E+K_10_F+K_10_G+K_10_H+K_10_I+K_11+K_12_A+K_12_B+K_12_C+K_12_D+K_12_E+K_12_F+K_12_G+K_12_H+K_12_I+K_12_J+K_12_K+K_12_L+K_12_M+K_12_N+K_12_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+CEDAD+SEXO+
                FG_01+G_02+D_11_P+G_11+D_09)
stepAIC(fitCKc, scope=scope, direction = "forward")


### 4 AIC: 3297.959 ----
#!!! Multicolinealidad, no converge
fit4K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_O + K_10_C + CEDAD + K_12_I + D2_05 + SEXO + 
                     K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                     K_12_K, data = MD_Kc, Hess = TRUE, method = "probit")

fit4K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_O + K_10_C + CEDAD + K_12_I + D2_05 + SEXO + 
                   K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                   K_12_K, data = MD_Kc, link = "probit")

summary(fit4K_mass); vif(fit4K_mass)
summary(fit4K_ord)

### 5 AIC: 3333.79  ----
fit5K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_O + K_10_C + CEDAD + K_12_I + SEXO + 
                     K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                     K_12_K, data = MD_Kc, Hess = TRUE, method = "probit")

fit5K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_O + K_10_C + CEDAD + K_12_I + SEXO + 
                   K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                   K_12_K, data = MD_Kc, link = "probit")

summary(fit5K_mass); vif(fit5K_mass)
summary(fit5K_ord)


# ---------------------------------------------------------------------------- #
## Pesos muestrales ----
# ---------------------------------------------------------------------------- #

personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
pk <- personas_seleccionadas %>% 
  dplyr::select(DIRECTORIO, FEX_C) %>% 
  mutate(DIRECTORIO = as.character(DIRECTORIO))

MD_Kpk <- MD_Kc %>% left_join(pk, by=c("DIRECTORIO"="DIRECTORIO"))
rm(personas_seleccionadas, pk)

### 6 AIC: 1477814.78 ----
fit6K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_O + K_10_C + CEDAD + K_12_I + SEXO + 
                     K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                     K_12_K, data = MD_Kpk, Hess = TRUE, method = "probit",
                     weights = FEX_C)

fit6K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_O + K_10_C + CEDAD + K_12_I + SEXO + 
                   K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                   K_12_K,, data = MD_Kpk, link = "probit", weights = FEX_C)

summary(fit6K_mass); vif(fit6K_mass)
summary(fit6K_ord)

### Correcciones ----
MD_Kpk2 <- MD_Kpk %>% 
  mutate(K_09_VALOR = scale(K_09_VALOR)) %>%
  mutate(D_11_P = scale(D_11_P)) %>%
  mutate(G_11 = scale(G_11)) %>%
  mutate(EDAD = scale(EDAD)) %>%
  mutate(TOTAL_PERSONAS = scale(TOTAL_PERSONAS)) %>%
  mutate(D_05 = scale(D_05)) %>%
  mutate(FEX_C = FEX_C)

### 7 AIC: 1477814.78  ----
fit7K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_O + K_10_C + CEDAD + K_12_I + SEXO + 
                     K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                     K_12_K, data = MD_Kpk2, Hess = TRUE, method = "probit", weights = FEX_C)

fit7K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_O + K_10_C + CEDAD + K_12_I + SEXO + 
                   K_11 + K_10_D + K_12_H + K_10_E + K_12_C + K_10_I + TOTAL_PERSONAS + 
                   K_12_K,, data = MD_Kpk2, link = "probit", weights = FEX_C)

summary(fit7K_mass); vif(fit7K_mass)
summary(fit7K_ord)  

fitCKpk <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09, 
                data = MD_Kpk2, Hess = TRUE, method = "probit", weights = FEX_C)
scope <- list(lower=~FG_01+G_02+D_11_P+G_11+D_09, 
              upper=~ K_04+K_09_VALOR+K_10_A+K_10_B+K_10_C+K_10_D+K_10_E+K_10_F+K_10_G+K_10_H+K_10_I+K_11+K_12_A+K_12_B+K_12_C+K_12_D+K_12_E+K_12_F+K_12_G+K_12_H+K_12_I+K_12_J+K_12_K+K_12_L+K_12_M+K_12_N+K_12_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+CEDAD+SEXO+
                FG_01+G_02+D_11_P+G_11+D_09)
stepAIC(fitCKpk, scope=scope, direction = "forward")

### 8 AIC: 1457926.79  ----
fit8K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_G + K_12_H + K_10_C + CEDAD + K_12_O + 
                     K_10_D + SEXO + K_11 + K_12_I + D2_03 + D_08 + K_10_E + 
                     K_10_I + D2_01 + K_10_H + K_12_C + K_12_A + K_12_D + K_12_L + 
                     TOTAL_PERSONAS + K_09_VALOR + K_10_F + K_10_B + K_10_A + 
                     D_10 + K_12_M + K_10_G + K_12_E + K_12_F + D_05 + K_12_B + 
                     K_12_N + K_12_J, data = MD_Kpk2, Hess = TRUE, method = "probit", weights = FEX_C)

fit8K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_G + K_12_H + K_10_C + CEDAD + K_12_O + 
                   K_10_D + SEXO + K_11 + K_12_I + D2_03 + D_08 + K_10_E + 
                   K_10_I + D2_01 + K_10_H + K_12_C + K_12_A + K_12_D + K_12_L + 
                   TOTAL_PERSONAS + K_09_VALOR + K_10_F + K_10_B + K_10_A + 
                   D_10 + K_12_M + K_10_G + K_12_E + K_12_F + D_05 + K_12_B + 
                   K_12_N + K_12_J, data = MD_Kpk2, link = "probit", weights = FEX_C)

summary(fit8K_mass); vif(fit8K_mass)
summary(fit8K_ord)

### 9 AIC:   ----
fit9K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                     D_09 + K_12_G + K_12_H + K_10_C + CEDAD + K_12_O + 
                     K_10_D + SEXO + K_11 + K_12_I + D2_03 + D_08 + K_10_E + 
                     K_10_I + D2_01 + K_10_H + K_12_C + K_12_A + K_12_D + K_12_L + 
                     TOTAL_PERSONAS + K_09_VALOR + K_10_F + K_10_B + K_10_A + 
                     D_10 + K_12_M + K_10_G + K_12_E + K_12_F + D_05 + K_12_B + 
                     K_12_N + K_12_J, data = MD_Kpk2, Hess = TRUE, method = "probit")

fit9K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11_P + G_11 + 
                   D_09 + K_12_G + K_12_H + K_10_C + CEDAD + K_12_O + 
                   K_10_D + SEXO + K_11 + K_12_I + D2_03 + D_08 + K_10_E + 
                   K_10_I + D2_01 + K_10_H + K_12_C + K_12_A + K_12_D + K_12_L + 
                   TOTAL_PERSONAS + K_09_VALOR + K_10_F + K_10_B + K_10_A + 
                   D_10 + K_12_M + K_10_G + K_12_E + K_12_F + D_05 + K_12_B + 
                   K_12_N + K_12_J, data = MD_Kpk2, link = "probit")

summary(fit9K_mass); vif(fit9K_mass)
summary(fit9K_ord)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Validación ----

# Función para categorizar en la matriz de confusión
categorizar <- function(x, Qp) {
  if (x < Qp[1]) {
    return("1")
  } else if (x >=  Qp[1] && x <  Qp[2]) {
    return("2")
  } else if (x >=  Qp[2] && x <  Qp[3]) {
    return("3")
  } else if (x >=  Qp[3] && x <  Qp[4]) {
    return("4")
  } else {
    return("5")
  }
}

#Funciones para obtener los gráficos de accuracy
get_diagonal <- function(mat, shift) {
  n <- nrow(mat)
  m <- ncol(mat)
  if (shift >= 0) {
    return(diag(mat[ , (1+shift):m, drop = FALSE]))
  } else {
    return(diag(mat[(1-shift):n, , drop = FALSE]))
  }
}


CM_cumsum <- function(conf_matrix){
  cumulative_sums <- numeric()
  max_shift <- nrow(conf_matrix) - 1
  
  for (shift in 0:max_shift) {
    main_diag <- sum(get_diagonal(conf_matrix, shift))  # Above or main diagonal
    off_diag <- if (shift > 0) sum(get_diagonal(conf_matrix, -shift)) else 0  # Below main diagonal
    cumulative_sums <- c(cumulative_sums, sum(cumulative_sums[length(cumulative_sums)], main_diag, off_diag, na.rm = TRUE))
  }
  return(cumulative_sums)
}


validacion = function(fit_mass, fit_ord, hat_X){
  ## Multicolinealidad ----
  multicol = car::vif(fit_mass)
  print(multicol)
  
  ## residuales ----
  # Obtain the SBS/probability-scale residuals
  pres <- presid(fit_mass)
  
  p1 <- ggplot(data.frame(y = pres), aes(sample = y)) +
    stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
    xlab("Sample quantile") +
    ylab("Theoretical quantile") + ggtitle("QQ-plot - Marijuana (SBS)"); print(p1)
  
  set.seed(101) # for reproducibility
  sres <- resids(fit_mass)
  
  p2 <- ggplot(data.frame(y = sres), aes(sample = y)) +
    stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
    xlab("Sample quantile") +
    ylab("Theoretical quantile") + ggtitle("QQ-plot - Marijuana (Surrogate)"); print(p2)
  
  residual <- grid.arrange(p1, p2, ncol = 2); residual
  
  ## Matriz de confusión ----
  beta <- fit_ord$beta
  hat_y <- hat_X %*% beta
  Qp <- fit_ord$alpha #los puntos de corte interceptos estimados
  
  categorias <- sapply(hat_y, categorizar, Qp)
  pred <- as.data.frame(cbind(MD_K$K_04, categorias, hat_y))
  
  #matriz de confusión con los valores reales en las filas, predichos columnas
  CM <- table(pred$V1, pred$categorias); print(CM) 
  
  ## Accuracy ----
  Accuracy <- cbind(CM_cumsum(CM)/nrow(hat_X), c(1:5))
  
  plot(y=Accuracy[,1], x=Accuracy[,2])
  plot(x= c(1:5), y=Accuracy[,1], type = "o", col = "blue", pch = 16, ylim = c(0, 1), 
       xlab = "Distancia", ylab = "Presicion", main = "")
  
  results = list(multicol, residual, confusion = CM, Accuracy)
  return(results)
}

# ---------------------------------------------------------------------------- #
#### fit5 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X5 <- MD_Kc %>% dplyr::select(K_04, FG_01 ,G_02 ,D_11_P ,G_11 ,
                                    D_09 ,K_12_O ,K_10_C ,CEDAD ,K_12_I ,SEXO ,
                                    K_11 ,K_10_D ,K_12_H ,K_10_E ,K_12_C ,K_10_I ,TOTAL_PERSONAS ,
                                    K_12_K)
hat_X5 <- model.matrix(K_04 ~ ., hat_X5)
hat_X5 <- hat_X5[,-1] # - el intercepto

val_f5 <- validacion(fit_mass = fit6K_mass, fit_ord = fit6K_ord, hat_X = hat_X5)

#### fit8 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X8 <- MD_Kpk %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, 
                                     D_09, K_12_G, K_12_H, K_10_C, CEDAD, K_12_O, 
                                     K_10_D, SEXO, K_11, K_12_I, D2_03, D_08, K_10_E, 
                                     K_10_I, D2_01, K_10_H, K_12_C, K_12_A, K_12_D, K_12_L, 
                                     TOTAL_PERSONAS, K_09_VALOR, K_10_F, K_10_B, K_10_A, 
                                     D_10, K_12_M, K_10_G, K_12_E, K_12_F, D_05, K_12_B, 
                                     K_12_N, K_12_J)
hat_X8 <- model.matrix(K_04 ~ ., hat_X8)
hat_X8 <- hat_X8[,-1] # - el intercepto

val_f8 <- validacion(fit_mass = fit8K_mass, fit_ord = fit8K_ord, hat_X = hat_X8)

#### fit9 ----
val_f9 <- validacion(fit_mass = fit9K_mass, fit_ord = fit9K_ord, hat_X = hat_X8)

#### fitC ----
fitCK_mass <- polr(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
              data = MD_K, Hess = TRUE, method = "probit")
fitCK_ord  <- clm(factor(K_04) ~ FG_01+G_02+D_11_P+G_11+D_09,
             data = MD_K, link = "probit")

hat_Xc <- MD_K %>% dplyr::select(K_04, FG_01, G_02, D_11_P, G_11, D_09)
hat_Xc <- model.matrix(K_04 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

val_fC <- validacion(fit_mass = fitCK_mass, fit_ord = fitCK_ord, hat_X = hat_Xc)

load("Limpieza_tablas/tablas.RData")
source("Ajuste_de_modelos/variables de control.R")
rm(list = setdiff(ls(), c("C_k", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")
library("ordinal")
library("car")
library("PResiduals")
library("ggplot2")
library("sure")

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

fit0K <- polr(factor(K_04) ~ 1, data = MD_K, Hess = TRUE, method = "probit")
summary(fit0K) #AIC: 3800.169

fitCK <- polr(factor(K_04) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_K, Hess = TRUE, method = "probit")
summary(fitCK) #AIC: 3776.28 

# ---------------------------------------------------------------------------- #

## selección ----
### 1 AIC: 3541.156 ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~ K_04+K_09_VALOR+K_10_A+K_10_B+K_10_C+K_10_D+K_10_E+K_10_F+K_10_G+K_10_H+K_10_I+K_11+K_12_A+K_12_B+K_12_C+K_12_D+K_12_E+K_12_F+K_12_G+K_12_H+K_12_I+K_12_J+K_12_K+K_12_L+K_12_M+K_12_N+K_12_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCK, scope=scope, direction = "forward")

fit1K <- polr(formula = factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                K_10_C + SEXO + K_10_E + D2_05 + K_11 + EDAD + K_10_D + D_01 + 
                D_07 + K_10_I, data = MD_K, Hess = TRUE, method = "probit")
summary(fit1K) #AIC: 3513.382 

PseudoR2(fit1K)

### 2 AIC: 3540.153 ----
fit2K <- polr(formula = factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                K_10_C + D_02 + K_10_E + D2_05 + K_11 + K_10_D + D_01 + K_10_I + 
                TOTAL_PERSONAS, data = MD_K, Hess = TRUE, method = "probit")
summary(fit2K) #AIC: 3540.153 
PseudoR2(fit2K)

### 3  AIC:  3346.052 ----
fit3K_mass <- polr(formula = factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                K_12_O + K_10_C + K_12_I + SEXO + D_02 + K_10_D + K_12_H + 
                K_11 + K_10_E + TOTAL_PERSONAS + K_12_C + EDAD + 
                K_10_I + D_01, data = MD_K, Hess = TRUE, method = "probit")
fit3K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                   K_12_O + K_10_C + K_12_I + SEXO + D_02 + K_10_D + K_12_H + 
                   K_11 + K_10_E + TOTAL_PERSONAS + K_12_C + EDAD + 
                   K_10_I + D_01, data = MD_K, link = "probit")
summary(fit3K_mass)
summary(fit3K_ord)

# Selecciono el mejor modelo (fit3)

# Validacion ----
vif(fit3K_mass)

## residuales ----
# Obtain the SBS/probability-scale residuals
pres <- presid(fit3K_mass)

p1 <- ggplot(data.frame(y = pres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
  xlab("Sample quantile") +
  ylab("Theoretical quantile") + ggtitle("QQ-plot - Marijuana (SBS)");p1

set.seed(101) # for reproducibility
sres <- resids(fit3K_mass)

p2 <- ggplot(data.frame(y = sres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
  xlab("Sample quantile") +
  ylab("Theoretical quantile") + ggtitle("QQ-plot - Marijuana (Surrogate)"); p2

grid.arrange(p1, p2, ncol = 2)

## matriz de confusión ----
# Copiado del modelo de efectos mixtos
# Función para categorizar
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

### fit3 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X <- MD_K %>% dplyr::select(K_04, FG_01, G_02, D_11, G_11, D_09, 
                         K_12_O, K_10_C, K_12_I, SEXO, D_02, K_10_D, K_12_H, 
                         K_11, K_10_E, TOTAL_PERSONAS, K_12_C, EDAD, 
                         K_10_I, D_01)
hat_X <- model.matrix(K_04 ~ ., hat_X)
hat_X <- hat_X[,2:28] # - el intercepto

beta <- fit3K_ord$beta

hat_y <- hat_X %*% beta
Qp <- fit3K_ord$alpha #los puntos de corte interceptos estimados

categorias <- sapply(hat_y, categorizar, Qp)
pred <- as.data.frame(cbind(MD_K$K_04, categorias, hat_y))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_fit3 <- table(pred$V1, pred$categorias)

### fitC ----
fitCK <- clm(factor(K_04) ~ FG_01+G_02+D_11+G_11+D_09,
             data = MD_K, link = "probit")

hat_Xc <- MD_K %>% dplyr::select(K_04, FG_01, G_02, D_11, G_11, D_09)
hat_Xc <- model.matrix(K_04 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

beta_c <- fitCK$beta
hat_yc <- hat_Xc %*% beta_c
Qp_c <- fitCK$alpha #los puntos de corte intercepto estimados

categorias_c <- sapply(hat_yc, categorizar, Qp_c)
pred_control <- as.data.frame(cbind(MD_K$K_04, categorias_c, hat_yc))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_control <- table(pred_control$V1, pred_control$categorias)


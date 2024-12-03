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
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))

summary(MD_L) #Los que contestaron 9 en la pregunta original quedan como NA's
MD_L <- MD_L[complete.cases(MD_L),]

# ---------------------------------------------------------------------------- #

fit0L <- polr(factor(L_03) ~ 1, data = MD_L, Hess = TRUE, method = "probit")
summary(fit0L) #AIC: 752.7631 

fitCL <- polr(factor(L_03) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_L, Hess = TRUE, method = "probit")
summary(fitCL) #AIC: 761.1499 

# ---------------------------------------------------------------------------- #
## seleccion ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~ L_03+ L_08_VALOR+ L_09_A+ L_09_B+ L_09_C+ L_09_D+ L_09_E+ L_09_F+ L_09_G+ L_09_H+ L_09_I+ L_10_A+ L_10_B+ L_11_A+L_11_B+L_11_C+L_11_D+L_11_E+L_11_F+L_11_G+L_11_H+L_11_I+L_11_J+L_11_K+L_11_L+L_11_M+L_11_N+L_11_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCL, scope=scope, direction = "forward")


### 2 AIC: 738.6616 ----
fit2L_mass <- polr(formula = factor(L_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
       L_09_C + EDAD + D_07 + D_02, data = MD_L, Hess = TRUE, method = "probit")
summary(fit2L_mass)  

fit2L_ord <- clm(formula = factor(L_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                     L_09_C + EDAD + D_07 + D_02, data = MD_L, link = "probit")
summary(fit2L_ord) #AIC: 738.6616 


### 3   AIC: 703.1233 ----
fit3L_mass <- polr(formula = factor(L_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
       L_11_O + L_11_A + EDAD + L_09_C + L_11_H + L_11_I,
     data = MD_L, Hess = TRUE, method = "probit")
summary(fit3L_mass)

fit3L_ord <- clm(formula = factor(L_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                   EDAD + L_09_C + L_11_A + L_11_O + L_11_H + L_11_I,
                   data = MD_L, link = "probit")
summary(fit3L_ord)

#  Validación ----
vif(fit3L_mass)

## residuales ----
pres <- presid(fit3L_mass)

p1 <- ggplot(data.frame(y = pres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
  xlab("Sample quantile") +
  ylab("Theoretical quantile") + ggtitle("QQ-plot - Cocaine (SBS)");p1

set.seed(101) # for reproducibility
sres <- resids(fit3L_mass)

p2 <- ggplot(data.frame(y = sres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
  xlab("Sample quantile") +
  ylab("Theoretical quantile") + ggtitle("QQ-plot - Cocaine (Surrogate)"); p2

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
hat_X <- MD_L %>% dplyr::select(L_03, FG_01 ,G_02 ,D_11 ,G_11 ,D_09 ,
                                EDAD ,L_09_C ,L_11_A ,L_11_O ,L_11_H ,L_11_I)
hat_X <- model.matrix(L_03 ~ ., hat_X)
hat_X <- hat_X[,-1] # - el intercepto

beta <- fit3L_ord$beta

hat_y <- hat_X %*% beta
Qp <- fit3L_ord$alpha #los puntos de corte interceptos estimados

categorias <- sapply(hat_y, categorizar, Qp)
pred <- as.data.frame(cbind(MD_L$L_03, categorias, hat_y))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_fit3 <- table(pred$V1, pred$categorias)

### fitC ----
fitCL <- clm(factor(L_03) ~ FG_01+G_02+D_11+G_11+D_09,
             data = MD_L, link = "probit")

hat_Xc <- MD_L %>% dplyr::select(L_03, FG_01, G_02, D_11, G_11, D_09)
hat_Xc <- model.matrix(L_03 ~ ., hat_Xc)
hat_Xc <- hat_Xc[,-1] # - el intercepto

beta_c <- fitCL$beta
hat_yc <- hat_Xc %*% beta_c
Qp_c <- fitCL$alpha #los puntos de corte intercepto estimados

categorias_c <- sapply(hat_yc, categorizar, Qp_c)
pred_control <- as.data.frame(cbind(MD_L$L_03, categorias_c, hat_yc))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_control <- table(pred_control$V1, pred_control$categorias)


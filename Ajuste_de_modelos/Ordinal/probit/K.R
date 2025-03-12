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

fit3K_ord <- clm(formula = factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                  K_12_O + K_10_C + K_12_I + SEXO + D_02 + K_10_D + K_12_H + 
                  K_11 + K_10_E + TOTAL_PERSONAS + K_12_C + EDAD + 
                  K_10_I + D_01, data = MD_K, link = "probit")
summary(fit3K_mass)
summary(fit3K_ord)

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
fitCKc <- polr(factor(K_04) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_Kc, Hess = TRUE, method = "probit")
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09, 
              upper=~ K_04+K_09_VALOR+K_10_A+K_10_B+K_10_C+K_10_D+K_10_E+K_10_F+K_10_G+K_10_H+K_10_I+K_11+K_12_A+K_12_B+K_12_C+K_12_D+K_12_E+K_12_F+K_12_G+K_12_H+K_12_I+K_12_J+K_12_K+K_12_L+K_12_M+K_12_N+K_12_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+CEDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCKc, scope=scope, direction = "forward")


### 4 AIC: 3297.959 ----
#!!! Multicolinealidad, no converge
fit4K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                    K_12_O + K_10_C + CEDAD + K_12_I + D2_05 + K_10_D + SEXO + 
                    K_11 + K_12_H + K_10_E + K_12_C + TOTAL_PERSONAS + K_10_I + 
                    D_01 + K_12_K, data = MD_Kc, Hess = TRUE, method = "probit")

fit4K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                    K_12_O + K_10_C + CEDAD + K_12_I + D2_05 + K_10_D + SEXO + 
                    K_11 + K_12_H + K_10_E + K_12_C + TOTAL_PERSONAS + K_10_I + 
                    D_01 + K_12_K, data = MD_Kc, link = "probit")
summary(fit4K_mass)
summary(fit4K_ord)

### 5 AIC: 3333.79  ----
fit5K_mass <- polr(factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                     K_12_O + K_10_C + CEDAD + K_12_I + K_10_D + SEXO + 
                     K_11 + K_12_H + K_10_E + K_12_C + TOTAL_PERSONAS + K_10_I + 
                     D_01 + K_12_K, data = MD_Kc, Hess = TRUE, method = "probit")

fit5K_ord <- clm(factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                   K_12_O + K_10_C + CEDAD + K_12_I + K_10_D + SEXO + 
                   K_11 + K_12_H + K_10_E + K_12_C + TOTAL_PERSONAS + K_10_I + 
                   D_01 + K_12_K, data = MD_Kc, link = "probit")
summary(fit5K_mass)
summary(fit5K_ord)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Validación ----

validacion = function(fit_mass, fit_ord){
  ## Multicolinealidad ----
  multicolinealidad = vif(fit_mass)
  print(multicolinealidad)
  
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
  
  grid.arrange(p1, p2, ncol = 2)
  
}


## residuales ----
### fit3 ----

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


### fit5 ----
# Obtain the SBS/probability-scale residuals
pres_f5 <- presid(fit5K_mass)

p1 <- ggplot(data.frame(y = pres_f5), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "#6B8E23", linewidth = 1, linetype= "dashed") +
  xlab("Sample quantile") +
  ylab("Theoretical quantile") + ggtitle("QQ-plot - Marijuana (SBS)");p1

set.seed(101) # for reproducibility
sres_f5 <- resids(fit5K_mass)

p2 <- ggplot(data.frame(y = sres_f5), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "#6B8E23", linewidth = 1, linetype= "dashed") +
  xlab("Sample quantile") +
  ylab("Theoretical quantile") + ggtitle("QQ-plot - Marijuana (Surrogate)"); p2

grid.arrange(p1, p2, ncol = 2)


# ---------------------------------------------------------------------------- #

## matriz de confusión ----
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

hat_y_f3 <- hat_X %*% beta
Qp <- fit3K_ord$alpha #los puntos de corte interceptos estimados

categorias_f3 <- sapply(hat_y_f3, categorizar, Qp)
pred_f3 <- as.data.frame(cbind(MD_K$K_04, categorias_f3, hat_y_f3))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_fit3 <- table(pred_f3$V1, pred_f3$categorias_f3)


### fit5 ----
#creo la matriz diseño con solo las variables seleccionadas en el modelo
hat_X <- MD_Kc %>% dplyr::select(K_04, FG_01, G_02, D_11, G_11, D_09, 
                                   K_12_O, K_10_C, CEDAD, K_12_I, K_10_D, SEXO, 
                                   K_11, K_12_H, K_10_E, K_12_C, TOTAL_PERSONAS, K_10_I, 
                                   D_01, K_12_K,)
hat_X <- model.matrix(K_04 ~ ., hat_X)
hat_X <- hat_X[,-1] # - el intercepto

beta <- fit5K_ord$beta

hat_y_f5 <- hat_X %*% beta
Qp <- fit5K_ord$alpha #los puntos de corte interceptos estimados

categorias_f5 <- sapply(hat_y_f5, categorizar, Qp)
pred_f5 <- as.data.frame(cbind(MD_Kc$K_04, categorias_f5, hat_y_f5))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_fit5 <- table(pred_f5$V1, pred_f5$categorias_f5)

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


#######
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

Accuracy_fit3 <- cbind(CM_cumsum(CM_fit3)/1210, c(1:5))
Accuracy_fit5 <- cbind(CM_cumsum(CM_fit5)/1210, c(1:5))
CM_control1 <- cbind("1"=rep(0, 5), CM_control)
Accuracy_fitC <- cbind(CM_cumsum(CM_control1)/1210, c(1:5))

plot(y=Accuracy_fit3[,1], x=Accuracy_fit3[,2])
plot(y=Accuracy_fit5[,1], x=Accuracy_fit5[,2])
plot(y=Accuracy_fitC[,1], x=Accuracy_fitC[,2])

plot(x= c(1:5), y=Accuracy_fit3[,1], type = "o", col = "blue", pch = 16, ylim = c(0, 1), 
     xlab = "Distancia", ylab = "Accuracy", main = "")
lines(x= c(1:5), y= Accuracy_fit5[,1], type = "o", col = "red", pch = 17)
lines(x= c(1:5), y= Accuracy_fitC[,1], type = "o", col = "green", pch = 18)

# Agregar leyenda
legend("bottomright", legend = c("fit 3", "fit 5", "Control"), 
       col = c("blue", "red", "green"), pch = c(16, 17, 18), lty = 1)

beta_c <- fitCK$beta
hat_yc <- hat_Xc %*% beta_c
Qp_c <- fitCK$alpha #los puntos de corte intercepto estimados

categorias_c <- sapply(hat_yc, categorizar, Qp_c)
pred_control <- as.data.frame(cbind(MD_K$K_04, categorias_c, hat_yc))

#matriz de confusión con los valores reales en las filas, predichos columnas
CM_control <- table(pred_control$V1, pred_control$categorias)

load("Limpieza_tablas/tablas.RData")
source("Ajuste_de_modelos/variables de control.R")
rm(list = setdiff(ls(), c("C_k", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")
library("ordinal")
library("car")
library("ggplot2")
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Co-variables ----
X <- d %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`DIRECTORIO`, `Depmuni`, `D_01`, `D_02`, `D_07`, `D_08`, `D_10`,
                `D2_01`, `D2_03`, `D2_05`, SEXO, EDAD, TOTAL_PERSONAS, `D_05`) %>%
  mutate_at(vars(3:11), as.factor) %>%
  mutate_at(vars(12, 14), as.numeric)

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

fitK_mm <- clmm2(factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                K_12_O + K_10_C + K_12_I + SEXO + D_02 + K_10_D + K_12_H + 
                K_11 + K_10_E + TOTAL_PERSONAS + K_12_C + EDAD + 
                K_10_I + D_01, 
              random = factor(Depmuni), data = MD_K, 
              Hess = TRUE, 
              link = "probit")

summary(fitK_mm)
summary(fit3K_ord) #Ajuste de modelos/ordinal/probit/K

## pruebas ----
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

#creo la matriz diseno con solo las variables seleccionadas en el modelo
hat_X <- MD_K %>% select(K_04, FG_01, G_02, D_11, G_11, D_09, 
                         K_12_O, K_10_C, K_12_I, SEXO, D_02, K_10_D, K_12_H, 
                         K_11, K_10_E, TOTAL_PERSONAS, K_12_C, EDAD, 
                         K_10_I, D_01)
hat_X <- model.matrix(K_04 ~ ., hat_X)
hat_X <- hat_X[,2:28] # - el intercepto

### Estimación para el modelo de efectos fijos ----
betaF <- fit3K_ord$beta

hat_yF <- qnorm(fitted(fit3K_ord))
QpF <- fit3K_ord$Theta #los puntos de corte interceptos estimados
QpF <- c(-Inf, QpF, Inf)  # Agregar -Inf y Inf para comparar adecuadamente

categoriasF1 <- sapply(hat_yF, asignar_categoria, QpM = QpF)
#pred_fijos <- as.data.frame(cbind(MD_K$K_04, categoriasF, hat_yF, categorias_predichas2, categorias_predichas))

qnorm(0.5, 4, 1)
qnorm(QpF)
#matriz de confusión con los valores reales en las filas, predichos columnas
CM_fijos <- table(pred_fijos$V1, pred_fijos$categoriasF, )


### Estimación para el modelo de efectos aleatorios ----
hat_yM <- qnorm(fitted(fitK_mm))


QpM <- fitK_mm$Theta
QpM <- c(-Inf, QpM, Inf)  # Agregar -Inf y Inf para comparar adecuadamente

# Función para asignar categoría basada en los puntos de corte
asignar_categoria <- function(hat_yM, QpM) {
  # Buscar el intervalo en el que cae el valor ajustado
  categoria <- findInterval(hat_yM, QpM)
  return(categoria)
}

# Aplicar la función a cada valor ajustado
categoriasM <- sapply(hat_yM, asignar_categoria, QpM = QpM)
pred_mixtos <- as.data.frame(cbind(MD_K$K_04, categoriasF, categoriasF1, categoriasM))


# Bondad de Ajuste ---- 
puntaje <- function(mc){
  
  v  <- c(mc[1,2], mc[2,3], mc[3,4], mc[4,5], 
          mc[2,1], mc[3,2], mc[4,3], mc[5,4])
  a  <- c(mc[1,3], mc[2,4], mc[3,5], 
          mc[3,1], mc[4,2], mc[5,3])
  m  <- c(mc[1,4], mc[2,5], 
          mc[4,1], mc[5,2])
  r  <- c(mc[5,1], mc[1,5])
  
  #Pesos
  pv <- 0.05
  pa <- 0.1
  pm <- 0.15
  pr <- 0.2
  
  puntaje <- sum(c(pv*v, pa*a, pm*m, pr*r))
  
  return(puntaje)
}

load("Limpieza_tablas/tablas.RData")

#Carga de librerías
library("dplyr")
library("MASS")
library("brant")      #pruebas de hipótesis
library("ordinal")    #ajustar modelo 2da manera
library("DescTools")  #obtener R2
library("generalhoslem") #test de bondad de ajuste

# ---------------------------------------------------------------------------- #
# Para las drogas que apliquen se utiliza como variable respuesta (Y) la 
# frecuencia de consumo en el ultimo año
# ---------------------------------------------------------------------------- #

# Co-variables ----
X <- d %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(entorno,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`DIRECTORIO`, `D_01`, `D_02`, `D_04`, `D_08`, `D_09`, `D_10`,
         `D2_01`, `D2_03`, `D2_05`, ESTRATO, `G_01`, `G_02`,
         `G_06_A`, `G_06_B`, `G_06_C`, `G_06_D`, `G_06_E`, `G_06_F`, `G_06_G`,
         `G_06_H`, `G_06_I`, `G_06_J`, `G_06_K`, `G_06_L`, `G_06_M`, `G_06_N`,
         `D_05`, TOTAL_PERSONAS) %>%
  mutate_at(vars(1:27), as.factor) %>%
  mutate_at(vars(28,29), as.numeric)

summary(X)

# ---------------------------------------------------------------------------- #
# J - Inhalables(1) ----
# ---------------------------------------------------------------------------- #
MD <- C_j_1 %>% 
  filter(J_03 != "na") %>% #Y. borrando los registros de na
  mutate(J_03 = factor(J_03, levels= c(1,2,3,4,5)))

fit0J <- polr(factor(J_03) ~ 1, data = MD, Hess = TRUE, method = "logistic")
summary(fit0J) #AIC: 84.16586 

## relacionadas ----
# Matriz diseño con solo las variables relacionadas
MD <- C_j_1 %>% 
  filter(J_03 != "na") %>% #Y. borrando los registros de na
  mutate(J_03 = factor(J_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(-`J_01`, -`J_02`, -`J_04`, -`J_05_D`, -`J_06_A`, -`J_06_E`, -`J_06_I`, -DIRECTORIO) %>%
  mutate_at(vars(1:12), as.factor)
  #Se eliminan las variables que son constantes y las que no son relevantes

summary(MD)

### fit 1 ----
fit1J <- polr(J_03 ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit1J) #AIC: 94.20705

anova(fit0J, fit1J)

# PRUEBAS 
brant(fit1J) #verificar odds proporcionales
PseudoR2(fit1J) #varianza explicada por el modelo

### fit 2 ----
stepAIC(fit1J, direction = "both")

fit2J <- polr(formula = J_03 ~ J_05_C + J_06_C + J_06_F + J_06_G + J_06_H, 
     data = MD, Hess = TRUE, method = "logistic")

anova(fit0J, fit2J)

# PRUEBAS 
brant(fit2J) #verificar odds proporcionales
PseudoR2(fit2J) #varianza explicada por el modelo

## socio-demográficas ----
# Matriz diseño con solo las variables socio-demográficas
MD <- C_j_1 %>% 
  filter(J_03 != "na") %>% #Y. borrando los registros de na
  mutate(J_03 = factor(J_03, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`J_03`, `D_01`, `D_08`, `D_09`, `D_10`,
         `D2_01`, `D2_03`, `D2_05`, ESTRATO, TOTAL_PERSONAS, `G_01`, `G_02`) %>%
  filter(complete.cases(.))

summary(MD)
### fit 3 (X)----

fit3J <- polr(J_03 ~ ., data = MD, Hess = TRUE, method = "logistic", start = )
summary(fit3J) #AIC: 60.00133 

#anova(fit0, fit2) 
#NO se puede realizar la prueba anova porque estrato tiene 1 NA

# PRUEBAS 
brant(fit3J) #verificar odds proporcionales
PseudoR2(fit3J) #varianza explicada por el modelo

### fit 4 (X)----
# Matriz diseño con algunas variables socio-demográficas
MD <- C_j_1 %>% 
  filter(J_03 != "na") %>% #Y. borrando los registros de na
  mutate(J_03 = factor(J_03, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`J_03`, `D2_05`, `J_05_A`, `J_05_B`, `J_05_C`, `J_05_E`, `J_05_F`,
         `J_06_B`, `J_06_C`, `J_06_D`, `J_06_F`, `J_06_G`, `J_06_H`) %>%
  mutate_at(vars(1:13), as.factor)

summary(MD)

fit4J <- polr(J_03 ~ ., data = MD, Hess = TRUE, method = "logistic")

# ---------------------------------------------------------------------------- #
# J - Inhalables(2) ----
# ---------------------------------------------------------------------------- #
MD <- C_j_2 %>% 
  filter(J_09 != "na") %>% #Y. borrando los registros de na
  mutate(J_09 = factor(J_09, levels= c(1,2,3,4,5)))

fit0J2 <- polr(J_09 ~ 1, data = MD, Hess = TRUE, method = "logistic")
summary(fit0J2) #AIC: 41.52553  

¡
## socio demográficas ----
# Matriz diseño con solo las variables socio demográficas
MD <- C_j_2 %>% 
  filter(J_09 != "na") %>% #Y. borrando los registros de na
  mutate(J_09 = factor(J_09, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`J_09`, `D_01`, `D_02`, `D_08`, `D_09`, `D_10`,
         `D2_01`, `D2_05`, ESTRATO, TOTAL_PERSONAS, `G_01`, `G_02`)

summary(MD)

fit1J2 <- polr(J_09 ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit1J2) #AIC: 34.00947  

anova(fit0J2, fit1J2)

# PRUEBAS 
brant(fit1J2) #verificar odds proporcionales
PseudoR2(fit1J2) #varianza explicada por el modelo

# ---------------------------------------------------------------------------- #
# K - marihuana ----
# ---------------------------------------------------------------------------- #
MD <- C_k %>% 
  filter(K_04 != "na" & K_04 != "9") %>% #Y. borrando los registros de na
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5)))

fit0K <- polr(K_04 ~ 1, data = MD, Hess = TRUE, method = "logistic")
summary(fit0K) #AIC: 3800.169  

## relacionadas ----
# Matriz diseño con solo las variables relacionadas
MD <- C_k %>% 
  filter(K_04 != "na" & K_04 != "9") %>% #Y. borrando los registros de na
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  dplyr::select(`K_01_A`, `K_01_B`, `K_01_C`, `K_01_D`, `K_10_A`, `K_10_B`,
         `K_10_C`, `K_10_D`, `K_10_E`, `K_10_F`, `K_10_G`, `K_10_H`, `K_10_I`, 
         `K_04`, `K_08`,`K_09_VALOR`) %>%
  mutate_at(vars(1:14), as.factor)
#Se eliminan las variables que son constantes y las que no son relevantes

summary(MD)

### fit 1 ----
fit1K <- polr(K_04 ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit1K) #AIC: 3026.506

anova(fit0K, fit1K)

# PRUEBAS 
brant(fit1K) #verificar odds proporcionales
PseudoR2(fit1K) #varianza explicada por el modelo

## socio-demográficas ----
# Matriz diseño con solo las variables socio-demográficas
MD <- C_k %>% 
  filter(K_04 != "na" & K_04 != "9") %>% 
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`K_04`, `D_01`, `D_02`, `D_04`, `D_05`, `D_08`, `D_09`, `D_10`,
         `D2_01`, `D2_03`, `D2_05`, ESTRATO, TOTAL_PERSONAS, `G_01`, `G_02`,
         `G_06_A`, `G_06_B`, `G_06_C`, `G_06_D`, `G_06_E`, `G_06_F`, `G_06_G`,
         `G_06_H`, `G_06_I`, `G_06_J`, `G_06_K`, `G_06_L`, `G_06_M`, `G_06_N`)

fit2 <- polr(factor(K_04) ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit2) #AIC: 3672.805 

### fit3 ----
# Matriz diseño con algunas variables socio-demográficas
MD <- C_k %>% 
  filter(K_04 != "na" & K_04 != "9") %>% 
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`K_04`, `K_01_A`, `K_01_B`, `K_01_C`, `K_01_D`, `K_10_A`, `K_10_B`,
         `K_10_C`, `K_10_D`, `K_10_E`, `K_10_F`, `K_10_G`, `K_10_H`, `K_10_I`, 
         `K_11_A`, `K_11_B`, `K_11_C`, `K_11_D`, `K_11_E`, `D_01`, `D_04`,
         `D2_05`, `G_01`, `G_02`, `G_06_A`, `G_06_B`, `G_06_C`, `G_06_D`, `G_06_E`, 
         `G_06_F`, `G_06_G`, `G_06_H`, `G_06_I`, `G_06_J`, `G_06_K`, `G_06_L`, `G_06_M`, `G_06_N`)

fit2 <- polr(factor(K_04) ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit2) #AIC: 3343.512  

anova(fit0, fit2)


## L - Cocaína ----
MD <- C_l %>% 
  filter(L_03 != "na" & L_03 != "9") %>% #Y. borrando los registros de na
  mutate(L_03 = factor(L_03, levels= c(1,2,3,4,5)))

fit0 <- polr(factor(L_03) ~ 1, data = MD, Hess = TRUE, method = "logistic")
summary(fit0) #AIC: 752.7631  

### (!) relacionadas  ----
# Matriz diseño con solo las variables relacionadas
MD <- C_l %>% 
  filter(L_03 != "na" & L_03 != "9") %>% #Y. borrando los registros de na
  mutate(L_03 = factor(L_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(`L_03`, `L_08_VALOR`, `L_10_A`, `L_10_B`, `L_09_A`, `L_09_B`, `L_09_C`,
         `L_09_D`, `L_09_E`, `L_09_F`, `L_09_G`, `L_09_H`, `L_09_I`)

fit1 <- polr(factor(L_03) ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit1) #Error?

### (!!) socio-demográficas ----
# Matriz diseño con solo las variables socio-demográficas
MD <- C_l %>% 
  filter(L_03 != "na" & L_03 != "9") %>% 
  mutate(L_03 = factor(L_03, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`L_03`, `D_01`, `D_02`, `D_04`, `D_05`, `D_08`, `D_09`, `D_10`,
         `D2_01`, `D2_03`, `D2_05`, ESTRATO, TOTAL_PERSONAS, `G_01`, `G_02`,
         `G_06_A`, `G_06_B`, `G_06_C`, `G_06_D`, `G_06_E`, `G_06_F`, `G_06_G`,
         `G_06_H`, `G_06_I`, `G_06_J`, `G_06_K`, `G_06_L`, `G_06_M`, `G_06_N`)

fit2 <- polr(factor(L_03) ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit2) 

## M - Basuco ----
MD <- C_m %>% 
  filter(M_03 != "na" & M_03 != "9") %>% #Y. borrando los registros de na
  mutate(M_03 = factor(M_03, levels= c(1,2,3,4,5)))

fit0 <- polr(factor(M_03) ~ 1, data = MD, Hess = TRUE, method = "logistic")
summary(fit0) #AIC: 180.5576   

### relacionadas ----
MD <- C_m %>% 
  filter(M_03 != "na" & M_03 != "9") %>% #Y. borrando los registros de na
  mutate(M_03 = factor(M_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(`M_03`, `M_08_VALOR`, `M_09_C`, `M_09_D`, `M_09_E`, `M_09_F`, `M_09_G`)

fit1 <- polr(factor(M_03) ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit1) #AIC: 184.4761 

anova(fit0, fit1)

### socio-demográficas ----
# Matriz diseño con solo las variables socio-demográficas
MD <- C_m %>% 
  filter(M_03 != "na" & M_03 != "9") %>% 
  mutate(M_03 = factor(M_03, levels= c(1,2,3,4,5))) %>%
  left_join(X,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`M_03`, `D_01`, `D_02`, `D_04`, `D_05`, `D_08`, `D_09`, `D_10`,
         `D2_01`, `D2_03`, `D2_05`, ESTRATO, TOTAL_PERSONAS, `G_01`, `G_02`,
         `G_06_A`, `G_06_B`, `G_06_C`, `G_06_D`, `G_06_E`, `G_06_F`, `G_06_G`,
         `G_06_H`, `G_06_I`, `G_06_J`, `G_06_K`, `G_06_L`, `G_06_M`, `G_06_N`)

fit2 <- polr(factor(M_03) ~ ., data = MD, Hess = TRUE, method = "logistic")
summary(fit2) #AIC: 116.0947

load("Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_j_1", "C_j_2", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("ordinal")
library("DescTools")
# ---------------------------------------------------------------------------- #
# Para las drogas que apliquen se utiliza como variable respuesta (Y) la 
# frecuencia de consumo en el ultimo año
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
# J - Inhalables(1) ----
# ---------------------------------------------------------------------------- #
MD_J <- C_j_1 %>% 
  filter(J_03 != "na") %>% #Y. borrando los registros de na
  mutate(J_03 = factor(J_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(-J_01, -J_02, -J_04) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))

fit0J <- polr(factor(J_03) ~ 1, data = MD_J, Hess = TRUE, method = "probit")
summary(fit0J) #AIC: 84.16586 

fitCJ <- polr(factor(J_03) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_J, Hess = TRUE, method = "probit")
summary(fitCJ) #AIC: 86.68526 

## Seleccion ----

### 1 AIC: 83.90639----
#var relacionadas
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~J_05_A+J_05_B+J_05_C+J_05_D+J_05_E+J_05_F+J_06_A+J_06_B+J_06_C+J_06_D+J_06_E+J_06_F+J_06_G+J_06_H+J_06_I+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCJ, scope=scope, direction = "forward")

fit1J_mass <- polr(formula = factor(J_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                J_05_B + J_06_C, data = MD_J, Hess = TRUE, method = "probit")
summary(fit1J_mass) 


fit1J_ord <- clm(formula = factor(J_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                     J_05_B + J_06_C, data = MD_J, link = "probit")
summary(fit1J_ord)

### 2 (!!)AIC: 83.69355 ----
# var sociodemograficas
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCJ, scope=scope, direction = "forward")

fit2J <- polr(formula = factor(J_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                D_08, data = MD_J, Hess = TRUE, method = "probit")
summary(fit2J)# AIC: 83.69355 

### 3 (!!)AIC: 79.78903 ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~J_05_A+J_05_B+J_05_C+J_05_D+J_05_E+J_05_F+J_06_A+J_06_B+J_06_C+J_06_D+J_06_E+J_06_F+J_06_G+J_06_H+J_06_I+
                    D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                    FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCJ, scope=scope, direction = "forward")

fit3J <- polr(formula = factor(J_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                D_08 + J_06_C + TOTAL_PERSONAS + D_05, data = MD_J, Hess = TRUE, 
              method = "probit")
summary(fit3J) #AIC: 79.78903 

### 4 (!)AIC: 75.59737  ----
fit4J_mass <- polr(formula = factor(J_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
              TOTAL_PERSONAS + J_05_F + J_06_D + J_06_B + J_06_G, 
              data = MD_J, Hess = TRUE, method = "probit")
summary(fit4J_mass) # AIC: 75.59737 

fit4J_ord <- clm(formula = factor(J_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 +
                  TOTAL_PERSONAS + J_05_F + J_06_D + J_06_B + J_06_G, 
                   data = MD_J, link = "probit")
summary(fit4J_ord)

#### pruebas ----
vif(fit4J_mass)

# ---------------------------------------------------------------------------- #
# J - Inhalables(2) ----
# ---------------------------------------------------------------------------- #
MD_J2 <- C_j_2 %>% 
  filter(J_09 != "na") %>% #Y. borrando los registros de na
  mutate(J_09 = factor(J_09, levels= c(1,2,3,4,5))) %>%
  dplyr::select(J_09, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))
summary(MD_J2)

fit0J2 <- polr(factor(J_09) ~ 1, data = MD_J2, Hess = TRUE, method = "probit")
summary(fit0J2) #AIC: 39.52552 

fitCJ2 <- polr(factor(J_09) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_J2, Hess = TRUE, method = "probit")
summary(fitCJ2) #AIC: 47.4908 

## selección ----
### 1 (!!) AIC: 22.05236 ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCJ2, scope=scope, direction = "forward")

fit1J2_mass <- polr(formula = factor(J_09) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                 D_07 + SEXO, data = MD_J2, Hess = TRUE, method = "probit")
summary(fit1J2_mass) 

fit1J2_ord <- clm(formula = factor(J_09) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                     D_07 + SEXO, data = MD_J2, link = "probit")
summary(fit1J2_ord)

#### pruebas ----
VIF(fit1J2_mass)
PseudoR2(fit1J2)
         
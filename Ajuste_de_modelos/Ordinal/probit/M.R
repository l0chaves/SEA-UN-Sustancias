load("Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_m", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")
library("ordinal")

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

MD_M <- C_m %>% 
  filter(M_03 != "na") %>% #Y. borrando los registros de na
  mutate(M_03 = factor(M_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(M_03, M_08_VALOR, M_09_A, M_09_B, M_09_C, M_09_D, M_09_E, M_09_F, M_09_G, M_09_H, M_09_I, M_10_A, M_10_B, M_10_C, M_10_D, M_10_E, M_10_F, M_10_G, M_10_H, M_10_I, M_10_J, M_10_K, M_10_L, M_10_M, M_10_N, M_10_O, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))

summary(MD_M)

# ---------------------------------------------------------------------------- #

fit0M <- polr(factor(M_03) ~ 1, data = MD_M, Hess = TRUE, method = "probit")
summary(fit0M) #AIC: 180.5576

fitCM <- polr(factor(M_03) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_M, Hess = TRUE, method = "probit")
summary(fitCM) #AIC: 187.5847 

# ---------------------------------------------------------------------------- #
## seleccion ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~ M_08_VALOR+ M_09_A+ M_09_B+ M_09_C+ M_09_D+ M_09_E+ M_09_F+ M_09_G+ M_09_H+ M_09_I+ M_10_A+M_10_B+M_10_C+M_10_D+M_10_E+M_10_F+M_10_G+M_10_H+M_10_I+M_10_J+M_10_K+M_10_L+M_10_M+M_10_N+M_10_O+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCM, scope=scope, direction = "forward")

### 1  AIC: 161.4985 ----
fit1M_mass <- polr(formula = factor(M_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                M_10_O + D2_01 + TOTAL_PERSONAS, data = MD_M, Hess = TRUE, 
              method = "probit")
summary(fit1M_mass) 


fit1M_ord <- clm(formula = factor(M_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                     M_10_O + D2_01 + TOTAL_PERSONAS, data = MD_M, link = "probit")
summary(fit1M_ord) 

#### pruebas ----
vif(fit1M_mass)

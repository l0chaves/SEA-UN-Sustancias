load("C:/Users/luluf/OneDrive - Universidad Nacional de Colombia/Semillero/Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_k", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Co-variables ----
X <- d %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`DIRECTORIO`, `D_01`, `D_02`, `D_07`, `D_08`, `D_10`,
                `D2_01`, `D2_03`, `D2_05`, TOTAL_PERSONAS, `D_05`) %>%
  mutate_at(vars(2:9), as.factor) %>%
  mutate_at(vars(10, 11), as.numeric)

summary(X)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

MD_K <- C_k %>% 
  filter(K_04 != "na") %>% #Y. borrando los registros de na
  mutate(K_04 = factor(K_04, levels= c(1,2,3,4,5))) %>%
  dplyr::select(K_04, K_09_VALOR, K_10_A, K_10_B, K_10_C, K_10_D, K_10_E, K_10_F, K_10_G, K_10_H, K_10_I, K_11, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate_at(vars(3:12), as.factor)
  
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
              upper=~ K_04+K_09_VALOR+K_10_A+K_10_B+K_10_C+K_10_D+K_10_E+K_10_F+K_10_G+K_10_H+K_10_I+K_11+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCK, scope=scope, direction = "forward")

fit1K <- polr(formula = factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + K_10_C + D_02 + 
              K_10_E + D2_05 + K_11 + K_10_D + D_01 + K_10_I, data = MD_K, Hess = TRUE, 
              method = "probit")
summary(fit1K) #AIC: 3541.156 
PseudoR2(fit1K)

### 2 AIC: 3540.153 ----
fit2K <- polr(formula = factor(K_04) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                K_10_C + D_02 + K_10_E + D2_05 + K_11 + K_10_D + D_01 + K_10_I + 
                TOTAL_PERSONAS, data = MD_K, Hess = TRUE, method = "probit")
summary(fit2K) #AIC: 3540.153 
PseudoR2(fit2K)

load("Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_n", "control", "d", "d2", "encuestas")))

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

MD_N <- C_n %>% 
  filter(N_03 != "na") %>% #Y. borrando los registros de na
  mutate(N_03 = factor(N_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(N_03, N_09, N_10_A, N_10_B, N_10_C, N_10_D, N_10_E, N_10_F, N_10_G, N_10_H, N_10_I, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))

summary(MD_N)

# ---------------------------------------------------------------------------- #

fit0N <- polr(factor(N_03) ~ 1, data = MD_N, Hess = TRUE, method = "probit")
summary(fit0N) #AIC: 140.9757

fitCN <- polr(factor(N_03) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_N, Hess = TRUE, method = "probit")
summary(fitCN) #AIC: 148.5553 
 
# ---------------------------------------------------------------------------- #
## selection ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~N_09+N_10_A+N_10_B+N_10_C+N_10_D+N_10_E+N_10_F+N_10_G+N_10_H+N_10_I+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)
stepAIC(fitCN, scope=scope, direction = "forward")


### 1   AIC:132.7801 ----
fit1N_mass <-polr(formula = factor(N_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                   D_07  + EDAD, data = MD_N, Hess = TRUE, 
                 method = "probit")
summary(fit1N_mass)

fit1N_ord <- clm(formula = factor(N_03) ~ FG_01 + G_02 + D_11 + G_11 + D_09 + 
                     N_10_C + D_07 + EDAD, data = MD_N, link = "probit")
summary(fit1N_ord)

#### pruebas ----
vif(fit1N_mass)

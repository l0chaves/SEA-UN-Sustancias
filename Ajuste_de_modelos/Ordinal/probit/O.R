load("Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_o", "control", "d", "d2", "encuestas")))

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

MD_O <- C_o %>% 
  filter(O_03 != "na") %>% #Y. borrando los registros de na
  mutate(O_03 = factor(O_03, levels= c(1,2,3,4,5))) %>%
  dplyr::select(O_03, O_04_FUMADA, O_09_VALOR, O_10_A, O_10_B, O_10_C, O_10_D, O_10_E, O_10_F, O_10_G, O_10_H, O_10_I, DIRECTORIO) %>% 
  left_join(control, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(X, by=c("DIRECTORIO"="DIRECTORIO"))

summary(MD_O)

# ---------------------------------------------------------------------------- #


fit0O <- polr(factor(O_03) ~ 1, data = MD_O, Hess = TRUE, method = "probit")
summary(fit0O) #AIC: 13.50271

fitCO <- polr(factor(O_03) ~ FG_01+G_02+D_11+G_11+D_09, data = MD_O, Hess = TRUE, method = "probit")
summary(fitCO) #AIC: 12.0011 

# ---------------------------------------------------------------------------- #
##  ??? no ----
scope <- list(lower=~FG_01+G_02+D_11+G_11+D_09,
              upper=~O_03+ O_04_FUMADA+ O_09_VALOR+O_10_A+O_10_B+O_10_C+O_10_D+O_10_E+O_10_F+O_10_G+O_10_H+O_10_I+
                D_01+D_02+D_07+D_08+D_10+D2_01+D2_03+D2_05+TOTAL_PERSONAS+D_05+EDAD+SEXO+
                FG_01+G_02+D_11+G_11+D_09)

stepAIC(fitCO, scope=scope, direction = "forward")

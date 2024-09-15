load("Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_l", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")
library("ordinal")
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


### 1 AIC: 738.6616 ----
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

#### pruebas ----
vif(fit3L_mass)

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

########## Modelo de regresión logistica para consumo en los ultimos 12 meses ##########

# Primero, correr el script "Datos.R"

##### Librerias #####
library(dplyr)
library(stats)
library(sqldf)
library(readr)
library(glmtoolbox)
library(survey)
library(sandwich)
library(lmtest)


##### Revisando que varibles tienen NA 
missings <- function(x) return(sum(is.na(x)))
apply(DF_ilicitas,2,missings)

##### DF con la def de consumo 12 meses de las drogas ilicitas #####
personas_seleccionadas <- read_csv("Datos_originales/personas_seleccionadas.csv")
personas_seleccionadas$DIRECTORIO = as.character(personas_seleccionadas$DIRECTORIO)

ilicitas12m = DF_ilicitas %>%
  left_join(personas_seleccionadas[, c("DIRECTORIO", "FEX_C")], by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_h[, c("DIRECTORIO", "H_03")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_i[, c("DIRECTORIO", "I_03")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_j_1[, c("DIRECTORIO", "J_02")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_k[, c("DIRECTORIO", "K_03")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_l[, c("DIRECTORIO", "L_02")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_m[, c("DIRECTORIO", "M_02")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_n[, c("DIRECTORIO", "N_02")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_o[, c("DIRECTORIO", "O_02")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_p1[, c("DIRECTORIO", "P_01")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_p2[, c("DIRECTORIO", "P_03")], by=c("DIRECTORIO"="DIRECTORIO")) %>% 
  left_join(C_p3[, c("DIRECTORIO", "P_05")], by=c("DIRECTORIO"="DIRECTORIO"))

colnames(ilicitas12m)

apply(ilicitas12m[,c("H_03","I_03","J_02","K_03","L_02","M_02",
                     "N_02","O_02","P_01","P_03","P_05")],2,missings)

# los NA son personas que no consumen la sustancia por eso se agrupan con los que
# contestaron que no consumieron los ultimos 12 meses.
ilicitas12m = ilicitas12m %>% 
  mutate(H_03 = case_when(is.na(H_03) ~ "2", TRUE ~ H_03)) %>%
  mutate(I_03 = case_when(is.na(I_03) ~ "2", TRUE ~ I_03)) %>%
  mutate(J_02 = case_when(is.na(J_02) ~ "2", TRUE ~ J_02)) %>%
  mutate(K_03 = case_when(is.na(K_03) ~ "2", TRUE ~ K_03)) %>%
  mutate(L_02 = case_when(is.na(L_02) ~ "2", TRUE ~ L_02)) %>%
  mutate(M_02 = case_when(is.na(M_02) ~ "2", TRUE ~ M_02)) %>%
  mutate(N_02 = case_when(is.na(N_02) ~ "2", TRUE ~ N_02)) %>%
  mutate(O_02 = case_when(is.na(O_02) ~ "2", TRUE ~ O_02)) %>%
  mutate(P_01 = case_when(is.na(P_01) ~ "2", TRUE ~ P_01)) %>%
  mutate(P_03 = case_when(is.na(P_03) ~ "2", TRUE ~ P_03)) %>%
  mutate(P_05 = case_when(is.na(P_05) ~ "2", TRUE ~ P_05))

apply(ilicitas12m[,c("H_03","I_03","J_02","K_03","L_02","M_02",
                     "N_02","O_02","P_01","P_03","P_05")],2,table)

# Quitar variables repetidas
ilicitas12m = ilicitas12m %>% 
  select(-PARENTESCO.y,-SEXO.y,-EDAD.y)
colnames(ilicitas12m)

##### Analisis de covariables #####

colnames(ilicitas12m)[2] = "SEXO"
colnames(ilicitas12m)[4] = "PARENTESCO"

# Categorizando la variable EDAD
colnames(ilicitas12m)[3] = "EDAD"
ilicitas12m = sqldf("select *,
             case when EDAD <= 17 then 'Teenagers'
                  when EDAD BETWEEN 18 AND 24 then 'Young'
                  when EDAD BETWEEN 25 AND 34 then 'Young Adult'
                  when EDAD BETWEEN 35 AND 44 then 'Adult'
                  when EDAD BETWEEN 45 AND 63 then 'Elderly'
                  else 'Third Age'
             end as CEDAD
             from ilicitas12m")

# *Categorizando la variable ESTRATO
ilicitas12m = sqldf("select *
             from ilicitas12m
             where ESTRATO != '9'")

ilicitas12m = sqldf("select *,
             case when ESTRATO IN ('1', '8') then 'Bajo-bajo'
                  when ESTRATO = '2' then 'Bajo'
                  when ESTRATO = '3' then 'Medio-bajo'
                  when ESTRATO = '4' then 'Medio'
                  when ESTRATO = '5' then 'Medio-alto'
                  when ESTRATO = '6' then 'Alto'
             end as CESTRATO
             from ilicitas12m")

### covariables ya revisadas: D_01

# Recategorizaciíón de las variables de consumo
# L_02: cocaina

# quitar NA's de las y
ilicitas12m <- sqldf("
SELECT * 
FROM ilicitas12m
WHERE K_03 != '9' AND L_02 != '9'
")
# K_03: marihuana
ilicitas12m = sqldf("select *,
             case when K_03 == '1' then 1
                  when K_03 == '2' then 0
             end as Mari
             from ilicitas12m")
# L_02: cocaina
ilicitas12m = sqldf("select *,
             case when L_02 == '1' then 1
                  when L_02 == '2' then 0
             end as Coca
             from ilicitas12m")

##### categorias de referencia #####
prop.table(table(ilicitas12m$D2_01))

ilicitas12m$D2_03 = relevel(as.factor(ilicitas12m$D2_03), ref = "6")
ilicitas12m$D2_05 = relevel(as.factor(ilicitas12m$D2_05), ref = "5")
ilicitas12m$D2_01 = relevel(as.factor(ilicitas12m$D2_01), ref = "9")


##### Modelación #####

##### Marihuana
# Modelo base
#fit0 = glm(Mari ~ CEDAD + SEXO , family = "binomial", weights = FEX_C, data = ilicitas12m)

fit0 = glm(Mari ~ CEDAD + SEXO , family = "binomial", data = ilicitas12m)
fit0.c = glm(Coca ~ CEDAD + SEXO , family = "binomial", data = ilicitas12m)

AIC0 = AIC(fit0)
AIC0.c = AIC(fit0.c)

# Ignorar D_03, D_04, D_05, D_07,D_12A, G_01_A, G_02_A, G_06
# pendiente D_11
# probar al final: D2_06, D2_07, G_04
fit.s = glm(Mari ~ SEXO+CEDAD+PARENTESCO+D_01+D_02+D_06+D_08+D_09+D_10+D_12_A+D_12_B+D_12_C
               + D2_01+D2_03+D2_04+D2_05
               + TIPO+SERVICIO+CESTRATO+TOTAL_PERSONAS
               + G_01+G_02+G_03+G_05+G_07+G_09 + D2_06+D2_07+G_04,
          family = "binomial",data = ilicitas12m, control = list(maxit = 1000))

fit.c.s = glm(Coca ~ SEXO+CEDAD+PARENTESCO+D_01+D_02+D_06+D_08+D_09+D_10+D_12_A+D_12_B+D_12_C
              + D2_01+D2_03+D2_04+D2_05
              + TIPO+SERVICIO+CESTRATO+TOTAL_PERSONAS
              + G_01+G_02+G_03+G_05+G_07+G_09 + D2_06+D2_07+G_04,
              family = "binomial",data = ilicitas12m, control = list(maxit = 1000))

# Seleccion de variables
model.f <- stepCriterion(fit.s, criterion = "aic", direction = "forward", force.in = fit0)
model.b <- stepCriterion(fit.s, criterion = "aic",direction = "backward", force.out = fit0)

model.c.f <- stepCriterion(fit.c.s, criterion = "aic", direction = "forward", force.in = fit0.c)
restric = list(keep = c("SEXO","CEDAD"))
model.c.b <- stepCriterion(fit.c.s, criterion = "aic",direction = "backward", restrictions = restric)

# estimación de modelos
fit.f = glm(Mari ~ CEDAD + SEXO + G_04 + G_03 + G_02 + G_05 + G_09 + G_01 + D2_03
                 + G_07 + D2_06 + D_02 + D_09 + TIPO + D_06 + D2_05 + CESTRATO,
         family = "binomial",data = ilicitas12m, control = list(maxit = 1000))
summary(fit.f)

fit.b = glm(Mari ~ SEXO + CEDAD + D_02 + D_09 + D2_03 + D2_05 + CESTRATO + TOTAL_PERSONAS
                 + G_01 + G_02 + G_03 + G_05 + G_07 + G_09 + D2_06 + D2_07 + G_04,
            family = "binomial",data = ilicitas12m, control = list(maxit = 1000))
summary(fit.b)

fit.c.f = glm(Coca ~ CEDAD + SEXO + G_04 + G_03 + G_02 + D_09 + G_01 + G_07 + D_06
                   + D_12_C + D2_03 + D2_06 + G_09 + D2_05 + D2_01 + G_05,
              family = "binomial",data = ilicitas12m, control = list(maxit = 1000))
summary(fit.c.f)

fit.c.b = glm(Coca ~ CEDAD + SEXO + D_06 + D_09 + D_12_C + D2_03 + D2_05 + G_01 + G_02
                   + G_03 + G_05 + G_07 + G_09 + D2_06 + D2_07 + G_04,
            family = "binomial",data = ilicitas12m, control = list(maxit = 1000))
stepCriterion(fit.c.b, criterion = "aic", direction = "forward", force.in = fit0.c)
summary(fit.c.b)

# Selección de modelo por AIC
AIC(fit0,fit.f,fit.b)       # fit.f
AIC(fit0.c,fit.c.f,fit.c.b) #  fit.c.f

fit.m = fit.f
fit.c = fit.c.f

summary(fit.m)
summary(fit.c)

##### Validación Marihuana #####
#### Análisis de Influencia
# Distancia de Cook
dcm <- cooks.distance(fit.m)
plot(dcm, type="h", xlab="Observation Index", ylab="Cook’s Distance")

#### Residuales
residuals2(fit.m, type = "pearson", plot.it = TRUE)
#summary(fit.m$residuals)
#View(cbind(ilicitas12m$Mari, fit.m$fitted.values, fit.m$residuals))
# WTF
envelope(fit.m, type = "pearson", plot.it = TRUE)

residuals2(fit.m, type = "deviance", plot.it = TRUE)
# atipicos
envelope(fit.m, type = "deviance", plot.it = TRUE)

residuals2(fit.m, type = "quantile", plot.it = TRUE)
# atipicos
envelope(fit.m, type = "quantile", plot.it = TRUE)
# envelopes bien

#### Multicolinealidad
# Factores de Inflación de Varianza
gvif(fit.m)

#### Habilidad Predictiva
s = sample(1:nrow(ilicitas12m), size = nrow(ilicitas12m)*0.7)
train = ilicitas12m[s,]
test = ilicitas12m[-s,]

fit.mt = glm(Mari ~ CEDAD + SEXO + G_04 + G_03 + G_02 + G_05 + G_09 + G_01 + D2_03
            + G_07 + D2_06 + D_02 + D_09 + TIPO + D_06 + D2_05 + CESTRATO,
            family = "binomial",data = train)

pr.m = predict(fit.mt, newdata = test, type = "response")

cROC.m = ROCc(cbind(test[,"Mari"],pr.m), col="red", col.lab="blue");cROC.m


##### Validación Cocaina #####
#### Análisis de Influencia
# Distancia de Cook
dcm.c <- cooks.distance(fit.c)
plot(dcm.c, type="h", xlab="Observation Index", ylab="Cook’s Distance")

#### Residuales
residuals2(fit.c, type = "pearson", plot.it = TRUE)
#summary(fit.c$residuals)
# WTF
envelope(fit.c, type = "pearson", plot.it = TRUE)

residuals2(fit.c, type = "deviance", plot.it = TRUE)
# atipicos
envelope(fit.c, type = "deviance", plot.it = TRUE)

residuals2(fit.c, type = "quantile", plot.it = TRUE)
# atipicos
envelope(fit.c, type = "quantile", plot.it = TRUE)
# envelopes bien

#### Multicolinealidad
# Factores de Inflación de Varianza
gvif(fit.c)

#### Habilidad Predictiva
fit.ct = glm(Coca ~ CEDAD + SEXO + G_04 + G_03 + G_02 + D_09 + G_01 + G_07 + D_06
             + D_12_C + D2_03 + D2_06 + G_09 + D2_05 + D2_01 + G_05,
             family = "binomial",data = train)

pr.c = predict(fit.ct, newdata = test, type = "response")

cROC.c = ROCc(cbind(test[,"Coca"],pr.c), col="red", col.lab="blue");cROC.c

##### Estimacion con Pesos #####
fit.mp = glm(Mari ~ CEDAD + SEXO + G_04 + G_03 + G_02 + G_05 + G_09 + G_01 + D2_03
            + G_07 + D2_06 + D_02 + D_09 + TIPO + D_06 + D2_05 + CESTRATO,
            family = "binomial", weights = FEX_C, data = ilicitas12m,
            control = list(maxit = 1000), mustart = fit.m$fitted.values)
summary(fit.m)
summary(fit.mp)
fit.m$coefficients/fit.mp$coefficients


fit.cp = glm(Coca ~ CEDAD + SEXO + G_04 + G_03 + G_02 + D_09 + G_01 + G_07 + D_06
              + D_12_C + D2_03 + D2_06 + G_09 + D2_05 + D2_01 + G_05,
              family = "binomial", weights = FEX_C, data = ilicitas12m,
              control = list(maxit = 1000), mustart = fit.c$fitted.values)
summary(fit.c)
summary(fit.cp)
fit.c$coefficients/fit.cp$coefficients

# p-valores
vcov_robusta.m = sandwich::vcovHC(fit.mp, type = "HC0")
coeftest(fit.mp, vcov. = vcov_robusta.m)

vcov_robusta.c = sandwich::vcovHC(fit.cp, type = "HC1")
coeftest(fit.cp, vcov. = vcov_robusta.c)

# OR
summary(fit.mp)
exp(coefficients(fit.mp))

summary(fit.cp)
exp(coefficients(fit.cp))

#
table(sqldf("select D2_05
      from ilicitas12m
      where D_02 = 3"))

nrow(ilicitas12m)

sum(ilicitas12m$Mari)
mean(ilicitas12m$Mari)*100

sum(ilicitas12m$Coca)
mean(ilicitas12m$Coca)*100


#### ORIENTACION SEXUAL
t0 = table(ilicitas12m$D2_06);t0
round(prop.table(t0)*100,2)

t = table(ilicitas12m$D2_06,ilicitas12m$Mari);cbind(t,round((t[,2]/t[,1]*100),2))
t = table(ilicitas12m$D2_06,ilicitas12m$Coca);cbind(t,round((t[,2]/t[,1]*100),2))

#### ESTADO CIVIL
t0 = table(ilicitas12m$D2_03);t0
round(prop.table(t0)*100,2)

t = table(ilicitas12m$D2_03,ilicitas12m$Mari);cbind(t,round((t[,2]/t[,1]*100),2))
t = table(ilicitas12m$D2_03,ilicitas12m$Coca);cbind(t,round((t[,2]/t[,1]*100),2))

table(ilicitas12m$CEDAD,ilicitas12m$D2_03)

#### OCU TIEMPO
t0 = table(ilicitas12m$D_02);t0
round(prop.table(t0)*100,2)

t = table(ilicitas12m$D_02,ilicitas12m$Mari);cbind(t,round((t[,2]/t[,1]*100),2))
t = table(ilicitas12m$D_02,ilicitas12m$Coca);cbind(t,round((t[,2]/t[,1]*100),2))

table(ilicitas12m$CEDAD,ilicitas12m$D2_03)

#### educacion
t0 = table(ilicitas12m$D2_05);t0
round(prop.table(t0)*100,2)

t = table(ilicitas12m$D2_05,ilicitas12m$Mari);cbind(t,round((t[,2]/t[,1]*100),2))
t = table(ilicitas12m$D2_05,ilicitas12m$Coca);cbind(t,round((t[,2]/t[,1]*100),2))

table(ilicitas12m$CEDAD,ilicitas12m$D2_03)

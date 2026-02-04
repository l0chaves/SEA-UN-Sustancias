#MODELOS DE PREVALENCIA CON DATOS A NIVEL POBLACIONAL

rm(list=ls())
library(dplyr)
library(readxl)
library(MASS)
library(VGAM)

# Cargando bases de datos de la ENCSPA ------------------------------------
Regresores<-readRDS ("~/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios/Regresores.rds")
Regresores<-within(Regresores,{MPIO<-as.numeric(MPIO)})
#Adding the counts of raizal,Palenquero to Afrodescendent category
Regresores <- within(Regresores, {
  `Negro(a), mulato(a), afrodescendiente` <- `Negro(a), mulato(a), afrodescendiente` + 
    `Palenquero de San Basilio` + 
    `Raizal del Archipiélago`
  rm(`Palenquero de San Basilio`, `Raizal del Archipiélago`)
})
#convirtiendo todo a porcentajes
Regresores<-Regresores %>%
  mutate(across(.cols = -c(MPIO, Poblacion,DPMP,Densidad,Superficie,Viviendas2019,Densidad_Vivienda), .fns = ~ round( 1* . / Poblacion, 3)))
str(Regresores)
#codigo de municipios:
  library(dplyr)

setwd("C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Datos_originales")
library(dplyr)

encuestas <- read.csv("encuestas.csv")[,c("Depmuni", "DIRECTORIO")] %>% 
  mutate(MPIO = Depmuni);encuestas <- encuestas[, !colnames(encuestas) %in% c("Depmuni")]

#Personas seleccionadas:

library(sqldf)

pselec <- read.csv("personas_seleccionadas.csv")
pselec <- sqldf("SELECT FEX_C AS SW, DIRECTORIO FROM pselec")


#e,f,k,l
#tabaco
ecapitulos<-read.csv("e_capitulos.csv")[,c('E_04','DIRECTORIO')]%>%
  mutate(E_04 = ifelse(E_04 %in% c(2, 9), 0, E_04)) %>% # Cambia el valor de L_02 a 0 si es 2 o 9
  full_join(pselec, by = "DIRECTORIO")%>%full_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%
  summarise(E_04=round(sum(E_04*SW, na.rm = TRUE),0))
#%>%
#replace_na(list(K_03 = 0))

#alcohol
fcapitulos<-read.csv("f_capitulos.csv")[,c('F_06','DIRECTORIO')]%>%
  mutate(F_06 = ifelse(F_06 %in% c(2, 9), 0, F_06)) %>% # Cambia el valor de L_02 a 0 si es 2 o 9
  full_join(pselec, by = "DIRECTORIO")%>%full_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%
  summarise(F_06=round(sum(F_06*SW, na.rm = TRUE)),0)
#%>%
# replace_na(list(K_03 = 0))


#marihuana
kcapitulos <- read.csv("k_capitulos.csv")[,c('K_03', 'DIRECTORIO')] %>%
  mutate(K_03 = ifelse(K_03 %in% c(2, 9), 0, K_03)) %>%
  full_join(pselec, by = "DIRECTORIO") %>%
  full_join(encuestas, by = "DIRECTORIO") %>%
  group_by(MPIO) %>%
  summarise(K_03 = round(sum(K_03 * SW, na.rm = TRUE), 0)) 
#%>%  replace_na(list(K_03 = 0))



#cocaina
lcapitulos<-read.csv("l_capitulos.csv")[,c('L_02','DIRECTORIO')]%>%
  mutate(L_02 = ifelse(L_02 %in% c(2, 9), 0, L_02)) %>% # Cambia el valor de L_02 a 0 si es 2 o 9
  full_join(pselec, by = "DIRECTORIO")%>%
  full_join(encuestas, by = "DIRECTORIO")%>%
  group_by(MPIO)%>%summarise(L_02=round(sum(L_02*SW, na.rm = TRUE),0))
#%>%replace_na(list(L_02 = 0))






# Modelos de prevalencia por municipio ------------------------------------
library(glmtoolbox)
#Modelos para tabaco ----
tabaco<-ecapitulos%>%inner_join(Regresores, by='MPIO')
#View(tabaco)
Formula1<-E_04~.-MPIO-DPMP -Poblacion-Superficie-Viviendas2019+Densidad+Densidad_Vivienda-Adult-`Ningún grupo étnico-racial`
mod_taba_1_1<-glm(Formula1,offset=log(Poblacion),family=poisson(log),data=tabaco)
#Incluyendo variables de control forsozamente:
mod_taba_1 <- step(mod_taba_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_taba_1)
zero.excess(mod_taba_1)

#Envelope
graf<-function(fit){
  set.seed(12192129)
  envelope(fit, rep=30, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
           col.axis="blue", col.main="black", family="mono", cex=0.8, plot.it=TRUE,ylim=c(-10,10))  #Simbolos claros de sobredispersion
}

graf(mod_taba_1)
#como no hay ceros, se usa modelos truncados con sobredispersion





mod_taba_2<-overglm(E_04 ~  Pob_Hombres + Teenagers + Young + Young_Adult + 
                      Adult + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
                      `Negro(a), mulato(a), afrodescendiente` + `Ningún grupo étnico-racial` + 
                      Densidad + Densidad_Vivienda,data=tabaco,offset=log(Poblacion),family = "nb1(log)")




mod_taba_3<-update(mod_taba_2 ,family="nb2(log)")
mod_taba_4<-update(mod_taba_2 ,family="nbf(log)") #convergence NOT ARCHIVED:
mod_taba_5<-update(mod_taba_1,family=quasipoisson())

mod_taba_6<-glm.nb(E_04 ~  Pob_Hombres + Teenagers + Young + Young_Adult + 
                     Adult + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
                     `Negro(a), mulato(a), afrodescendiente` + `Ningún grupo étnico-racial` + 
                     Densidad + Densidad_Vivienda+offset(log(Poblacion)),data=tabaco)
AIC(mod_taba_1,mod_taba_2,mod_taba_3,mod_taba_4,mod_taba_6)
round(cbind(mod_taba_1$coefficients,mod_taba_4$coefficients,mod_taba_5$coefficients,mod_taba_6$coefficients),5)
summary(mod_taba_4)
graf(mod_taba_3)




mod_taba_4_2<-vglm(E_04 ~  Pob_Hombres + Teenagers + Young + Young_Adult  + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
  `Negro(a), mulato(a), afrodescendiente`  + 
  Densidad + Densidad_Vivienda+offset(log(Poblacion)),data=tabaco, family=negbinomial)

summary(mod_taba_4_2)
mod_taba_4_3<-glm.nb(E_04 ~  Pob_Hombres + Teenagers + Young + Young_Adult + 
                      Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
                       `Negro(a), mulato(a), afrodescendiente`  + 
                       Densidad + Densidad_Vivienda+offset(log(Poblacion)),data=tabaco)

round(mod_taba_4_3$coefficients)
# Modelos de alcohol ------------------------------------------------------

alcohol<-fcapitulos%>%inner_join(Regresores, by='MPIO')
Formula2<-F_06~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda-`0`
mod_alc_1_1<-glm(Formula2,offset=log(Poblacion),family=poisson(log),data=alcohol)
summary(mod_alc_1_1)
#Incluyendo variables de control forsozamente:
mod_alc_1 <- step(mod_alc_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_alc_1)
zero.excess(mod_alc_1) #no se rechaza, no hay exceso, hay falta


#Envelope
set.seed(12192129)
graf(mod_alc_1) #sobredispersion y no hay ceros, vamos a usar modelso truncados

mod_alc_2<-overglm(F_06 ~ Pob_Hombres + Teenagers + Young + Young_Adult + Adult + 
                     Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + `Negro(a), mulato(a), afrodescendiente` + 
                     `Ningún grupo étnico-racial` + Superficie + Densidad + 
                     Densidad_Vivienda,offset=log(Poblacion),data=alcohol,family="nb1")



mod_alc_3<-update(mod_alc_2,family="nb2(log)")
mod_alc_4<-update(mod_alc_2,family="nbf(log)")



AIC(mod_alc_1,mod_alc_2,mod_alc_3,mod_alc_4)
summary(mod_alc_3)
#Comparando estimaciones
round(cbind(mod_alc_1$coefficients,mod_alc_3$coefficients ),5)

#selected ZTNBf!


mod_alc_4_2<-vglm(F_06 ~ Pob_Hombres + Teenagers + Young + Young_Adult + Adult + 
                    Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + `Negro(a), mulato(a), afrodescendiente` + 
                    `Ningún grupo étnico-racial` + Superficie + Densidad + 
                    Densidad_Vivienda,offset(log(Poblacion)),data=alcohol,family=negbinomial)



summary(mod_alc_4_2)

# Modelos de marihuana ----------------------------------------------------
marihuana<-kcapitulos%>%inner_join(Regresores, by='MPIO')
marihuana_sin<-kcapitulos_sin%>%inner_join(Regresores, by='MPIO')
Formula3<-K_03~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_mari_1_1<-glm(Formula3,offset=log(Poblacion),family=poisson(log),data=marihuana)
summary(mod_mari_1_1)
mod_mari_1<-step(mod_mari_1_1,direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
zero.excess(mod_mari_1)
summary(mod_mari_1)
set.seed(457)
graf(mod_mari_1)




mod_mari_1<-zeroinf(K_03 ~ Pob_Hombres + Teenagers + Young + Young_Adult + 
                      Adult + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
                      `Negro(a), mulato(a), afrodescendiente` + `Ningún grupo étnico-racial` + 
                      Superficie + Densidad + Densidad_Vivienda,offset(log(Poblacion)),family="poi(log)", zero.link="logit",data=marihuana)




mod_mari_2<-update(mod_mari_1, family="nb1(log)")
mod_mari_3<-update(mod_mari_1,family="nb2")
mod_mari_4<-update(mod_mari_1,family="nbf(log)")
AIC(mod_mari_1,mod_mari_2,mod_mari_4)


mod_mari_4_2<-vglm(K_03 ~ Pob_Hombres + Teenagers + Young + Young_Adult + 
       Adult + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
       `Negro(a), mulato(a), afrodescendiente` + `Ningún grupo étnico-racial` + 
       Superficie + Densidad + Densidad_Vivienda,offset(log(Poblacion)),family=zanegbinomial,data=marihuana)

summary(mod_mari_4)
summary(mod_mari_4_2)
round(cbind(rbind(mod_mari_4$coefficients$counts,mod_mari_4$coefficients$zeros),coef(mod_mari_4_2)),3)








# MOdelos cocaina ---------------------------------------------------------

coca<-lcapitulos%>%inner_join(Regresores, by='MPIO')
Formula4<-L_02~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_coca_1_1<-glm(Formula4,offset=log(Poblacion),family=poisson(log),data=coca)
#Incluyendo variables de control forsozamente:
mod_coca_1 <- step(mod_coca_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
zero.excess(mod_coca_1)

mod_coca_1<-vglm(L_02 ~ Pob_Hombres + Teenagers + Young + Young_Adult + Adult + 
                      Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + `Negro(a), mulato(a), afrodescendiente` + 
                      `Ningún grupo étnico-racial` + Superficie + Densidad + 
                      Densidad_Vivienda,data=coca,offset(Poblacion),family=zanegbinomial)


round(coef(mod_coca_1),3)



summary(mod_coca_1)



cocaina

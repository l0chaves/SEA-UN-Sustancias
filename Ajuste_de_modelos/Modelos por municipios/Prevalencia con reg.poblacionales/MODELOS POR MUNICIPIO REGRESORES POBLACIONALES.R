#MODELOS DE PREVALENCIA CON DATOS A NIVEL POBLACIONAL

rm(list=ls())
library(dplyr)
library(readxl)


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
  mutate(across(.cols = -c(MPIO, Poblacion,DPMP,Densidad,Superficie,Viviendas2019,Densidad_Vivienda), .fns = ~ round(100 * . / Poblacion, 3)))
str(Regresores)
#codigo de municipios:
setwd("C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Datos_originales")
encuestas<-read.csv("encuestas.csv")%>%select(MPIO='Depmuni','DIRECTORIO')   
nrow(encuestas)
#Personas seleccionadas:
pselec<-read.csv("personas_seleccionadas.csv")%>%select(SW='FEX_C','DIRECTORIO')   
#e,f,k,l
#tabaco
ecapitulos<-read.csv("e_capitulos.csv")%>%select('E_04','DIRECTORIO')%>%filter(E_04==1)%>%
  inner_join(pselec, by = "DIRECTORIO")%>%inner_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%summarise(E_04=round(sum(E_04*SW),0))
summary(ecapitulos)
#View(ecapitulos)
  #alcohol
fcapitulos<-read.csv("f_capitulos.csv")%>%select('F_06','DIRECTORIO')%>%filter(F_06==1)%>%
  inner_join(pselec, by = "DIRECTORIO")%>%inner_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%summarise(F_06=round(sum(F_06*SW)),0)
summary(fcapitulos)


#sin factores de expansion
#fcapitulos<-read.csv("f_capitulos.csv")%>%select('F_06','DIRECTORIO')%>%filter(F_06==1)%>%
#  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%summarise(F_06=sum(F_06))
#summary(fcapitulos)

#marihuana
kcapitulos<-read.csv("k_capitulos.csv")%>%select('K_03','DIRECTORIO')%>%filter(K_03==1)%>%
  inner_join(pselec, by = "DIRECTORIO")%>%inner_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%summarise(K_03=round(sum(K_03*SW),0))
summary(kcapitulos)
#cocaina
lcapitulos<-read.csv("l_capitulos.csv")%>%select('L_02','DIRECTORIO')%>%filter(L_02==1)%>%
  inner_join(pselec, by = "DIRECTORIO")%>%inner_join(encuestas, by = "DIRECTORIO")%>%group_by(MPIO)%>%summarise(L_02=round(sum(L_02*SW),0))
summary(lcapitulos)
#No hay ceros por municipio, es decir hay al menos un consumidor de droga en cada municipio.




# Modelos de prevalencia por municipio ------------------------------------
library(glmtoolbox)
#Modelos para tabaco ----
tabaco<-ecapitulos%>%inner_join(Regresores, by='MPIO')
#View(tabaco)
Formula1<-E_04~.-MPIO-DPMP -Poblacion-Superficie-Viviendas2019+Densidad+Densidad_Vivienda
mod_taba_1_1<-glm(Formula1,offset=log(Poblacion),family=poisson(log),data=tabaco)
#Incluyendo variables de control forsozamente:
mod_taba_1 <- step(mod_taba_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_taba_1)

#Envelope
set.seed(12192129)
envelope(mod_taba_1,type="quantile")  #Simbolos claros de sobredispersion
#como no hay ceros, se usa modelos truncados con sobredispersion





mod_taba_2<-overglm(E_04 ~ Pob_Hombres + Teenagers + Young + Young_Adult + 
                      Adult + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
                     # `Raizal del Archipiélago` + `Palenquero de San Basilio` + 
                      `Negro(a), mulato(a), afrodescendiente` + `Ningún grupo étnico-racial` + 
                      Densidad + Densidad_Vivienda,data=tabaco,offset=log(Poblacion),family = "ztnb1(log)")





mod_taba_3<-update(mod_taba_2 ,family="ztnb2(log)")
mod_taba_4<-update(mod_taba_2 ,family="ztnbf(log)")
mod_taba_5<-update(mod_taba_1,family=quasipoisson())

AIC(mod_taba_1,mod_taba_2,mod_taba_3,mod_taba_4)
summary(mod_taba_4)

par(mfrow=c(2,2))
set.seed(12192129)
envelope(mod_taba_1,type="quantile",main="mod 1")
envelope(mod_taba_2,type="quantile",main="mod 2")
envelope(mod_taba_3,type="quantile",main="mod 3")
envelope(mod_taba_4,type="quantile",main="mod 4") #No funciona

adjR2(mod_taba_1,mod_taba_5) #Terrible

stepCriterion(mod_taba_4)


fit<-update(mod_taba_4,~ Indigena + Densidad + Young + Adult + Teenagers + Third_Age + `Gitano(a) o Rrom` + `Ningún grupo étnico-racial` )
stepCriterion(mod_taba_1)
summary(fit)

#Comparando estimaciones
round(cbind(mod_taba_1$coefficients,mod_taba_4$coefficients),5)

#selected ZTNBf!

# Modelos de alcohol ------------------------------------------------------

alcohol<-fcapitulos%>%inner_join(Regresores, by='MPIO')
Formula2<-F_06~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_alc_1_1<-glm(Formula2,offset=log(Poblacion),family=poisson(log),data=alcohol)
summary(mod_alc_1_1)
#Incluyendo variables de control forsozamente:
mod_alc_1 <- step(mod_alc_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_alc_1)
#Envelope
set.seed(12192129)
envelope(mod_alc_1,type="quantile") #sobredispersion y no hay ceros, vamos a usar modelso truncados

mod_alc_2<-overglm( F_06 ~ Pob_Hombres + Teenagers + Young + Young_Adult + Adult + 
                      Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + `Negro(a), mulato(a), afrodescendiente` + 
                      `Ningún grupo étnico-racial` + Superficie + Densidad + 
                      Densidad_Vivienda,offset=log(Poblacion),data=alcohol,family="ztnb1(log)")

set.seed(12192129)
envelope(mod_alc_2,type="quantile")

mod_alc_3<-update(mod_alc_2,family="ztnb2(log)")
mod_alc_4<-update(mod_alc_2,family="ztnbf(log)")


AIC(mod_alc_1,mod_alc_2,mod_alc_3,mod_alc_4)
summary(mod_alc_4)
#Comparando estimaciones
round(cbind(mod_alc_1$coefficients,mod_alc_4$coefficients ),5)

stepCriterion(mod_alc_4)

mod_alc_4_1<-update(mod_alc_4,~ Superficie + `Ningún grupo étnico-racial` + `Negro(a), mulato(a), afrodescendiente` )
mod_alc_4_2<-update(mod_alc_4,~ Superficie + `Ningún grupo étnico-racial` + `Negro(a), mulato(a), afrodescendiente`+ Pob_Mujeres)
anova(mod_alc_4_1,mod_alc_4_2)#No se mete a las mujeres
summary(mod_alc_4_1)

#selected ZTNBf!


# Modelos de marihuana ----------------------------------------------------
marihuana<-kcapitulos%>%inner_join(Regresores, by='MPIO')
Formula3<-K_03~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_mari_1_1<-glm(Formula3,offset=log(Poblacion),family=poisson(log),data=marihuana)
summary(mod_mari_1_1)
mod_mari_1<-step(mod_mari_1_1,direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_mari_1)
set.seed(457)
envelope(mod_mari_1)#Subdispersion

mod_mari_2<-overglm(formula = K_03 ~ Pob_Hombres + Teenagers + Young + Young_Adult + 
                      Adult + Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + 
                      `Negro(a), mulato(a), afrodescendiente` + `Ningún grupo étnico-racial` + 
                      Superficie + Densidad + Densidad_Vivienda,  family = "ztnb2", data = marihuana, offset = log(Poblacion))
envelope(mod_mari_2) #mejora
mod_mari_3<-update(mod_mari_2,family="ztnbf(log)")
AIC(mod_mari_1,mod_mari_2,mod_mari_3)#mod2  1840.294


summary(mod_mari_2) #mayoria de matametros no significativos, usaremos el mismo modelo, pero con stepaic
#Comparando estimaciones
round(cbind(mod_mari_1$coefficients,mod_mari_2$coefficients),5)

  
#mod_mari_5<-update(mod_mari_2,fomula=Formula3)
  #summary(mod_mari_5)
  #mod_mari_6<-stepCriterion(mod_mari_5,direction = "forward", scope = list(lower = . ~ Densidad + Densidad_Vivienda))

#selected ZTNB2!


# MOdelos cocaina ---------------------------------------------------------
coca<-lcapitulos%>%inner_join(Regresores, by='MPIO')
Formula4<-L_02~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_coca_1_1<-glm(Formula4,offset=log(Poblacion),family=poisson(log),data=coca)
#Incluyendo variables de control forsozamente:
mod_coca_1 <- step(mod_coca_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_coca_1)
set.seed(123)
envelope(mod_coca_1,residuals="standarized")#sobredispersion 
#Modelos para sobrdiseprsion
mod_coca_2<-overglm(formula= L_02 ~  Pob_Hombres + Teenagers + Young + Young_Adult + Adult + 
                      Elderly + Third_Age + Indigena + `Gitano(a) o Rrom` + `Negro(a), mulato(a), afrodescendiente` + 
                      `Ningún grupo étnico-racial` + Superficie + Densidad + 
                      Densidad_Vivienda, family = "ztnb1(log)", data = coca, offset = log(Poblacion))
mod_coca_3<-update(mod_coca_2,family="ztnb2")
mod_coca_4<-update(mod_coca_2,family="ztnbf")

AIC(mod_coca_1,mod_coca_2,mod_coca_3,mod_coca_4) #Me quedo con el segundo

summary(mod_coca_3)
envelope(mod_coca_3,residuals="quantile")
#Comparando estimaciones
round(cbind(mod_coca_1$coefficients,mod_coca_4$coefficients),5)

#selected ZTNB1!
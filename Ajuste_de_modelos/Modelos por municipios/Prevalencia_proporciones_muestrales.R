rm(list=ls())
# Librerias ---------------------------------------------------------------

library(readxl)
library(dplyr)
library(glmtoolbox)


# Carga de datos\ ---------------------------------------------------------

## Cargando base de datos tabla total --------------------------------------
setwd('C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios')
tabla_total <- readRDS("tabla_total.rds") 
tabla_total[is.na(tabla_total)] <- 0
row.names(tabla_total)<-tabla_total$Depmuni
tabla_total$Depmuni<-as.numeric(tabla_total$Depmuni)
respuestas_por_muni <- readRDS("respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni

## Cargando proyecciones y retroporyecciones -------------------------------

Poblacion <- read_excel("~/GitHub/SEMILLERO-SEA-UN/Datos_originales/adicional/DCD-area-proypoblacion-Mun-2005-2019.xlsx",skip = 11)%>%
  filter(`ÁREA GEOGRÁFICA`=="Total",AÑO==2019)%>%select(MPIO,Población)
colnames(Poblacion)<-c("Depmuni","poblacion")
Poblacion$Depmuni<-as.numeric(Poblacion$Depmuni)

tabla_total<-inner_join(tabla_total,Poblacion,by="Depmuni")
  


# Base de datos Municipio con superficie municipal ------------------------
Municipios <- read_excel("~/GitHub/SEMILLERO-SEA-UN/Datos_originales/adicional/Municipios.xlsx")%>%select(Depmun,Superficie)
Municipios <-within(Municipios ,{Depmuni<-Depmun
Depmun<-NULL})

# Base de datos Numero de viviendas en cada municipio ---------------------

Hogares<- read_excel("~/GitHub/SEMILLERO-SEA-UN/Datos_originales/adicional/anexo-proyecciones-hogares-dptal-2018-2050-mpal-2018-2035.xlsx", 
              sheet = "Proyecciones Hogares mpio", 
              range = "C11:G3358")
colnames(Hogares)<-c("Depmuni","Municipio","Area","Viviendas2018","Viviendas2019")
Hogares<-Hogares%>%filter(Area=="Total")%>%select("Depmuni","Viviendas2019")


Municipios <- Municipios %>% right_join(Hogares, by = c("Depmuni"))
  #Creando la variable densidad poblacional
  Municipios<-within(Municipios, {Densidad_Vivienda<-Viviendas2019/Superficie })
  
  
#Creando tabal_total2 con regresores:
tabla_total2 <- tabla_total %>%
  select(-which(colnames(tabla_total) %in% colnames(respuestas_por_muni) & colnames(tabla_total) != "Depmuni"))%>%
  select(-c(4:22))
tabla_total2<-within(tabla_total2, Depmuni<-as.numeric(Depmuni)) 


# Conviritiendo regresores en porcentajes ---------------------------------

str(tabla_total2)
tabla_total2 <- tabla_total2 %>%
  mutate(across(.cols = -c(Depmuni, poblacion), .fns = ~ round(100 * . / 59977, 3)))   # 59977 (# personas que respondieron la encuesta)              poblacion

#Uniendo con Municipios
Municipios<-within(Municipios, Depmuni<-as.numeric(Depmuni)) 
tabla_total2<- tabla_total2%>%inner_join(Municipios, by = c("Depmuni") )
tabla_total2<-within(tabla_total2,{Densidad_Pob<-poblacion/Superficie})

# Cargando bases de datos de la ENCSPA ------------------------------------


#codigo de municipios:
setwd("C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Datos_originales")
encuestas<-read.csv("encuestas.csv")%>%select('Depmuni','DIRECTORIO')   
nrow(encuestas)
#e,f,k,l
#tabaco
ecapitulos<-read.csv("e_capitulos.csv")%>%select('E_04','DIRECTORIO')%>%filter(E_04==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(E_04=sum(E_04))
summary(ecapitulos)
#alcohol
fcapitulos<-read.csv("f_capitulos.csv")%>%select('F_06','DIRECTORIO')%>%filter(F_06==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(F_06=sum(F_06))
summary(fcapitulos)
#marihuana
kcapitulos<-read.csv("k_capitulos.csv")%>%select('K_03','DIRECTORIO')%>%filter(K_03==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(K_03=sum(K_03))
summary(kcapitulos)
#cocaina
lcapitulos<-read.csv("l_capitulos.csv")%>%select('L_02','DIRECTORIO')%>%filter(L_02==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(L_02=sum(L_02))
summary(lcapitulos)
#No hay ceros por municipio, es decir hay al menos un consumidor de droga en cada municipio.


# Modelos de prevalencia por municipio ------------------------------------



#Modelos para tabaco ----
Formula1<-E_04~.-Depmuni -poblacion-Superficie-Viviendas2019+Densidad_Pob+Densidad_Vivienda
mod_taba_1_1<-glm(Formula1,offset=log(poblacion),family=poisson(log),data=ecapitulos%>%inner_join(tabla_total2, by='Depmuni'))
#Incluyendo variables de control forsozamente:
mod_taba_1 <- step(mod_taba_1_1, direction = "both", scope = list(lower = . ~ Densidad_Pob + Densidad_Vivienda))
summary(mod_taba_1)
#Envelope
set.seed(12192129)
envelope(mod_taba_1,type="quantile") 
#como no hay ceros, se usa modelos truncados
mod_taba_2_1<-overglm(E_04 ~ D_01_1 + D_01_2 + D_02_1 + D_02_4 + D_02_6 + 
                      D_02_8 + D_07_1 + D_07_2 + D_07_3 + D_07_na + Densidad_Vivienda + 
                      Densidad_Pob,data=ecapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
#seleccionando variables incluyendo las de control forzadamaente
stepCriterion(mod_taba_2_1 , force.in =  ~ Densidad_Pob + Densidad_Vivienda)
mod_taba_2<-update(mod_taba_2,~ Densidad_Pob + Densidad_Vivienda + D_07_2 + D_07_3 )

mod_taba_3<-update(mod_taba_2_1,family="ztnb1")
#seleccionando variables incluyendo las de control forzadamaente
stepCriterion(mod_taba_3 , force.in =  ~ Densidad_Pob + Densidad_Vivienda)
mod_taba_3<-update(mod_taba_2_1,~ Densidad_Pob + Densidad_Vivienda + D_07_2 ,family="ztnb1")

mod_taba_4<-update(mod_taba_2_1,family="ztnb2")
#seleccionando variables incluyendo las de control forzadamaente
stepCriterion(mod_taba_4 , force.in =  ~ Densidad_Pob + Densidad_Vivienda)
mod_taba_4<-update(mod_taba_2_1,~ Densidad_Pob + Densidad_Vivienda + D_07_2 + D_07_3 ,family="ztnb2")

mod_taba_5<-update(mod_taba_2_1,family="ztnbf")
  #seleccionando variables incluyendo las de control forzadamaente
stepCriterion(mod_taba_5 , force.in =  ~ Densidad_Pob + Densidad_Vivienda)

mod_taba_6<-update(mod_taba_1,family=quasipoisson())

AIC(mod_taba_1,mod_taba_2,mod_taba_3,mod_taba_4,mod_taba_5,mod_taba_6) #modelo escogido 4 BNII 
adjR2(mod_taba_1,mod_taba_6)
summary(mod_taba_2)
summary(mod_taba_3)
summary(mod_taba_4)
#Envelope
set.seed(12192129)
envelope(mod_taba_3,type="quantile")
#Envelope
set.seed(12192129)
envelope(mod_taba_4,type="quantile")
#Envelope
set.seed(12192129)
envelope(mod_taba_5,type="quantile")


plot(cooks.distance(mod_taba_3),type="h",main="Distancia de cook para Tabaco")




# Modelos para alcohol ----------------------------------------------------
alcohol<-fcapitulos%>%inner_join(tabla_total2, by='Depmuni')
plot(with(alcohol, 100*F_06/Poblacion))

Formula2<-F_06~.-Depmuni -poblacion-Superficie-Viviendas2019
mod_alc_1_1<-glm(Formula2,offset=log(poblacion),family=poisson(log),data=alcohol)
#Incluyendo variables de control forsozamente:
mod_alc_1 <- step(mod_alc_1_1, direction = "both", scope = list(lower = . ~ Densidad_Pob + Densidad_Vivienda))
summary(mod_alc_1)
#Envelope
set.seed(12192129)
envelope(mod_alc_1,type="quantile") 
#como no hay ceros, se usa modelos truncados
mod_alc_2<-overglm( F_06 ~ SEXO.x_1 + SEXO.x_2 + D_02_1 + D_02_2 + 
                         D_02_3 + D_02_4 + D_02_5 + D_02_6 + D_02_7 + D_02_8 + D_07_1 + 
                         D_07_3 + D_07_9 + D_07_na + D2_01_1 + D2_01_2 + Densidad_Vivienda + 
                         Densidad_Pob+offset(log(poblacion)),data=alcohol,family = "ztpoi")

summary(mod_alc_2)

mod_alc_3<-update(mod_alc_2,family="ztnb1")
mod_alc_4<-update(mod_alc_2,family="ztnb2")
mod_alc_5<-update(mod_alc_2,family="ztnbf")
mod_alc_6<-update(mod_alc_1,family=quasipoisson())

AIC(mod_alc_1,mod_alc_2,mod_alc_3,mod_alc_4,mod_alc_5,mod_alc_6) #modelo escogido 4 BNII 
adjR2(mod_alc_1,mod_alc_6)





# Modelos para Marihuana --------------------------------------------------
marihuana<-kcapitulos%>%inner_join(tabla_total2, by='Depmuni')
Formula3<-K_03~.-Depmuni -poblacion-Superficie-Viviendas2019+offset(log(poblacion))
mod_mari_1_1<-glm(Formula3,family=poisson(log),data=marihuana)
#Incluyendo variables de control forsozamente:
mod_mari_1 <- step(mod_mari_1_1, direction = "both", scope = list(lower = . ~ Densidad_Pob + Densidad_Vivienda))

Formula3<-K_03 ~ SEXO.x_1 + SEXO.x_2 + D_02_1 + D_02_2 + 
  D_02_6 + D_02_8 + Densidad_Vivienda + Densidad_Pob + offset(log(poblacion))


set.seed(12192129)
envelope(mod_mari_1,type="quantile") 

#como no hay ceros, se usa modelo truncado
mod_mari_2<-overglm(Formula3,data=marihuana,family = "ztpoi")
mod_mari_3<-update(mod_mari_2,family="ztnb1")
mod_mari_5<-update(mod_mari_2,family="ztnbf")
mod_mari_6<-update(mod_mari_1,family=quasipoisson())
AIC(mod_mari_1,mod_mari_2,mod_mari_3,mod_mari_5,mod_mari_6) #Modelo escogido segun aic ztnbf
adjR2(mod_mari_1,mod_mari_6)

summary(mod_mari_1)
summary(mod_mari_2)
set.seed(12192129)
envelope(mod_mari_3,type="quantile")



# Modelos para caocaina ---------------------------------------------------


cocaina<-lcapitulos%>%inner_join(tabla_total2, by='Depmuni')
Formula4_1<-L_02~.-Depmuni -poblacion-Superficie-Viviendas2019+offset(log(poblacion))
mod_coca_1_1<-glm(Formula4_1,family=poisson(log),data=cocaina)
mod_coca_1 <- step(mod_coca_1_1, direction = "both", scope = list(lower = . ~ Densidad_Pob + Densidad_Vivienda))
summary(mod_coca_1)
set.seed(12192129)
envelope(mod_coca_1,type="quantile") #Hay subdispersion muy densa

Formula4<-L_02 ~ D_01_1 + D_02_1 + Densidad_Vivienda + Densidad_Pob+offset(log(poblacion))
#como no hay ceros, se usa modelo truncado
mod_coca_2<-overglm(Formula4,data=lcapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
#mod_coca_2<-overglm(Formula4_1,data=lcapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")




mod_coca_3<-update(mod_coca_2,family="ztnb1") #no se tiene en cuenta
mod_coca_4<-update(mod_coca_2,family="ztnb2") #sistema es computacionalmente singular
mod_coca_5<-update(mod_coca_2,family="ztnbf") #modelo apropiado para subdispersion
mod_coca_6<-update(mod_coca_1,family=quasipoisson())
AIC(mod_coca_1,mod_coca_2,mod_coca_3,mod_coca_5,mod_coca_6) 
adjR2(mod_coca_1,mod_coca_6) #escogeremos el de quasiverosimilitud


summary(mod_coca_1)



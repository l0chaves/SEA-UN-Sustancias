
# Librerias ---------------------------------------------------------------

library(readxl)
library(dplyr)
library(glmtoolbox)


# Carga de datos\ ---------------------------------------------------------

## Cargando abse de datos tabla total --------------------------------------
setwd('C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios')
tabla_total <- readRDS("tabla_total.rds") 
tabla_total[is.na(tabla_total)] <- 0
row.names(tabla_total)<-tabla_total$Depmuni
respuestas_por_muni <- readRDS("respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni

## Cargando proyecciones y retroporyecciones -------------------------------

Poblacion <- read_excel("~/GitHub/SEMILLERO-SEA-UN/Datos_originales/adicional/DCD-area-proypoblacion-Mun-2005-2019.xlsx",skip = 11)%>%
  filter(`ÁREA GEOGRÁFICA`=="Total")%>%
  select(MPIO,Población)
colnames(Poblacion)<-c("MPIO","poblacion")

tabla_total<-tabla_total %>% inner_join(Poblacion, by=c("Depmuni"="MPIO"))

#Creando tabal_total2 con regresores:
tabla_total2 <- tabla_total %>%
  select(-which(colnames(tabla_total) %in% colnames(respuestas_por_muni) & colnames(tabla_total) != "Depmuni"))%>%
  select(-c(4:22))
tabla_total2<-within(tabla_total2, Depmuni<-as.numeric(Depmuni)) 
########################################################################

#codigo de municipios:
setwd("C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Datos_originales")
encuestas<-read.csv("encuestas.csv")%>%select('Depmuni','DIRECTORIO')
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
mod_taba_1<-glm(E_04~.-Depmuni -poblacion,offset=log(poblacion),family=poisson(log),data=ecapitulos%>%inner_join(tabla_total2, by='Depmuni'))
stepCriterion(mod_taba_1)
#Incluyendo variables de control forsozamente:
Formula1= E_04~ SEXO.x_1+SEXO.x_2+ D2_01_2 + D2_01_4 + D2_05_9 + D2_01_1 + D2_05_8 + D_07_9 + D_02_7 + D2_05_7 + D_07_3 + D2_05_3 + D_01_2 + D_02_6 + D2_05_4 + D2_01_9 + D_02_5 + D_02_8 + D2_05_6 + D2_01_5 + D2_05_1 + D_02_3 + D_02_4 + D_07_2 + D_02_2  + D2_05_2  
mod_taba_1<-update(mod_taba_1,formula=Formula1)
summary(mod_taba_1)
set.seed(12192129)
#Envelope
envelope(mod_taba_1,type="quantile") #Hay sobredispersion
#como no hay ceros, se usa modelos truncados
mod_taba_2<-overglm(Formula1,offset=log(poblacion),data=ecapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
mod_taba_3<-update(mod_taba_2,family="ztnb1")
mod_taba_4<-update(mod_taba_2,family="ztnb2")
mod_taba_5<-update(mod_taba_2,family="ztnbf")
mod_taba_6<-update(mod_taba_1,family=quasipoisson())
AIC(mod_taba_1,mod_taba_2,mod_taba_3,mod_taba_4,mod_taba_5,mod_taba_6) #modelo escogido 4 BNII 
summary(mod_taba_4)
envelope(mod_taba_4,type="quantile")
plot(cooks.distance(mod_taba_4),type="h",main="Distancia de cook para Tabaco")




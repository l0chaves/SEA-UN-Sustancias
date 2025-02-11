# Librerias ---------------------------------------------------------------

library(readxl)
library(dplyr)
library(glmtoolbox)


# Carga de datos\ ---------------------------------------------------------
setwd('C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios')
tabla_total <- readRDS("tabla_total.rds") 
tabla_total[is.na(tabla_total)] <- 0
row.names(tabla_total)<-tabla_total$Depmuni
respuestas_por_muni <- readRDS("respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni
View(tabla_total)


## Creando tabla_total2 ----------------------------------------------------


tabla_total2 <- tabla_total %>%
  select(-which(colnames(tabla_total) %in% colnames(respuestas_por_muni) & colnames(tabla_total) != "Depmuni"))%>%
  select(-c(4:22))
tabla_total2<-within(tabla_total2, Depmuni<-as.numeric(Depmuni)) 

## cargando capitulos de la encuesta ---------------------------------------
#e,f,k,l

setwd("C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Datos_originales")

###encuestas
encuestas<-read.csv("encuestas.csv")%>%select('Depmuni','DIRECTORIO')
###tabaco
ecapitulos<-read.csv("e_capitulos.csv")%>%select('E_04','DIRECTORIO')%>%filter(E_04==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(E_04=sum(E_04))

summary(ecapitulos)

###alcohol
fcapitulos<-read.csv("f_capitulos.csv")%>%select('F_06','DIRECTORIO')%>%filter(F_06==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(F_06=sum(F_06))

summary(fcapitulos)

###marihuana
kcapitulos<-read.csv("k_capitulos.csv")%>%select('K_03','DIRECTORIO')%>%filter(K_03==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(K_03=sum(K_03))

summary(kcapitulos)

###cocaina
lcapitulos<-read.csv("l_capitulos.csv")%>%select('L_02','DIRECTORIO')%>%filter(L_02==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(L_02=sum(L_02))

summary(lcapitulos)

#No hay ceros por municipio, es decir hay al menos un consumidor de droga en cada municipio.



# Modelos para variable de consumo en los ultimos 12 meses ----------------




#Modelos para tabaco ----
mod_taba_1<-glm(E_04~.-Depmuni,family=poisson(log),data=ecapitulos%>%inner_join(tabla_total2, by='Depmuni'))
stepCriterion(mod_taba_1)
#Incluyendo variables de control forsozamente:
Formula1= E_04~ SEXO.x_1+SEXO.x_2+D2_01_1 + D_07_9 + D2_05_3 + D2_05_1 + D_02_6 + D_02_5 + D_02_8 + D_07_2 + D2_01_2 + D_02_2 + D2_05_4 + D2_05_7 + D2_05_5 
mod_taba_1<-update(mod_taba_1,formula=Formula1)
summary(mod_taba_1)
set.seed(12192129)
#Envelope
envelope(mod_taba_1,type="quantile") #Hay sobredispersion
#como no hay ceros, se usa modelos truncados
mod_taba_2<-overglm(Formula1,data=ecapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
mod_taba_3<-update(mod_taba_2,family="ztnb1")
mod_taba_4<-update(mod_taba_2,family="ztnb2")
mod_taba_5<-update(mod_taba_2,family="ztnbf")
mod_taba_6<-update(mod_taba_1,family=quasipoisson())
AIC(mod_taba_1,mod_taba_2,mod_taba_3,mod_taba_4,mod_taba_5,mod_taba_6) #modelo escogido 4 BNII 
summary(mod_taba_4)
envelope(mod_taba_4,type="quantile")
plot(cooks.distance(mod_taba_4),type="h",main="Distancia de cook para Tabaco")


#Modelos para Alcohol ----
mod_alc_1<-glm(F_06~.-Depmuni,family=poisson(log),data=fcapitulos%>%inner_join(tabla_total2, by='Depmuni'))
stepCriterion(mod_alc_1)
#Incluyendo variables de control forsozamente:
Formula2=F_06~SEXO.x_1+SEXO.x_2+ D_07_3 + D_07_2 + D_02_5 + D_02_6 + D2_01_2 + D2_05_3 + D_07_9 + D2_01_3 + D2_05_1 + D2_01_4 + D_02_2 + D_01_2 + D2_05_4  + D2_05_6 + D_02_3 + D2_05_5 
mod_alc_1<-update(mod_alc_1,Formula2)
summary(mod_alc_1)
envelope(mod_alc_1,type="quantile") #Hay sopbredispersion muy densa
#como no hay ceros, se usa modelo truncado
mod_alc_2<-overglm(Formula2,data=fcapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
mod_alc_3<-update(mod_alc_2,family="ztnb1") #Convergence not achieved!! 
mod_alc_4<-update(mod_alc_2,family="ztnb2")
mod_alc_5<-update(mod_alc_2,family="ztnbf")#Convergence not achieved!! 
mod_alc_6<-update(mod_alc_1,family=quasipoisson())
AIC(mod_alc_1,mod_alc_2,mod_alc_3,mod_alc_4,mod_alc_5,mod_alc_6) #Modelo escodigo es el 4 BNII truncado por ceros.
adjR2(mod_alc_1,mod_alc_6)
set.seed(12192129)
envelope(mod_alc_4,type="quantile")
plot(cooks.distance(mod_alc_4),type="h",main="Distancia de cook para Alcohol")


#Modelos para marihuana ----
mod_mari_1<-glm(K_03~.-Depmuni,family=poisson(log),data=kcapitulos%>%inner_join(tabla_total2, by='Depmuni'))
stepCriterion(mod_mari_1)
#Incluyendo variables de control forsozamente:
Formula3= K_03~ SEXO.x_1+SEXO.x_2+D2_05_3 + D_07_9 + D_02_6 + D_07_1 + D_07_2 + D_02_8 + D2_05_9 + D_02_5  + D2_05_2 + D_02_2 + D2_01_1 + D2_05_8 + D_07_3
mod_mari_1<-update(mod_mari_1,formula=Formula3)
set.seed(12192129)
envelope(mod_mari_1,type="quantile") #Hay sopbredispersion muy densa
#como no hay ceros, se usa modelo truncado
mod_mari_2<-overglm(Formula3,data=kcapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
mod_mari_3<-update(mod_mari_2,family="ztnb1")
mod_mari_4<-update(mod_mari_2,family="ztnb2")
mod_mari_5<-update(mod_mari_2,family="ztnbf")
mod_mari_6<-update(mod_mari_1,family=quasipoisson())
AIC(mod_mari_1,mod_mari_2,mod_mari_3,mod_mari_4,mod_mari_5,mod_mari_6) #Modelo escogido segun aic ztnbf
adjR2(mod_mari_1,mod_mari_6)
summary(mod_mari_5)
set.seed(12192129)
envelope(mod_mari_4,type="quantile")
#plot(residuals2(mod_mari_6))


#Modelos para cocaina ----
mod_coca_1<-glm(L_02~.-Depmuni,family=poisson(log),data=lcapitulos%>%inner_join(tabla_total2, by='Depmuni'))
stepCriterion(mod_coca_1)
Formula4=L_02~ SEXO.x_1+SEXO.x_2+D_07_na + D_02_1  + D_02_4 + D2_05_3 + D2_05_2 + D_07_9 + D2_05_7 + D2_05_5 + D2_01_4 + D2_01_5 + D2_01_3 
mod_coca_1<-update(mod_coca_1,Formula4)
summary(mod_coca_1)
set.seed(12192129)
envelope(mod_coca_1,type="quantile") #Hay subdispersion muy densa
#como no hay ceros, se usa modelo truncado
mod_coca_2<-overglm(Formula4,data=lcapitulos%>%inner_join(tabla_total2, by='Depmuni'),family = "ztpoi")
mod_coca_3<-update(mod_coca_2,family="ztnb1") #no se tiene en cuenta
#mod_coca_4<-update(mod_coca_2,family="ztnb2") #sistema es computacionalmente singular
mod_coca_5<-update(mod_coca_2,family="ztnbf") #modelo apropiado para subdispersion
mod_coca_6<-update(mod_coca_1,family=quasipoisson())
AIC(mod_coca_1,mod_coca_2,mod_coca_3,mod_coca_5,mod_coca_6) 
adjR2(mod_coca_1,mod_coca_6) #escogeremos el de quasiverosimilitud


summary(mod_coca_5)
summary(mod_coca_6)

plot(residuals2(mod_coca_6,type = "deviance"))
mean(residuals2(mod_coca_6,type = "deviance"))
abline(h=0,col="red")





set.seed(12192129)
envelope(mod_coca_6,type="standardized")
residuos=lcapitulos$L_02- fitted(mod_coca_6)
plot(residuos)
summary(mod_coca_5)


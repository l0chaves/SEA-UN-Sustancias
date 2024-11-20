library(dplyr)
#Creando tabal_total2 con regresores:
tabla_total2 <- tabla_total %>%
  select(-which(colnames(tabla_total) %in% colnames(respuestas_por_muni) & colnames(tabla_total) != "Depmuni"))%>%
  select(-c(4:22))
tabla_total2<-within(tabla_total2, Depmuni<-as.numeric(Depmuni)) 

#codigo de municipios:
encuestas<-read.csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/encuestas.csv")%>%select('Depmuni','DIRECTORIO')
#e,f,k,l
#tabaco
ecapitulos<-read.csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/e_capitulos.csv")%>%select('E_04','DIRECTORIO')%>%filter(E_04==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(E_04=sum(E_04))
#tabaco
fcapitulos<-read.csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/f_capitulos.csv")%>%select('F_06','DIRECTORIO')%>%filter(F_06==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(F_06=sum(F_06))
#tabaco
#marihuana
kcapitulos<-read.csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/k_capitulos.csv")%>%select('K_03','DIRECTORIO')%>%filter(K_03==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(K_03=sum(K_03))
#cocaina
lcapitulos<-read.csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/l_capitulos.csv")%>%select('L_02','DIRECTORIO')%>%filter(L_02==1)%>%
  inner_join(encuestas, by = "DIRECTORIO")%>%group_by(Depmuni)%>%summarise(L_02=sum(L_02))



fit<-glm(F_06~.-Depmuni,family=poisson(log),data=fcapitulos%>%inner_join(tabla_total2, by='Depmuni'))
summary(fit)
library(glmtoolbox)
stepCriterion(fit)


#D_01_2+D_01_1+
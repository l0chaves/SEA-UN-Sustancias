setwd('C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios')
tabla_total <- readRDS("tabla_total.rds") 
tabla_total[is.na(tabla_total)] <- 0
row.names(tabla_total)<-tabla_total$Depmuni
respuestas_por_muni <- readRDS("respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni
View(tabla_total)





############################Modelos univariados
library(glmtoolbox)
###############Tobacco
fit1glm<- glm(E_05~SEXO.x_1 + SEXO.x_2 + D_01_1 + D_01_2 + D_02_1 + D_02_2 + D_02_3 + D_02_4 + D_02_5 + D_02_6 + D_02_7 + D_02_8 + D_07_1 + D_07_2 + D_07_3 + D_07_9 + D_07_na + D2_01_1 + D2_01_2 + D2_01_3 + D2_01_4 + D2_01_5 + D2_01_9 + D2_05_1 + D2_05_2 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8 + D2_05_9, data=tabla_total,family = "poisson")
fit1glm<-step(fit1glm,direction="both")
zero.excess(fit1glm,alternative="excess")
summary(fit1glm)
envelope(fit1glm)
fit1glm1<-zeroalt(E_05~ SEXO.x_2 + D_02_3 + D_02_4 + D_02_7 + D_07_1 + 
                    D_07_3 + D2_01_4 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + 
                    D2_05_7|D_02_4  ,data=tabla_total,family = "poi(log)")
fit1glm2<-zeroalt(E_05~ SEXO.x_2 + D_02_3 + D_02_4 + D_02_7 + D_07_1 + 
                    D_07_3 + D2_01_4 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + 
                    D2_05_7  ,data=tabla_total,family = "poi(log)")

fit1glm3<-zeroalt(E_05~ SEXO.x_2 + D_02_3 + D_02_4 + D_02_7 + D_07_1 + 
                    D_07_3 + D2_01_4 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + 
                    D2_05_7|D_07_1+D_07_3+D2_05_3+D2_05_6   ,data=tabla_total,family = "poi(log)")


fit1glm4<-update(fit1glm3,family="nb1")
fit1glm5<-update(fit1glm3,family="nb2")
fit1glm6<-update(fit1glm3,family="nbf")

AIC(fit1glm,fit1glm1,fit1glm2,fit1glm3,fit1glm4,fit1glm5,fit1glm6)
summary(fit1glm3)

stargazer::stargazer(summary(fit1glm3)) #ZAP Model
###############Electronic Cigars
fit2glm<- glm(E_12~SEXO.x_1 + SEXO.x_2 + D_01_1 + D_01_2 + D_02_1 + D_02_2 + D_02_3 + D_02_4 + D_02_5 + D_02_6 + D_02_7 + D_02_8 + D_07_1 + D_07_2 + D_07_3 + D_07_9 + D_07_na + D2_01_1 + D2_01_2 + D2_01_3 + D2_01_4 + D2_01_5 + D2_01_9 + D2_05_1 + D2_05_2 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8 + D2_05_9 , data=tabla_total,family = "poisson")
fit2glm<-step(fit2glm)
envelope(fit2glm)
zero.excess(fit2glm)
fit2glm1<-zeroalt(E_12~  SEXO.x_2 + D_02_3 + D_02_4 + D_02_7 + D_07_1 + 
                    D_07_3 + D2_01_4 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + 
                    D2_05_7,data=tabla_total, family='poi(log)')


summary(fit2glm1)
fit2glm2<-update(fit2glm1,~SEXO.x_2 + D_02_3 + D_02_4 + D_02_7 + D_07_1 + 
                   D_07_3 + D2_01_4 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + 
                   D2_05_7|D_07_1+D_07_3+D2_05_3+D2_05_6)
fit2glm3<-update(fit2glm2,family='nb1')
fit2glm4<-update(fit2glm2,family='nb2')
fit2glm5<-update(fit2glm2,family='nbf')
AIC(fit2glm,fit2glm1,fit2glm2,fit2glm3,fit2glm4,fit2glm5) 
summary(fit2glm2) #ZAP Model



#####################Marijuana
fit3glm<- glm(K_05~SEXO.x_1 + SEXO.x_2 + D_01_1 + D_01_2 + D_02_1 + D_02_2 + D_02_3 + D_02_4 + D_02_5 + D_02_6 + D_02_7 + D_02_8 + D_07_1 + D_07_2 + D_07_3 + D_07_9 + D_07_na + D2_01_1 + D2_01_2 + D2_01_3 + D2_01_4 + D2_01_5 + D2_01_9 + D2_05_1 + D2_05_2 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8 + D2_05_9 , data=tabla_total,family = "poisson")
fit3glm<-step(fit3glm)
zero.excess(fit3glm)
fit3glm1<-zeroalt(K_05 ~ SEXO.x_1 + SEXO.x_2 + D_01_1 + D_02_1 + D_02_2 + D_02_5 + 
                    D_02_6 + D_07_2 + D_07_9 + D2_01_2 + D2_01_4 + D2_05_1 + 
                    D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8,family="poi(log)",data=tabla_total)

summary(fit3glm1)

fit3glm2<-update(fit3glm1,K_05 ~ SEXO.x_1 + SEXO.x_2 + D_01_1 + D_02_1 + D_02_2 + D_02_5 + 
                   D_02_6 + D_07_2 + D_07_9 + D2_01_2 + D2_01_4 + D2_05_1 + 
                   D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8|D_02_6)

fit3glm3<-update(fit3glm2,family='nb1')
fit3glm4<-update(fit3glm2,family='nb2')
fit3glm5<-update(fit3glm2,family='nbf')
AIC(fit3glm,fit3glm1,fit3glm2,fit3glm3,fit3glm4,fit3glm5) 
summary(fit3glm2) #ZAP Model


#####################Cocaine
fit4glm<- glm(L_04~SEXO.x_1 + SEXO.x_2 + D_01_1 + D_01_2 + D_02_1 + D_02_2 + D_02_3 + D_02_4 + D_02_5 + D_02_6 + D_02_7 + D_02_8 + D_07_1 + D_07_2 + D_07_3 + D_07_9 + D_07_na + D2_01_1 + D2_01_2 + D2_01_3 + D2_01_4 + D2_01_5 + D2_01_9 + D2_05_1 + D2_05_2 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8 + D2_05_9 , data=tabla_total,family = "poisson")
fit4glm<-step(fit4glm)
zero.excess(fit4glm)
fit4glm1<-overglm(L_04 ~ SEXO.x_1 + SEXO.x_2 + D_01_1 + D_02_1 + D_02_2 + D_02_5 + 
                    D_02_6 + D_07_2 + D_07_9 + D2_01_3 + D2_05_1 + D2_05_2 + 
                    D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8
                  ,family='nb1',data=tabla_total)
fit4glm2<-update(fit4glm1,family='nb2')
fit4glm3<-update(fit4glm1,family='nbf')
AIC(fit4glm,fit4glm1,fit4glm2,fit4glm3)
summary(fit4glm)#Poisson

###################Alcohol

fit5glm<-glm(F_07~SEXO.x_1 + SEXO.x_2 + D_01_1 + D_01_2 + D_02_1 + D_02_2 + D_02_3 + D_02_4 + D_02_5 + D_02_6 + D_02_7 + D_02_8 + D_07_1 + D_07_2 + D_07_3 + D_07_9 + D_07_na + D2_01_1 + D2_01_2 + D2_01_3 + D2_01_4 + D2_01_5 + D2_01_9 + D2_05_1 + D2_05_2 + D2_05_3 + D2_05_4 + D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8 + D2_05_9 , data=tabla_total,family = "poisson")
fit5glm<-step(fit5glm)
zero.excess(fit5glm) #need fot truncated model
fit5glm1<-overglm(F_07 ~ SEXO.x_2 + D_01_1 + D_02_1 + D_02_2 + D_02_3 + D_02_4 + 
  D_02_7 + D_07_1 + D_07_2 + D_07_3 + D_07_9 + D2_01_1 + D2_01_2 + 
  D2_01_3 + D2_01_5 + D2_05_1 + D2_05_2 + D2_05_3 + D2_05_4 + 
  D2_05_5 + D2_05_6 + D2_05_7 + D2_05_8,data=tabla_total,family = "ztpoi")
fit5glm2<-update(fit5glm1,family="ztnb1")
fit5glm3<-update(fit5glm1,family="ztnb2")
fit5glm4<-update(fit5glm1,family="ztnbf")
AIC(fit5glm,fit5glm1,fit5glm2,fit5glm3,fit5glm4) #ztnb2

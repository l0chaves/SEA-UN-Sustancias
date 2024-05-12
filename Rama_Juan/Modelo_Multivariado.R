#####################################################
#Modelo multivariado
tabla_total <- readRDS("~/Semillero SEA/Rama_Juan/tabla_total.rds")
row.names(tabla_total)<-tabla_total$Depmuni
respuestas_por_muni <- readRDS("~/Semillero SEA/Rama_Juan/respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni
View(tabla_total)
library(mvabund)
rtas_por_muni<-mvabund(tabla_total[,2:4]) #17

fit1<-manyglm(rtas_por_muni~SEXO.x_1+SEXO.x_2+D_01_1+ +D_01_2 +D_02_1+ D_02_2+D_02_3+D_02_4+D_02_5+D_02_6+D_02_7+D_02_8 , data=tabla_total[,-c(1:17)],family = "poisson")

#model<-manyglm(subset(respuestas_por_muni, select = -c(Depmuni))~., family = "poisson", data = tabla_total[,-c(1)])
##GLM univariadp
fit_E_05<-glm(tabla_total$E_05 ~ ., family = "poisson", data = tabla_total[,-c(1:17)])
summary(fit_E_05)

fit_E_12<-glm(tabla_total$E_12 ~ ., family = "poisson", data = tabla_total[,-c(1:17)])
summary(fit_E_12)

library(glmtoolbox)
envelope(fit_E_05)

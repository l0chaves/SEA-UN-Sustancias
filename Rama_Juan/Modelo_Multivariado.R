######################cARGANDO DATOS###############################
tabla_total <- readRDS("~/Semillero SEA/Rama_Juan/tabla_total.rds")
row.names(tabla_total)<-tabla_total$Depmuni
respuestas_por_muni <- readRDS("~/Semillero SEA/Rama_Juan/respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni
#################Librerias necesarias#################################################
library(mvabund)
library(glmtoolbox)
library(glmtoolbox)
######################################################################################

#Modelo multivariado(GLM)----
      ##Modelo poisson negativo multivariado----
rtas_por_muni<-mvabund(tabla_total[,2:4]) #17
fit1<-manyglm(rtas_por_muni~SEXO.x_1+SEXO.x_2+D_01_1+ +D_01_2 +D_02_1+ D_02_2+D_02_3+D_02_4+D_02_5+D_02_6+D_02_7+D_02_8 , data=tabla_total[,-c(1:17)],family = "poisson")
summary(fit1, resamp="residual")
plot(fit1)  #De acuerdo al gráico presentado, hay sobredispersion, por lo que se ba a ausar un modelo bn
  ##Modelo binomial negativo multivariado----
fit2<-manyglm(rtas_por_muni~SEXO.x_1+SEXO.x_2+D_01_1+ +D_01_2 +D_02_1+ D_02_2+D_02_3+D_02_4+D_02_5+D_02_6+D_02_7+D_02_8 , data=tabla_total[,-c(1:17)],family = "negative_binomial")
summary(fit2, resamp="residual") #Hay varias variables significativas interesantes
plot(fit2)

#GLM univariadp----
fit_E_05<-glm(tabla_total$E_05 ~ ., family = "poisson", data = tabla_total[,-c(1:17)])
summary(fit_E_05)

fit_E_12<-glm(tabla_total$E_12 ~ ., family = "poisson", data = tabla_total[,-c(1:17)])
summary(fit_E_12)




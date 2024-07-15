tabla_total <- readRDS("~/Semillero SEA/Rama_Juan/tabla_total.rds")
row.names(tabla_total)<-tabla_total$Depmuni
respuestas_por_muni <- readRDS("~/Semillero SEA/Rama_Juan/respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni
############################Modelos univariados
library(glmtoolbox)
fitglm1<- glm(E_05~SEXO.x_1+SEXO.x_2+D_01_1+ +D_01_2 +D_02_1+ D_02_2+D_02_3+D_02_4+D_02_5+D_02_6+D_02_7+D_02_8 , data=tabla_total,family = "poisson")
summary(fitglm1)
envelope(fitglm1)
zero.excess(fitglm1)
plot(fitglm1)

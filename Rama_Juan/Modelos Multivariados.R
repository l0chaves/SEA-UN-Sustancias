View(Drogas)
rm(list=ls())
load("C:/Users/jufem/Downloads/tablas.RData")
View(d)
library(mvabund)
library(dplyr)
#Respuestas últimos 30 días
library(dplyr)
library(fastDummies)
# Concatenate all operations with summing

  #Ce
tabe<-  inner_join(C_e, encuestas, by = "DIRECTORIO") %>%
  mutate(E_05 = as.numeric(E_05),
         E_12 = as.numeric(E_12))%>%
  filter(E_05 == 1 & E_12 == 1)%>%
  group_by(Depmuni) %>%
  summarise(E_05 = sum(E_05, na.rm = TRUE),
            E_12 = sum(E_12, na.rm = TRUE)) 
  
    #Cf
tabf<-    inner_join(C_f, encuestas, by = "DIRECTORIO") %>%
      mutate(F_07 = as.numeric(F_07))%>%
      filter(F_07 == 1)%>%
      group_by(Depmuni) %>%
      summarise(F_07 = sum(F_07, na.rm = TRUE))
    #Ch
tabh<-    inner_join(C_h, encuestas, by = "DIRECTORIO") %>% 
      mutate(H_04 = as.numeric(H_04))%>%
      filter(H_04 == 1)%>%
      group_by(Depmuni) %>%
      summarise(H_04 = sum(H_04, na.rm = TRUE))
    #Ci
tabi<-    inner_join(C_i, encuestas, by = "DIRECTORIO") %>%
      mutate(I_04 = as.numeric(I_04))%>%
      filter(I_04 == 1)%>%
      group_by(Depmuni) %>%
      summarise(I_04 = sum(I_04, na.rm = TRUE))
    #Cj_1
tabj1<-    inner_join(C_j_1, encuestas, by = "DIRECTORIO") %>%
      mutate(J_04 = as.numeric(J_04))%>%
      filter(J_04 == 1)%>%
      group_by(Depmuni) %>%
      summarise(J_04 = sum(J_04, na.rm = TRUE))
    
    #cj_2
    
tabj2<-    inner_join(C_j_2, encuestas, by = "DIRECTORIO") %>%
      mutate(J_10 = as.numeric(J_10))%>%
      filter(J_10 == 1)%>%
      group_by(Depmuni) %>%
      summarise(J_10 = sum(J_10, na.rm = TRUE))
    #cj3
tabj3<-    inner_join(C_j_3, encuestas, by = "DIRECTORIO") %>%
      mutate(J_12 = as.numeric(J_12))%>%
      filter(J_12 == 1)%>%
      group_by(Depmuni) %>%
      summarise(J_12 = sum(J_12, na.rm = TRUE))
    #Ck
tabk<-    inner_join(C_k, encuestas, by = "DIRECTORIO") %>%
      mutate(K_05 = as.numeric(K_05))%>%
      filter(K_05 == 1)%>%
      group_by(Depmuni) %>%
      summarise(K_05 = sum(K_05, na.rm = TRUE))
    #Cl
tabl<-    inner_join(C_l, encuestas, by = "DIRECTORIO") %>%
      mutate(L_04 = as.numeric(L_04))%>%
      filter(L_04 == 1)%>%
      group_by(Depmuni) %>%
      summarise(L_04 = sum(L_04, na.rm = TRUE))
    #Cm
tabm<-    inner_join(C_m, encuestas, by = "DIRECTORIO") %>%
      mutate(M_04 = as.numeric(M_04))%>%
      filter(M_04 == 1)%>%
      group_by(Depmuni) %>%
      summarise(M_04 = sum(M_04, na.rm = TRUE))
    #cn
tabn<-    inner_join(C_n, encuestas, by = "DIRECTORIO") %>%
      mutate(N_08 = as.numeric(N_08))%>%
      filter(N_08 == 1)%>%
      group_by(Depmuni) %>%
      summarise(N_08 = sum(N_08, na.rm = TRUE))
    #Co
tabo<-    inner_join(C_o, encuestas, by = "DIRECTORIO") %>%
      mutate(O_05 = as.numeric(O_05))%>%
      filter(O_05 == 1)%>%
      group_by(Depmuni) %>%
      summarise(O_05 = sum(O_05, na.rm = TRUE))
    #Cp1
tabp1<-    inner_join(C_p1, encuestas, by = "DIRECTORIO") %>%
      mutate(P_02 = as.numeric(P_02))%>%
      filter(P_02 == 1)%>%
      group_by(Depmuni) %>%
      summarise(P_02 = sum(P_02, na.rm = TRUE))
    #cp2
tabp2<-    inner_join(C_p2, encuestas, by = "DIRECTORIO") %>%
      mutate(P_04 = as.numeric(P_04))%>%
      filter(P_04 == 1)%>%
      group_by(Depmuni) %>%
      summarise(P_04 = sum(P_04, na.rm = TRUE))
    #cp3
 tabp3<-   inner_join(C_p3, encuestas, by = "DIRECTORIO") %>%
      mutate(P_06 = as.numeric(P_06))%>%
      filter(P_06 == 1)%>%
      group_by(Depmuni) %>%
      summarise(P_06 = sum(P_06, na.rm = TRUE))
    
 respuestas_por_muni <- tabe %>%
   full_join(tabf) %>%
   full_join(tabh) %>%
   full_join(tabi) %>%
   full_join(tabj1) %>%
   full_join(tabj2) %>%
   full_join(tabj3) %>%
   full_join(tabk) %>%
   full_join(tabl) %>%
   full_join(tabm) %>%
   full_join(tabn) %>%
   full_join(tabo) %>%
   full_join(tabp1) %>%
   full_join(tabp2) %>%
   full_join(tabp3)
 #saveRDS(respuestas_por_muni, "C:\\Users\\jufem\\OneDrive\\Documentos\\Semillero SEA\\Rama_Juan\\respuestas_por_muni.rds")
View(respuestas_por_muni)
personas_seleccionadas$grupo_edad <- cut(personas_seleccionadas$EDAD, breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("<20", "20-30", "30-40", "40-50", "50-60", ">60", "70-80", "80-90", "90-100"))
Datos=personas_seleccionadas%>%inner_join(d,by="DIRECTORIO")
Datos=Datos%>%inner_join(d2,by="DIRECTORIO")
Datos<-subset(Datos,select=c("SEXO.x","PARENTESCO.y","grupo_edad","D_01","D_02","D_07","D2_01","D2_05","DIRECTORIO"))
covariables_por_Depto<-inner_join(Datos,encuestas,by="DIRECTORIO" )%>% select("Depmuni","SEXO.x","PARENTESCO.y","grupo_edad","D_01","D_02","D_07","D2_01","D2_05","DIRECTORIO")

covariables_por_Depto <- covariables_por_Depto %>%
  dummy_cols(select_columns = c("SEXO.x", "PARENTESCO.y", "grupo_edad", "D_01", "D_02", "D_07", "D2_01", "D2_05"))

covariables_por_Depto_resumen <- covariables_por_Depto[,-c(2:10)] %>%group_by(Depmuni) %>%
  summarise(across(everything(), sum))

tabla_total<-full_join(respuestas_por_muni,covariables_por_Depto_resumen)



#saveRDS(tabla_total, "C:\\Users\\jufem\\OneDrive\\Documentos\\Semillero SEA\\Rama_Juan\\tabla_total.rds")






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


### Alcohol ###
library(readr)
library(stringr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(FactoClass)
library(glmtoolbox)

# importando base de datos
personas_seleccionadas <- read_csv("Drogas/Datos originales/personas_seleccionadas.csv")
personas_seleccionadas <- personas_seleccionadas %>%
  select(DIRECTORIO, SEXO, EDAD, PARENTESCO) %>%
  mutate_all(as.character) %>%
  mutate(EDAD = as.integer(EDAD))


encuestas <- read_csv("Drogas/Datos originales/encuestas.csv")
encuestas <- encuestas %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`ESTADO_ENCUESTA`, -`RECO_DIC`) %>%
  mutate_all(as.character) %>%
  mutate(TOTAL_PERSONAS = as.integer(TOTAL_PERSONAS))


C_f <- read_csv("Drogas/Datos originales/f_capitulos.csv")
C_f <- C_f %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) %>%
  filter(F_03 == 1)

str(C_f)

vdem <- read.csv("Drogas/Datos originales/d_capitulos.csv")
vdem <- vdem %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`)


# dataframe general
df<-NULL
df <- sqldf("select C_f.*, encuestas.Depmuni, encuestas.TIPO, encuestas.SERVICIO,
                    encuestas.ESTRATO, encuestas.TOTAL_PERSONAS
             from C_f 
             left join encuestas on (C_f.DIRECTORIO = encuestas.DIRECTORIO)")


df <- sqldf("select df.*, personas_seleccionadas.SEXO, personas_seleccionadas.EDAD
             from df left join personas_seleccionadas on (df.DIRECTORIO = 
                                                          personas_seleccionadas.DIRECTORIO)")

df <- sqldf("select df.*, D_01, D_02, D_03, D_04, D_05, D_06, D_07, D_08, D_09, D_10
             from df left join vdem on (df.DIRECTORIO = vdem.DIRECTORIO)")


View(df)


df2<-NULL
df2<-df[,c(1,8,35, seq(37,51,1))]
str(df2)


# Categorizando la variable SEXO
df2<- sqldf("select *,
             case when SEXO=1 then 'M'
                  else 'F'
             end as CSEXO
             from df2")

# Categorizando la variable EDAD
df2<- sqldf("select *,
             case when EDAD <= 17 then 'Teenagers'
                  when EDAD <= 24 then 'Young'
                  when EDAD <= 34 then 'Young Adult'
                  when EDAD <= 44 then 'Adult'
                  when EDAD <= 63 then 'Elderly'
                  else 'Third Age'
             end as CEDAD
             from df2")



# Categorizando la variable ESTRATO
df2<- sqldf("select *,
             case when ESTRATO = 1 then 'Bajo-bajo'
                  when ESTRATO = 2 then 'Bajo'
                  when ESTRATO = 3 then 'Medio-bajo'
                  when ESTRATO = 4 then 'Medio'
                  when ESTRATO = 5 then 'Medio-alto'
                  else 'Alto'
             end as CESTRATO
             from df2")



# Categorizando la variable SERVICIO
df2<- sqldf("select *,
             case when SERVICIO = 1 then 'CON_ERG'
                  else 'SIN_ERG'
             end as CSERVICIO
             from df2")


# Categorizando la variable familiares cercanos que se emborrachan frecuentemente
df2<- sqldf("select *,
             case when F_01 = 1 then 'TIENE'
                  else 'NO_TIENE'
             end as CF_01
             from df2")


# Categorizando la variable Depmuni(Residencia)
df2 <- within(df2,{
  Tipo <- ifelse(str_sub(Depmuni,3,5)=="001","Capital","Otro")
  Tipo <- ifelse(Depmuni=="25001","Otro",Tipo)
})

View(df2)
str(df2)

df2$D_01 <- as.character(df2$D_01)
df2$D_02 <- as.character(df2$D_02)
df2$D_03 <- as.character(df2$D_03)
df2$D_04 <- as.character(df2$D_04)
df2$D_06 <- as.character(df2$D_06)
df2$D_07 <- as.character(df2$D_07)
df2$D_08 <- as.character(df2$D_08)
df2$D_09 <- as.character(df2$D_09)
df2$D_10 <- as.character(df2$D_10)

acmdf <- df2[,c(seq(9,15,1))]
#acmdf <- lapply(acsdf, as.factor) #solo para la libreria FactoClass
str(acmdf)
View(acmdf)

############# ACM con la libreria FactoClass ##################

#acm <-dudi.acm(acmdf, scannf = FALSE, nf =3)
#barplot (acm$eig, cex.axis =0.6)
#dev.print(device = xfig, file = "ACMadmiValP.fig")
#eiglst <-data.frame(vp = acm$eig, porce = acm$eig*100/sum(acm$eig),
#                        acupor = cumsum(acm$eig)*100/sum(acm$eig))
#xtable (eiglst, digits=c(1 ,3 ,1 ,1)) # tabla en formato LaTeX

###### grafico
#selin <-seq (25,40000,500)
#length(selin)
#plot(acm, Tcol = FALSE, roweti=as.character(selin), cframe =1, cex.row =0.6,
#       cex.global=0.8, gg = TRUE )

# Nuve de categorias
#plot(acm,Trow=FALSE,cex.global=0.8,cframe=1,gg=TRUE,col.col="black")

#plot(acm,cex.global=0.8,cframe=1,gg=TRUE)

############## ACM con la libreria FactoMineR #############
library(FactoMineR)
library(factoextra)

##al.mca <- MCA(acmdf, graph = FALSE)
#fviz_screeplot(al.mca, addlabels = TRUE, ylim = c(0, 45)) # % de varianza explicada

# Correlacion entre variables
#fviz_mca_var(al.mca, choice = "mca.cor", 
#             repel = TRUE, 
#             ggtheme = theme_minimal())


# cos2 de cada variable
#fviz_mca_var(al.mca, col.var = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#             repel = TRUE, # Avoid text overlapping
#             ggtheme = theme_minimal())



# grafico interesante
#fviz_mca_ind(al.mca, 
#             label = "none", # hide individual labels
#             habillage = "CESTRATO", # color by groups 
#             palette = c("#00AFBB", "#E7B800", "blue","red","green","purple"),
#             addEllipses = TRUE, ellipse.type = "confidence",
#             ggtheme = theme_minimal()) 


#fviz_ellipses(al.mca, c("CSEXO", "CTIPO"),
#              geom = "point")


#fviz_mca_biplot(al.mca, 
#                repel = TRUE, # Avoid text overlapping (slow if many point)
#                ggtheme = theme_minimal())

# ACM con variables e individuos suplementarios

#al.mca2 <- MCA(acmdf, ind.sup = 30000:42441, 
#               quali.sup = 1:2,  graph=FALSE)


#### grafico 1
ggplot(df2, aes(x = CESTRATO, fill = CF_01)) +
  geom_bar(position = "dodge") +
  labs(x = "Estrato", y = "Frecuencia", fill = "Familiar que\nconsume alcohol") +
  theme_minimal()

#### grafico 2
ggplot(df2, aes(x = CEDAD, fill = Tipo)) +
  geom_bar(position = "dodge") +
  labs(x = "Edad", y = "Frecuencia", fill = "Residencia") +
  theme_minimal()

#### grafico de cajas
ggplot(df2, aes(x = CESTRATO, y = EDAD, fill = CESTRATO)) +
  geom_boxplot() +
  labs(x = "Edad", y = "Estrato") +
  theme_minimal()

### grafico de violin
ggplot(df2, aes(x = CEDAD, y = TOTAL_PERSONAS, fill = EDAD)) +
  geom_violin() +
  labs(x = "EDAD", y = "Número de personas que\nconforman el hogar") +
  scale_fill_manual(values = c("Adolescente" = "blue", "Adulto" = "red", "Joven" = "green", "Persona Mayor"="purple")) +
  theme_minimal()

## prueba chi cuadrado para determinar asociacion

pf <- read_csv("Drogas/Datos originales/f_capitulos.csv")
tabla<-table(pf$F_03,pf$F_01)
chisq.test(tabla)


#OR estimado
OR=(tabla[1,1]*tabla[2,2])/(tabla[1,2]*tabla[2,1])
OR

##### Fumadores #####

smoker <- read_csv("Drogas/Datos originales/e_capitulos.csv", col_names = TRUE)
smoker <- smoker %>%
  dplyr::select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`) %>%
  mutate_all(as.character) #%>%
  #filter(E_01 == 1)

table(smoker$E_04) #80 personas no respondieron

smoker <- sqldf("select *
                 from smoker
                 where E_04 in (1,2)")

fdf<-NULL
fdf <- sqldf("select smoker.*, encuestas.Depmuni, encuestas.TIPO, encuestas.SERVICIO,
                    encuestas.ESTRATO, encuestas.TOTAL_PERSONAS
             from smoker 
             left join encuestas on (smoker.DIRECTORIO = encuestas.DIRECTORIO)")


fdf <- sqldf("select fdf.*, personas_seleccionadas.SEXO, personas_seleccionadas.EDAD
             from fdf left join personas_seleccionadas on (fdf.DIRECTORIO = 
                                                          personas_seleccionadas.DIRECTORIO)")
head(fdf)
View(fdf)



fdf2<-NULL
fdf2<-fdf[,c(2,4,10,seq(14,20,1))]
str(fdf2)


# Categorizando la variable SEXO
fdf2<- sqldf("select SEXO, EDAD, ESTRATO, TIPO, SERVICIO, TOTAL_PERSONAS,Depmuni, E_02, E_04, E_10,
             case when SEXO=1 then 'M'
                  else 'F'
             end as CSEXO
             from fdf2")

# Categorizando la variable EDAD
fdf2<- sqldf("select *,
             case when EDAD <= 17 then 'Teenagers'
                  when EDAD <= 24 then 'Young'
                  when EDAD <= 34 then 'Young Adult'
                  when EDAD <= 44 then 'Adult'
                  when EDAD <= 63 then 'Elderly'
                  else 'Third Age'
             end as CEDAD
             from fdf2")



# Categorizando la variable ESTRATO
fdf2<- sqldf("select *,
             case when ESTRATO = 1 then 'Bajo-bajo'
                  when ESTRATO = 2 then 'Bajo'
                  when ESTRATO = 3 then 'Medio-bajo'
                  when ESTRATO = 4 then 'Medio'
                  when ESTRATO = 5 then 'Medio-alto'
                  else 'Alto'
             end as CESTRATO
             from fdf2")


# Categorizando la variable TIPO de vivienda
fdf2<- sqldf("select *,
             case when TIPO = 1 then 'Casa'
                  when TIPO = 2 then 'Apartamento'
                  when TIPO = 3 then 'Cuarto'
                  else 'Otra vivienda'
             end as CTIPO
             from fdf2")

# Categorizando la variable SERVICIO
fdf2<- sqldf("select *,
             case when SERVICIO = 1 then 'CON_ERG'
                  else 'SIN_ERG'
             end as CSERVICIO
             from fdf2")


# Categorizando la variable sistemas electronicos de administracion de nicotina
fdf2<- sqldf("select *,
             case when E_10 = 1 then 'SI_SE'
                  else 'NO_SE'
             end as CE_10
             from fdf2")


# Categorizando la variable Depmuni(Residencia)
fdf2 <- within(fdf2,{
  Tipo <- ifelse(str_sub(Depmuni,3,5)=="001","Capital","Otro")
  Tipo <- ifelse(Depmuni=="25001","Otro",Tipo)
})

head(fdf2)
View(fdf2)

sdf <- fdf2[,c(seq(8,16,1))]
#acmdf <- lapply(acsdf, as.factor) #solo para la libreria FactoClass
str(sdf)
head(sdf)

sdf$E_10 <- ifelse(sdf$E_10==2,0,1)
sdf$E_04 <- ifelse(sdf$E_04==2,0,1)

head(sdf)

## Modelo para fumadores

fit_0<-glm(E_04~CEDAD+CESTRATO+CSEXO+CE_10+E_02+CTIPO+ CE_10*CEDAD+CE_10*CESTRATO, family = binomial(), data = sdf)

# Selección hacia adelante
modelo_forward_s <- stepCriterion(fit_0, criterion = "bic", direction = "forward")

# Seleccion hacia atras
modelo_backward_s <- stepCriterion(fit_0, criterion = "bic",direction = "backward")

fit_1<-glm(E_04~CEDAD+CSEXO+CE_10+CE_10*CEDAD, family = binomial(), data = sdf)
AIC(fit_0,fit_1)
BIC(fit_0,fit_1)
adjR2(fit_0,fit_1)

fit_2<-glm(E_04~1, family = binomial(), data = sdf)
anova2(fit_2,fit_1)

summary(fit_1)
envelope(fit_1)


##############################################################################
####### Matriz de confusión Fumadores ##########

#Fumadores
hist(predict.glm(fit_1, type = "response"))

prob_smoker<-predict.glm(fit_1, type = "response")
predic_smoker<-ifelse(prob_smoker>0.1,1,0)

## Volviendo las columnas factores
actual_levels_F <- factor(sdf$E_04, levels = c(0, 1)) 
predicho_levels_F <- factor(predic_smoker, levels = c(0, 1))

tabla<-table(actual_levels_F, Predicho=predicho_levels_F)
tabla
tpr<-tabla[2,2]/(sum(tabla[2,]))
tpr

tnr<-tabla[1,1]/(sum(tabla[1,]))
tnr


#calculando alpha

alpha <- seq(0,1,0.02)
tpr_tot_smoker<-NULL
tnr_tot_smoker<-NULL
for (i in alpha) {
  predic_smoker_i<-NULL
  print(i)
  alpha_i<-alpha[i]
  predic_smoker_i<-ifelse(prob_smoker>i,1,0)
  predicho_levels_F_i <- factor(predic_smoker_i, levels = c(0, 1))
  tabla_i<-table(Actual=actual_levels_F, Predicho=predicho_levels_F_i)
  tpr_i<-tabla_i[2,2]/(sum(tabla_i[2,]))
  tpr_tot_smoker<-c(tpr_tot,tpr_i)
  tnr_i<-tabla_i[1,1]/(sum(tabla_i[1,]))
  tnr_tot_smoker<-c(tnr_tot,tnr_i)
}

tabla_comp_smoker<-NULL
tabla_comp_smoker<-cbind(tabla_comp_smoker,tpr_tot_smoker)
tabla_comp_smoker<-cbind(tabla_comp_smoker,tnr_tot_smoker)
tabla_comp_smoker<-cbind(alpha,tabla_comp_smoker)

tabla_comp_smoker


#Gráfico

# Crear el dataframe con los datos
data_fumadores <- data.frame(
  alpha = tabla_comp_smoker[seq(1,51,1),1],
  tpr_tot = tabla_comp_smoker[seq(1,51,1),2],
  tnr_tot = tabla_comp_smoker[seq(1,51,1),3]
)

# Calcular la tasa de falsos positivos (FPR)
data_fumadores$fpr_tot <- 1 - data_fumadores$tnr_tot

# Graficar la curva ROC
plot(data_fumadores$fpr_tot, data_fumadores$tpr_tot, type = "l", col = "blue", 
     xlab = "FPR (1 - TNR)", ylab = "TPR (Sensibilidad)", 
     main = "Curva ROC Tabaco",
     xlim = c(0,1), ylim = c(0,1))
grid()

abline(a = 0, b = 1, col = "red", lty = 2)



#### grafico 1
ggplot(fdf2, aes(x = CESTRATO, fill = CE_10)) +
  geom_bar(position = "dodge") +
  labs(x = "Estrato", y = "Frecuencia", fill = "Uso de Sistemas\nElectrónicos") +
  theme_minimal()

#### grafico 2
ggplot(fdf2, aes(x = CEDAD, fill = Tipo)) +
  geom_bar(position = "dodge") +
  labs(x = "Edad", y = "Frecuencia", fill = "Residencia") +
  theme_minimal()

#### grafico de cajas
ggplot(fdf2, aes(x = CESTRATO, y = EDAD, fill = CESTRATO)) +
  geom_boxplot() +
  labs(x = "Edad", y = "Estrato") +
  theme_minimal()

### grafico de viloin
ggplot(fdf2, aes(x = CEDAD, y = TOTAL_PERSONAS, fill = EDAD)) +
  geom_violin() +
  labs(x = "EDAD", y = "Número de personas que\nconforman el hogar") +
  scale_fill_manual(values = c("Adolescente" = "blue", "Adulto" = "red", "Joven" = "green", "Persona Mayor"="purple")) +
  theme_minimal()

## 
ggplot(fdf2, aes(x = as.numeric(E_02))) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma de primera vez que fumó cigarrillo-tabaco",
       x = "Edad",
       y = "Frecuencia")




##### Modelo Consumidores de alcohol #####

f_capitulos <- read_csv("Drogas/Datos originales/f_capitulos.csv")
View(f_capitulos)


# dataframe general
df_alcohol<-NULL
df_alcohol <- sqldf("select f_capitulos.*, encuestas.Depmuni, encuestas.TIPO, encuestas.SERVICIO,
                     encuestas.ESTRATO, encuestas.TOTAL_PERSONAS
                     from f_capitulos 
                     left join encuestas on (f_capitulos.DIRECTORIO = encuestas.DIRECTORIO)")


df_alcohol <- sqldf("select df_alcohol.*, personas_seleccionadas.SEXO, personas_seleccionadas.EDAD
                     from df_alcohol left join personas_seleccionadas on (df_alcohol.DIRECTORIO = 
                                                          personas_seleccionadas.DIRECTORIO)")
View(df_alcohol)



df2_alcohol<-NULL
df2_alcohol<-df_alcohol[,c(5,1,38,39,40,41,42,43,44)]
str(df2_alcohol)


# Categorizando la variable SEXO
df2_alcohol<- sqldf("select SEXO, EDAD, ESTRATO, TIPO, SERVICIO, TOTAL_PERSONAS,Depmuni, F_01, F_03,
             case when SEXO=1 then 'M'
                  else 'F'
             end as CSEXO
             from df2_alcohol")

# Categorizando la variable EDAD
df2_alcohol<- sqldf("select *,
             case when EDAD <= 18 then 'Adolescente'
                  when EDAD <= 26 then 'Joven'
                  when EDAD <= 59 then 'Adulto'
                  else 'Persona Mayor'
             end as CEDAD
             from df2_alcohol")



# Categorizando la variable ESTRATO
df2_alcohol<- sqldf("select *,
             case when ESTRATO = 1 then 'Bajo-bajo'
                  when ESTRATO = 2 then 'Bajo'
                  when ESTRATO = 3 then 'Medio-bajo'
                  when ESTRATO = 4 then 'Medio'
                  when ESTRATO = 5 then 'Medio-alto'
                  else 'Alto'
             end as CESTRATO
             from df2_alcohol")


# Categorizando la variable TIPO de vivienda
df2_alcohol<- sqldf("select *,
             case when TIPO = 1 then 'Casa'
                  when TIPO = 2 then 'Apartamento'
                  when TIPO = 3 then 'Cuarto'
                  else 'Otra vivienda'
             end as CTIPO
             from df2_alcohol")

# Categorizando la variable SERVICIO
df2_alcohol<- sqldf("select *,
             case when SERVICIO = 1 then 'CON_ERG'
                  else 'SIN_ERG'
             end as CSERVICIO
             from df2_alcohol")


# Categorizando la variable familiares cercanos que se emborrachan frecuentemente
df2_alcohol<- sqldf("select *,
             case when F_01 = 1 then 'TIENE'
                  else 'NO_TIENE'
             end as CF_01
             from df2_alcohol")


# Categorizando la variable Depmuni(Residencia)
df2_alcohol <- within(df2_alcohol,{
  Tipo <- ifelse(str_sub(Depmuni,3,5)=="001","Capital","Otro")
  Tipo <- ifelse(Depmuni=="25001","Otro",Tipo)
})

df2_alcohol$F_03 <- ifelse(df2_alcohol$F_03==2,0,1)

View(df2_alcohol)

##### Modelo ####
df2 <- sqldf("select *
              from df2
              where F_06 in (1,2)")
df2$F_06 <- ifelse(df2$F_06==2,0,1)
table(df2$F_06)
View(df2)

fit <- glm(F_06 ~ CSEXO + CEDAD + CESTRATO + CF_01 + CEDAD*CSEXO , family = binomial(), data = df2)

fit_nulo <- glm(F_06 ~ 1, family = binomial(), data = df2)  

# Modelo Forward

# Selección hacia adelante
modelo_forward <- stepCriterion(fit, criterion = "bic", direction = "forward")

# Seleccion hacia atras
modelo_backward <- stepCriterion(fit, criterion = "bic",direction = "backward")

summary(fit)

predicciones <- predict(fit, df2, type = "response")
View(predicciones)

help("envelope")

envelope(fit)

adjR2(fit)

fit2 <- glm(F_06 ~ CF_01 + CSEXO*CEDAD, family = binomial(link = "logit"), data = df2)

summary(fit2)
colnames(df)

table(df2$D_06)


anova2(fit_nulo, fit)

summary(fit)

tabla_contingencia <- table(df2$CESTRATO,df2$F_06)

tabla_con_margenes <- addmargins(tabla_contingencia)


xtable(summary(fit))

envelope(fit)


##############################################################################
####### Matriz de confusión ##########

##Alcohol
summary(fit)
hist(predict.glm(fit, type = "response"))

prob_alc<-predict.glm(fit, type = "response")
predic_alc<-ifelse(prob_alc>0.2,1,0)

## Volviendo las columnas factores
actual_levels <- factor(df2$F_06, levels = c(0, 1)) 
predicho_levels <- factor(predic_alc, levels = c(0, 1))

tabla<-table(Actual=actual_levels, Predicho=predicho_levels)
tabla
tpr<-tabla[2,2]/(sum(tabla[2,]))
tpr

tnr<-tabla[1,1]/(sum(tabla[1,]))
tnr

#calculando alpha

alpha <- seq(0,1,0.02)
tpr_tot<-NULL
tnr_tot<-NULL
for (i in alpha) {
  predic_smoker_i<-NULL
  alpha_i<-alpha[i]
  predic_smoker_i<-ifelse(prob_alc>i,1,0)
  fpredicho_levels <- factor(predic_smoker_i, levels = c(0, 1))
  tabla_i<-table(Actual=actual_levels, Predicho=fpredicho_levels)
  tpr_i<-tabla_i[2,2]/(sum(tabla_i[2,]))
  tpr_tot<-c(tpr_tot,tpr_i)
  tnr_i<-tabla_i[1,1]/(sum(tabla_i[1,]))
  tnr_tot<-c(tnr_tot,tnr_i)
}

tabla_comp<-NULL
tabla_comp<-cbind(tabla_comp,tpr_tot)
tabla_comp<-cbind(tabla_comp,tnr_tot)
tabla_comp<-cbind(alpha,tabla_comp)

tabla_comp
plot(tabla_comp[,1])


#Gráfico

# Crear el dataframe con los datos
data <- data.frame(
  alpha = tabla_comp[,1],
  tpr_tot = tabla_comp[,2],
  tnr_tot = tabla_comp[,3]
)

# Calcular la tasa de falsos positivos (FPR)
data$fpr_tot <- 1 - data$tnr_tot

# Graficar la curva ROC
plot(data$fpr_tot, data$tpr_tot, type = "l", col = "blue", 
     xlab = "FPR (1 - TNR)", ylab = "TPR (Sensibilidad)", 
     main = "Curva ROC Alcohol",
     xlim = c(0,1), ylim = c(0,1))
grid()

abline(a = 0, b = 1, col = "red", lty = 2)


head(data)
tail(data_fumadores)




par(mfrow=c(1,2))







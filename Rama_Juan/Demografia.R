load("C:/Users/jufem/Downloads/tablas.RData")############################################################################################################################
####################################################################################
# ACM / cluster ----
library(FactoMineR)
library(factoextra)
library(fastDummies)
#Z=(fastDummies::dummy_cols(categorias))[,-1]
#dat= prcomp(Z, scale = TRUE)
#fviz_contrib(dat, choice = "var", axes = 1, top = 10)
#fviz_screeplot(dat, addlabels = TRUE, ylim = c(0, 20))
#fviz_pca_var(dat)
#boxplot(Z[,-1],las=2)
##################################Descriptiva ---- #########################################
#############################################################################
library(pyramid)
head(personas_seleccionadas)
tabla<-  table(personas_seleccionadas$SEXO,personas_seleccionadas$EDAD)
datos<-data.frame(Hombres=tabla[2,],Mujeres=tabla[1,] , Edad=colnames(tabla))
pyramid(datos,Llab="Masculino",Rlab="Femenino",Clab="Edad",main="Consumo",Cstep=2,Rcol="#836FFF",Lcol="#FF6347")
#Graficos marginales
ggplot(personas_seleccionadas, aes(x=SEXO)) + geom_bar(fill= c("#DDB4EB", ))
personas_seleccionadas$grupo_edad <- cut(personas_seleccionadas$EDAD, breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c("<20", "20-30", "30-40", "40-50", "50-60", ">60", "70-80", "80-90", "90-100"))

# Graficar por grupos de eedad de 10 años
library(viridis)
ggplot(personas_seleccionadas, aes(x = EDAD, fill = grupo_edad)) + 
  geom_bar() +
  scale_fill_manual(values = c(viridis(6))) +
  labs(title = "Distribución de edades", x = "Edad", y = "Frecuencia")+
  theme(legend.position = "bottom")
#Grafico por percentiles de edad
min_edad <- min(personas_seleccionadas$EDAD, na.rm = TRUE)

# Calcular percentiles de la edad incluyendo el mínimo en el primer percentil
percentiles_edad <- c(min_edad, quantile(personas_seleccionadas$EDAD, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE))
# Crear intervalos de edades basados en percentiles
personas_seleccionadas$percentiles_edad <- cut(personas_seleccionadas$EDAD, breaks = percentiles_edad, labels = c("0-25%", "25-50%", "50-75%", "75-100%"))
# Graficar
ggplot(personas_seleccionadas, aes(x = EDAD, fill = percentiles_edad)) + 
  geom_bar() +
  scale_fill_viridis_d(name="percentiles de la edad",option = "magma") +
  labs(title = "Distribución de edades", x = "Edad", y = "Frecuencia")+
  theme(legend.position = "bottom")

#  Graficos de parentezco
# Convertir PARENTESCO a factor y ordenar por frecuencia
personas_seleccionadas$PARENTESCO <- factor(personas_seleccionadas$PARENTESCO, levels = names(sort(table(personas_seleccionadas$PARENTESCO), decreasing = TRUE)))
# Trazar el gráfico
ggplot(personas_seleccionadas, aes(x = PARENTESCO)) + 
  geom_bar(fill = viridis(10)) +
  labs(title = "Frecuencia de Parentesco", x = "Parentesco", y = "Frecuencia")

#Aporte economico
d$D_01 <- factor(d$D_01, levels = c(1, 2))
levels(d$D_01) <- c("Sí", "No")
ggplot(d, aes(x = D_01)) + geom_bar(fill = viridis(2)) +
  labs(title = "Aporte economico al hogar",  y = "Frecuencia")
#Ocupacion
d$D_02 <- factor(d$D_02, levels = c(1, 2, 3, 4, 5, 6, 7, 8))
levels(d$D_02) <- c("Trabajando", "Buscando Trabajo", "Estudiando", "Oficios del hogar",
                    "Incapacitado permanentemente para trabajar", "Pensionado", "Otro", "Otra actividad")
ggplot(d, aes(x = D_02)) + 
  geom_bar(fill = viridis(8)) +
  labs(title = "En qué actividad ocupó la mayor parte del tiempo\n la semana pasada", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Seguridad Social 
d$D_07 <- factor(d$D_07, levels = c(1, 2, 3, 9))
levels(d$D_07) <- c("Contributivo (EPS)", "Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)",
                    "Subsidiado (EPS-S)", "No sabe, no informa")
ggplot(d, aes(x = D_07)) + 
  geom_bar(fill = viridis(5)) +
  labs(title = "Régimen de seguridad social en salud", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##Información de raza o etnia:
d2$D2_01 <- factor(d2$D2_01, levels = c(1, 2, 3, 4, 5, 9))

levels(d2$D2_01) <- c("Indígena", "Gitano / ROM", "Raizal del archipiélago de San Andrés y Providencia", 
                      "Palanquero de San Basilio", "Negro, mulato, afrodescendiente o afrocolombiano", 
                      "Ninguno de los anteriores")

ggplot(d2, aes(x = D2_01)) + 
  geom_bar(fill = viridis(6)) +
  labs(title = "Raza o Etnia", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###Nivel educativo
d2$D2_05 <- factor(d2$D2_05, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
levels(d2$D2_05) <- c("Ninguno", "Preescolar", "Básica primaria(1-5)", 
                      "Básica secundaria (6-9)", "Media (10-13)", 
                      "Técnica / tecnológica", "Universitaria", 
                      "Postgrado", "No sabe / No informa")
ggplot(d2, aes(x = D2_05)) + geom_bar(fill = viridis(9))+
  labs(title = "Mayor nivel educativo alzanzado y aprobado",  y = "Frecuencia")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

########################Dedscriptiva multivariada

##################################################################################  

#difrencia entre hombres - mujeres y edad significativa?
#chisq.test(tabla)
#Por drogas
library(ggplot2)
ggplot(categorias_por_individuo_edit[,-1], aes(x = categorias, y = cantidad)) +
  geom_col()+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
#Las de valor maximo
max_value <- max(categorias_por_individuo_edit$cantidad)
maxvaal<-categorias_por_individuo_edit[categorias_por_individuo_edit$cantidad == max_value, ]
library(ggplot2)
ggplot(maxvaal[,-1], aes(x = categorias, y = cantidad)) +
  geom_col()+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#########ACP solo de las categorias con valor maximo:
# ACM / cluster ----
library(FactoMineR)
library(factoextra)
library(fastDummies)

















############################################################
#####################################   MAC
######################################################
#mac CON EDAD,SEXO Y PARENTESCO:
str(personas_seleccionadas)
str(d)
Datos=personas_seleccionadas%>%inner_join(d,by="DIRECTORIO")
Datos=Datos%>%inner_join(d2,by="DIRECTORIO")
Datos<-subset(Datos,select=c("SEXO.x","PARENTESCO.y","grupo_edad","D_01","D_02","D_07","D2_01","D2_05"))
library(FactoMineR)
library(factoextra)
uni.mca <- MCA(Datos, graph = FALSE)
print(uni.mca)
##EIgenvalues
library(pander)
eigenval <- get_eigenvalue(uni.mca)
pander(head(eigenval))
#Screelot
fviz_screeplot(uni.mca, addlabels = TRUE) + geom_hline(yintercept = 7.14, linetype = 2, color = "red")
#CRITERIO DE BENZECRI
s<-ncol(Datos)
eigenval[,1]>(1/s)
#--> se calcula tau para los primeros 11 ejes 
eig17<-uni.mca$eig[1:17] 
tau<-(s/(s-1))^2*(eig17-(1/s))^2 
ptau<-tau/sum(tau)*100 #Nuevas tasas de inercia

#hast Dim.17 importate de dim 18 a dim 40 ejes residuales
#así el criterio considera que 23 ejes son parasitos y no aportan informacion
eigenval[1:17,]
#Bajo el citerio de benzecri se retiene un 53.216575% de la inercia

plot(uni.mca,Trow=FALSE,gg=TRUE,col.col="black",cex.global=0.8)

#Biplot
fviz_mca_biplot(uni.mca, repel = TRUE, 
                ggtheme = theme_grey())+labs(
                  title ="           Representación simultanea de los individuos y las categorías")

fviz_mca_biplot(uni.mca)


fviz_mca_var(uni.mca, col.var = "purple", shape.var = 10, repel = TRUE,
             ggtheme = theme_grey())+labs(title = "                     Nube de puntos de las variables categoricas")
#vARIABLES
var <- get_mca_var(uni.mca)
pander(head(var$cos2, 15))


###Agrupando a los individuos.

fviz_mca_ind(uni.mca,
             label = "none",
             habillage = "SEXO",
             pallette = c("#CCCCFF", "#F08080"),
             addEllipses = TRUE,
             ggtheme = theme_grey())
#Se puede observar cómo las elipses de concentración de los puntos correspondientes a las categorías de la variable sexo están diferenciadas entre sí horizontalmente, indicando que la dimensión representada en ese eje (dimensión 1) discrimina entre ambas categorías de la variable.
fviz_ellipses(uni.mca, 1:3, 
              geom = "point")

##Variable Suplementaria: categorias de consumo:



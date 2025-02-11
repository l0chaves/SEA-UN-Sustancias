##MAPAS
library(dplyr)
library(readr)
#library(sqldf)

# El archivo respuestas_po_mini.rds necesario para esta tabla se encuentra en la carpeta de modelos por municipip


setwd("C:/Users/jufem/OneDrive/Documentos/Semillero SEA/Rama_Juan")
respuestas_por_muni <- readRDS("respuestas_por_muni.rds")
row.names(respuestas_por_muni)<-respuestas_por_muni$Depmuni
respuestas_por_muni[is.na(respuestas_por_muni)] <- 0

Municipios <- readRDS("C:/Users/jufem/Downloads/Municipios (1).rds")
tabla_total2<-respuestas_por_muni%>%inner_join(Municipios,by=c("Depmuni"="Depmun"))%>%select(everything(respuestas_por_muni),Departamento, Dep, Municipio,Poblacion)
tabla_total3<-tabla_total2%>%group_by(Dep,Departamento) %>% summarise(across(where(is.numeric), sum, na.rm = TRUE))
##################################
#proyecciones_deprtamentales <- read_excel("DCD-area-sexo-edad-proyepoblacion-dep-2020-2050-ActPostCOVID-19.xlsx", skip = 10,n_max=2079)
#proyecciones_deprtamentales<-within(proyecciones_deprtamentales, {DP<-as.numeric(DP)})
#proyecciones_deprtamentales<-proyecciones_deprtamentales%>%filter(AÑO==2020 & `ÁREA GEOGRÁFICA`=='Total')%>%group_by(DP,DPNOM)
#data2<- proyecciones_deprtamentales%>%summarise(TOTPOB=sum(`Total general`))
############################### Uniendo tablas de proyecciones
tabla_total3<-within(tabla_total3,Dep<-as.numeric(Dep) )
#tabla_total3<-tabla_total3%>%inner_join(data2,by=c("Dep"="DP"))%>%select(everything(tabla_total3),TOTPOB)
##############################
library(sf)
library(ggplot2)

#MGN de colombia x departamento
mgnxdeptos <- st_read("Mapa/MGN_DPTO_POLITICO.shp",quiet=TRUE);mgnxdeptos<-within(mgnxdeptos,DPTO_CCDGO<-as.numeric(DPTO_CCDGO))
#Uniendo la TABLA_OTAL
mapaxdeptos <- mgnxdeptos %>% left_join(tabla_total3,by=c("DPTO_CCDGO"="Dep"));mapaxdeptos<-within(mapaxdeptos,DPTO_CCDGO<-as.character(DPTO_CCDGO))
#Mapas paises fronterizos con colombia
poligcol <- st_read("Mapa/admin00.shp",quiet=TRUE)
paises<-c("Venezuela","Ecuador","Panama","Peru","Brazil")
mapacolombia <- poligcol %>% filter(CNTRY_NAME %in% paises) ;box <- st_bbox(mapaxdeptos)
  #library(readr)
  #write_rds(mapaxdeptos,"mapaxdeptos.rds")
############################
#############################

##########Lo que hay que cargar

#mapacolombia<-read_rds("mapacolombia.rds")

############### Graficando los mapas
######################E_05 tabaco
ggplot() + geom_sf(data=mapacolombia) +
  geom_sf(data=mapaxdeptos,aes(fill=round(E_05/sum(E_05),3)),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapaxdeptos,aes(label=ifelse(E_05/sum(E_05) > 0.10,Departamento,"")),col="black",
               fontface="bold",size=4,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitude",y="Latitude",title="Consumption prevalence of \n cigarettes in the last 30 days",
       fill="prevalence") +
  scale_fill_gradient(low="white",high="red3",n.breaks=5) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"))

######################F_07 alcohol
ggplot() + geom_sf(data=mapacolombia) +
  geom_sf(data=mapaxdeptos,aes(fill=round(F_07/sum(F_07),3)),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapaxdeptos,aes(label=ifelse(F_07/sum(F_07) >0.10,Departamento,"")),col="black",
               fontface="bold",size=4,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitude",y="Latitude",title="Consumption prevalence of \n alcohol in the last 30 days",
       fill="prevalence") +
  scale_fill_gradient(low="white",high="red3",n.breaks= 16) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"))

######################marihuana
ggplot() + geom_sf(data=mapacolombia) +
  geom_sf(data=mapaxdeptos,aes(fill=round(K_05/sum(K_05),3)),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapaxdeptos,aes(label=ifelse(K_05/sum(K_05) >0.12,Departamento,"")),col="black",
               fontface="bold",size=4,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitude",y="Latitude",title="Consumption prevalence of \n marijuana in the last 30 days",
       fill="prevalence") +
  scale_fill_gradient(low="white",high="red3",n.breaks= 16) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"))

######################cocaina
ggplot() + geom_sf(data=mapacolombia) +
  geom_sf(data=mapaxdeptos,aes(fill=round(L_04/sum(L_04),3)),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapaxdeptos,aes(label=ifelse(L_04/sum(L_04) >0.3,Departamento,"")),col="black",
               fontface="bold",size=4,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitude",y="Latitude",title="Consumption prevalence of \n cocaine in the last 30 days",
       fill="prevalence") +
  scale_fill_gradient(low="white",high="red3",n.breaks= 16) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"))

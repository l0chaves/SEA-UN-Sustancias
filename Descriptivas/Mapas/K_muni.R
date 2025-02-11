library(sf)
library(dplyr)
library(ggplot2)
help(st_read)
getwd()
deptoshp <- st_read("Descriptivas/Mapas/MGN_DPTO_POLITICO.shp",quiet=TRUE)
#los mapas se descargan de la pagina del dane

mapdeptos <- deptoshp %>% left_join(deptos,by=c("DPTO_CCDGO"="Dep"))
str(mapdeptos)

mundoshp <- st_read("admin00.shp",quiet=TRUE)

mundocol <- mundoshp %>% 
  filter(CNTRY_NAME %in% c("Peru","Brazil","Venezuela","Ecuador","Panama"))

str(mundocol)
help(geom_sf)
box <- st_bbox(mapdeptos) 
#calcula los limites que envuelve a los poligonos de mapdeptos
box



#Proporcion de la categoria 1
depC1 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "1")/1210)
head(depC1)


ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=Irural),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=mapdeptos,aes(label=ifelse(Irural > 70,Departamento,"")),col="black",
               fontface="bold",size=3,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Índice de\nRuralidad") +
  #scale_fill_gradient(low="white",high="red",n.breaks=5) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) +
  theme(panel.background=element_rect(fill="lightblue"))

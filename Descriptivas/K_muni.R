library(sf)
library(dplyr)
library(ggplot2)

munishp <- st_read("Descriptivas/Mapas/MGN_MPIO_POLITICO.shp",quiet=TRUE)
munishp$Depmuni <- paste(munishp$DPTO_CCDGO, munishp$MPIO_CCDGO, sep = "")

mundoshp <- st_read("Descriptivas/Mapas/admin00.shp",quiet=TRUE)

mundocol <- mundoshp %>% 
  filter(CNTRY_NAME %in% c("Peru","Brazil","Venezuela","Ecuador","Panama"))


#Proporcion de la categoria 1
depC1 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "1")/1210)
head(depC1)

mapmuni <- munishp %>% left_join(depC1,by=c("Depmuni"="Depmuni"))
box <- st_bbox(mapmuni) 

ggplot() +
  geom_sf(data=mapmuni,aes(fill=Icon),col="gray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Índice de\nRuralidad") +
  scale_fill_gradient(low="yellow",high="green4",n.breaks=5) +
  theme(panel.background=element_rect(fill="lightblue"))



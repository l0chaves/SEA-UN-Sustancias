library(sf)
library(dplyr)
library(ggplot2)

munishp <- st_read("Descriptivas/Mapas/MGN_MPIO_POLITICO.shp",quiet=TRUE)
munishp$Depmuni <- paste(munishp$DPTO_CCDGO, munishp$MPIO_CCDGO, sep = "")

mundoshp <- st_read("Descriptivas/Mapas/admin00.shp",quiet=TRUE)

# Descriptivas en la muestra ----
## 1 ----
depC1 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "1")/1210)
head(depC1)

mapmuni1 <- munishp %>% left_join(depC1,by=c("Depmuni"="Depmuni"))
box1 <- st_bbox(mapmuni1) 

ggplot() +
  geom_sf(data=mapmuni1,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box1$xmin,box1$xmax),ylim=c(box1$ymin,box1$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo 1 vez en los ultimos 12 meses",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))


## 2 ----
depC2 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "2")/1210)
head(depC2)

mapmuni2 <- munishp %>% left_join(depC2,by=c("Depmuni"="Depmuni"))
box2 <- st_bbox(mapmuni2) 

ggplot() +
  geom_sf(data=mapmuni2,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box2$xmin,box2$xmax),ylim=c(box2$ymin,box2$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces en los ultimos 12 meses",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 3 ----
depC3 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "3")/1210)
head(depC3)

mapmuni3 <- munishp %>% left_join(depC3,by=c("Depmuni"="Depmuni"))
box3 <- st_bbox(mapmuni3) 

ggplot() +
  geom_sf(data=mapmuni3,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box3$xmin,box3$xmax),ylim=c(box3$ymin,box3$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces mensualmente",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 4 ----
depC4 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "4")/1210)
head(depC4)

mapmuni4 <- munishp %>% left_join(depC4,by=c("Depmuni"="Depmuni"))
box4 <- st_bbox(mapmuni4) 

ggplot() +
  geom_sf(data=mapmuni4,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box4$xmin,box4$xmax),ylim=c(box4$ymin,box4$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces semanalmente",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 5 ----
depC5 <- MD_K %>%
  select(Depmuni, K_04) %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(K_04 == "5")/1210)
head(depC5)

mapmuni5 <- munishp %>% left_join(depC5,by=c("Depmuni"="Depmuni"))
box5 <- st_bbox(mapmuni5) 

ggplot() +
  geom_sf(data=mapmuni5,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box5$xmin,box5$xmax),ylim=c(box5$ymin,box5$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo Diario",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))


# Estimaciones efectos Fijos ----
df <- as.data.frame(cbind(Cat = pred_fijos$categoriasF, Depmuni = MD_K$Depmuni))
## 1 ----
depC1 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "1")/1210)
head(depC1)

mapmuni1 <- munishp %>% left_join(depC1,by=c("Depmuni"="Depmuni"))
box1 <- st_bbox(mapmuni1) 

ggplot() +
  geom_sf(data=mapmuni1,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box1$xmin,box1$xmax),ylim=c(box1$ymin,box1$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo 1 vez en los ultimos 12 meses",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))


## 2 ----
depC2 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "2")/1210)
head(depC2)

mapmuni2 <- munishp %>% left_join(depC2,by=c("Depmuni"="Depmuni"))
box2 <- st_bbox(mapmuni2) 

ggplot() +
  geom_sf(data=mapmuni2,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box2$xmin,box2$xmax),ylim=c(box2$ymin,box2$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces en los ultimos 12 meses",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 3 ----
depC3 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "3")/1210)
head(depC3)

mapmuni3 <- munishp %>% left_join(depC3,by=c("Depmuni"="Depmuni"))
box3 <- st_bbox(mapmuni3) 

ggplot() +
  geom_sf(data=mapmuni3,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box3$xmin,box3$xmax),ylim=c(box3$ymin,box3$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces mensualmente",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 4 ----
depC4 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "4")/1210)
head(depC4)

mapmuni4 <- munishp %>% left_join(depC4,by=c("Depmuni"="Depmuni"))
box4 <- st_bbox(mapmuni4) 

ggplot() +
  geom_sf(data=mapmuni4,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box4$xmin,box4$xmax),ylim=c(box4$ymin,box4$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces semanalmente",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 5 ----
depC5 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "5")/1210)
head(depC5)

mapmuni5 <- munishp %>% left_join(depC5,by=c("Depmuni"="Depmuni"))
box5 <- st_bbox(mapmuni5) 

ggplot() +
  geom_sf(data=mapmuni5,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box5$xmin,box5$xmax),ylim=c(box5$ymin,box5$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo Diario",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))


# Estimaciones efectos Mixtos ----
df <- as.data.frame(cbind(Cat = pred_mixtos$categoriasM, Depmuni = MD_K$Depmuni))
## 1 ----
depC1 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "1")/1210)
head(depC1)

mapmuni1 <- munishp %>% left_join(depC1,by=c("Depmuni"="Depmuni"))
box1 <- st_bbox(mapmuni1) 

ggplot() +
  geom_sf(data=mapmuni1,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box1$xmin,box1$xmax),ylim=c(box1$ymin,box1$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo 1 vez en los ultimos 12 meses",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))


## 2 ----
depC2 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "2")/1210)
head(depC2)

mapmuni2 <- munishp %>% left_join(depC2,by=c("Depmuni"="Depmuni"))
box2 <- st_bbox(mapmuni2) 

ggplot() +
  geom_sf(data=mapmuni2,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box2$xmin,box2$xmax),ylim=c(box2$ymin,box2$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces en los ultimos 12 meses",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 3 ----
depC3 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "3")/1210)
head(depC3)

mapmuni3 <- munishp %>% left_join(depC3,by=c("Depmuni"="Depmuni"))
box3 <- st_bbox(mapmuni3) 

ggplot() +
  geom_sf(data=mapmuni3,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box3$xmin,box3$xmax),ylim=c(box3$ymin,box3$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces mensualmente",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 4 ----
depC4 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "4")/1210)
head(depC4)

mapmuni4 <- munishp %>% left_join(depC4,by=c("Depmuni"="Depmuni"))
box4 <- st_bbox(mapmuni4) 

ggplot() +
  geom_sf(data=mapmuni4,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box4$xmin,box4$xmax),ylim=c(box4$ymin,box4$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo algunas veces semanalmente",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

## 5 ----
depC5 <- df %>%
  group_by(Depmuni) %>%
  summarize(Icon=sum(Cat == "5")/1210)
head(depC5)

mapmuni5 <- munishp %>% left_join(depC5,by=c("Depmuni"="Depmuni"))
box5 <- st_bbox(mapmuni5) 

ggplot() +
  geom_sf(data=mapmuni5,aes(fill=Icon),col="#8C8C8C",linetype="solid", linewidth = 0.3) +
  coord_sf(xlim=c(box5$xmin,box5$xmax),ylim=c(box5$ymin,box5$ymax),expand=FALSE) +
  labs(x="Longitud",y="Latitud",title="Consumo Diario",fill="Proporción \nde consumidores") +
  scale_fill_gradient(low="#F0B51A",high="#B22222",n.breaks=5, limits = c(0, 0.1)) +
  theme(panel.background=element_rect(fill="white"))

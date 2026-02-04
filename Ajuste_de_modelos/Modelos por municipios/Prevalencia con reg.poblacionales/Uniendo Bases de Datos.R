rm(list=ls())
library(dplyr)
library(readxl)
# Proyecciones y retroproyeeciones de poblacion ---------------------------

setwd("~/GitHub/SEMILLERO-SEA-UN/Datos_originales/adicional")
Poblacion <- read_excel("DCD-area-sexo-edad-proypoblacion-Mun-2005-2019 (1).xlsx", 
                        skip = 11) %>%
  filter(`ÁREA GEOGRÁFICA` == "Total", AÑO == 2019) %>%
  select(1:6, 179:267)

  Poblacion$Teenagers <- rowSums(Poblacion[, paste0("Total_", 0:17)], na.rm = TRUE)
  Poblacion$Young <- rowSums(Poblacion[, paste0("Total_", 18:24)], na.rm = TRUE)
  Poblacion$Young_Adult <- rowSums(Poblacion[, paste0("Total_", 25:34)], na.rm = TRUE)
  Poblacion$Adult <- rowSums(Poblacion[, paste0("Total_", 35:44)], na.rm = TRUE)
  Poblacion$Elderly <- rowSums(Poblacion[, paste0("Total_", 45:63)], na.rm = TRUE)
  Poblacion$Third_Age <- rowSums(Poblacion[, paste0("Total_", 64:84)], na.rm = TRUE) + Poblacion$`Total_85 y más`
 # Eliminar las columnas 
  Poblacion <- Poblacion[, !names(Poblacion) %in% paste0("Total_", 0:84)]
  Poblacion$`Total_85 y más` <- NULL
  


# Regimen de seguridad social ---------------------------------------------

    url<-"https://www.datos.gov.co/resource/hn4i-593p.csv"  
  data<-read.csv(url)%>%filter(ano==2019)
  
# Datos raciales ----------------------------------------------------------

 Raza <- read_excel("anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun (1).xlsx", 
                skip = 11)%>%
    filter(`ÁREA GEOGRÁFICA` == "Total", AÑO == 2019)%>%select(-c(1:2,5:7))  
  colnames(Raza)<-c("MPIO","Municipio","Indigena","Gitano(a) o Rrom","Raizal del Archipiélago","Palenquero de San Basilio","Negro(a), mulato(a), afrodescendiente","Ningún grupo étnico-racial")
  
# Base de datos Numero de viviendas en cada municipio ---------------------

Hogares<- read_excel("anexo-proyecciones-hogares-dptal-2018-2050-mpal-2018-2035.xlsx", 
                     sheet = "Proyecciones Hogares mpio", 
                     range = "C11:G3358")
colnames(Hogares)<-c("MPIO","DPMP","Area","Viviendas2018","Viviendas2019")
Hogares<-Hogares%>%filter(Area=="Total")%>%select("MPIO","Viviendas2019")


# Area --------------------------------------------------------------------
library(readr)
TerriData <- read_delim("TerriData_Dim1.txt", 
                        delim = "|", escape_double = FALSE, 
                        col_types = cols(`Dato Numérico` = col_number()), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), 
                        trim_ws = TRUE)

TerriData1<-TerriData%>%filter(Indicador == "Extensión")%>%select(Municipio=Entidad,MPIO=`Código Entidad`,Superficie= `Dato Numérico`) 
TerriData2<-TerriData%>%filter(Indicador == "Densidad poblacional", Año==2019,Mes == 12)%>%select(Municipio=Entidad,MPIO=`Código Entidad`,Densidad= `Dato Numérico`) 







# Vivienda y acceso a servicios públicos ---------------------------------------------------------------

TerriDataDim3 <- read_delim("TerriData_Dim3.txt", 
                        delim = "|", escape_double = FALSE, 
                        col_types = cols(`Dato Numérico` = col_number()), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), 
                        trim_ws = TRUE)

TerriData3 <- TerriDataDim3 %>%
  filter(Indicador == "Penetración de banda ancha", Año == 2019,Mes == 12) %>%
  select(Municipio = Entidad,
         MPIO = `Código Entidad`,
         Internet_Coverage = `Dato Numérico`)

TerriData4 <- TerriDataDim3 %>%
  filter(Indicador == "Cobertura de alcantarillado (REC)", Año == 2019,Mes == 12) %>%
  select(Municipio = Entidad,
         MPIO = `Código Entidad`,
         Sewer_coverage = `Dato Numérico`)



# Educacion ---------------------------------------------------------------
TerriDataDim4 <- read_delim("TerriData_Dim4.txt", 
                            delim = "|", escape_double = FALSE, 
                            col_types = cols(`Dato Numérico` = col_number()), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

TerriData6 <- TerriDataDim4 %>%
  filter(
    Indicador == "Cobertura bruta en educación primaria",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
    primary_school_coverage = `Dato Numérico`
  )
TerriData7 <- TerriDataDim4 %>%
  filter(
    Indicador == "Cobertura bruta en educación secundaria",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
    secondary_school_coverage = `Dato Numérico`
  )

TerriData8 <- TerriDataDim4 %>%
  filter(
    Indicador == "Cobertura bruta en educación media",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
    HighSchool_coverage = `Dato Numérico`
  )



TerriData9 <- TerriDataDim4 %>%
  filter(
    Indicador == "Tasa de deserción intra-anual del sector oficial en educación básica y media (Desde transición hasta once)",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
    dropout_rate = `Dato Numérico`
  )


# Seguridad Y Convivencia Ciudadana ---------------------------------------
TerriDataDim6 <- read_delim("TerriData_Dim6.txt", 
                            delim = "|", escape_double = FALSE, 
                            col_types = cols(`Dato Numérico` = col_number()), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)
TerriData10 <- TerriDataDim6 %>%
  filter(
    Indicador == "Tasa de violencia intrafamiliar por cada 100.000 habitantes",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
     Domestic_Violence_Rate= `Dato Numérico`
  )



# Health ------------------------------------------------------------------

TerriDataDim5 <- read_delim("TerriData_Dim5.txt", 
                            delim = "|", escape_double = FALSE, 
                            col_types = cols(`Dato Numérico` = col_number()), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)
TerriData11 <- TerriDataDim5 %>%
  filter(
    Indicador == "Tasa de negligencia y abandono",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
    Neglect_and_abandonment= `Dato Numérico`
  )
TerriData12 <- TerriDataDim5 %>%
  filter(
    Indicador == "Afiliados al SGSSS",
    Año == 2019,
    Mes == 12
  ) %>%
  select(
    Municipio = Entidad,
    MPIO = `Código Entidad`,
    Social_security= `Dato Numérico`
  )




## Union Terridatas
TerriDatas <- TerriData1 %>%
  left_join(TerriData2, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData3, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData4, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData6, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData7, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData8, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData9, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
  left_join(TerriData10, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
left_join(TerriData11, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))%>%
left_join(TerriData12, by = c("Municipio" = "Municipio", "MPIO" = "MPIO"))

TerriDatas$MPIO[TerriDatas$MPIO == 88000] <- 88001 #Error en la marcacion de la DIvipola


View(TerriDatas)

#Uniendo las poblacionales

TablaTotal <- Poblacion %>%
  inner_join(Hogares, by = "MPIO") %>%
  inner_join(Raza, by = c("MPIO"))%>%
  left_join(TerriDatas, by = c("MPIO")) %>%select(-c(Municipio.x,Municipio.y,DP,DPNOM,AÑO, `ÁREA GEOGRÁFICA`)) 
colnames(TablaTotal)[colnames(TablaTotal) == "Total Hombres"] <- "Pob_Hombres"
colnames(TablaTotal)[colnames(TablaTotal) == "Total Mujeres"] <- "Pob_Mujeres"
colnames(TablaTotal)[colnames(TablaTotal) == "Total General"] <- "Poblacion"
str(TablaTotal)

TablaTotal<-within(TablaTotal,{ Superficie<- Superficie/100   #COMO SUPERFICIE ESTA EN HECTAREAS, VAMOS A CAMBIARLA A KM2
                         Densidad_Vivienda<-Viviendas2019/Superficie #Vivienda/km2
                         
                         })

View(TablaTotal)
saveRDS(TablaTotal, "~/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios/Regresores.rds")


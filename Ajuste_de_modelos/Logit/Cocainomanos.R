library(readr)
library(stringr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(FactoClass)
library(glmtoolbox)


encuestas <- read_csv("Drogas/Datos originales/encuestas.csv")
encuestas <- encuestas %>%
  select(-`SECUENCIA_ENCUESTA`, -`SECUENCIA_P`, -`ORDEN`, -`ESTADO_ENCUESTA`, -`RECO_DIC`) %>%
  mutate_all(as.character) %>%
  mutate(TOTAL_PERSONAS = as.integer(TOTAL_PERSONAS))


l_capitulos<-read_csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/l_capitulos.csv")
d_capitulos<-read_csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/d_capitulos.csv")
d2_capitulos<-read_csv("C:/Users/Embag/OneDrive/Documents/GitHub/SEMILLERO-SEA-UN/Datos_originales/d2_capitulos.csv")


df_cocaina<-NULL
df_cocaina <- sqldf("select l_capitulos.*, encuestas.Depmuni, encuestas.TIPO, encuestas.SERVICIO,
                     encuestas.ESTRATO, encuestas.TOTAL_PERSONAS
                     from l_capitulos 
                     left join encuestas on (l_capitulos.DIRECTORIO = encuestas.DIRECTORIO)")


df_cocaina <- sqldf("select df_cocaina.L_02,df_cocaina.DIRECTORIO ,personas_seleccionadas.SEXO, personas_seleccionadas.EDAD,df_cocaina.ESTRATO
                     from df_cocaina left join personas_seleccionadas on (df_cocaina.DIRECTORIO = 
                                                          personas_seleccionadas.DIRECTORIO)")

df_cocaina<-sqldf("select df_cocaina.*, d_capitulos.*
                  from df_cocaina left join d_capitulos on (df_cocaina.DIRECTORIO = d_capitulos.DIRECTORIO )")

library(dplyr)
df_cocaina<-sqldf("select df_cocaina.*, d2_capitulos.*
                  from df_cocaina left join d2_capitulos on (df_cocaina.DIRECTORIO = d2_capitulos.DIRECTORIO )")

  
View(df_cocaina)

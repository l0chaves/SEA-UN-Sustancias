library(dplyr)
load("Limpieza_tablas/tablas.RData")
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# CREACION DE DATA FRAMES ----

## ID sustancias ----
### ilegales ----
DF_ilicitas <- personas_seleccionadas %>%
  left_join(d,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(entorno,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(tratamiento,by=c("DIRECTORIO"="DIRECTORIO")) %>%  
  left_join(trabajo,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_g[, c("DIRECTORIO", "Y")], by=c("DIRECTORIO"="DIRECTORIO"))

prop.table(table(DF_ilicitas$Y))

### legales ----
DF_legales <- personas_seleccionadas %>%
  left_join(d,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(entorno,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(tratamiento,by=c("DIRECTORIO"="DIRECTORIO")) %>%  
  left_join(trabajo,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_e[, c("DIRECTORIO", "E_01")], by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_f[, c("DIRECTORIO", "F_03")], by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate(Y = case_when(E_01 == "1" ~ "1", F_03 == "1" ~ "1", TRUE ~ "2")) %>%
  select(-`E_01`, -`F_03`)

prop.table(table(DF_legales$Y))

# Crear categorías ----
# Se mira solo si han consumido algún vez en la vida ya que aun no hay def de consumo

#### categóricas mutuamente excluyentes ----
Tipo_consumo <- C_g %>%
  filter(Y == "1") %>%
  select(DIRECTORIO, A = `G_11_A`, B = `G_11_B`, C = `G_11_C`, D = `G_11_D`, 
         E = `G_11_E`, `F` = `G_11_F`, G = `G_11_G`, H = `G_11_H`, I = `G_11_I`,
         J = `G_11_J`, K = `G_11_K`, L = `G_11_L`, M = `G_11_M`, N = `G_11_N`, 
         O = `G_11_O`, P = `G_11_P`, Q = `G_11_Q`, R = `G_11_R`, S = `G_11_S`, 
         `T` = `G_11_T`, U = `G_11_U`, V = `G_11_V`)

# Se cambian las categorías de la variable original para T y F
Tipo_consumo <- Tipo_consumo %>%
  mutate_at(vars(-DIRECTORIO), ~ifelse(. == "1", TRUE, FALSE))

# Función para obtener las categorías de cada individuo
obtener_categorias <- function(fila) {
  categorias <- names(fila)[fila]
  return(paste(categorias, collapse = ", "))
}

categorias_por_individuo <- apply(Tipo_consumo[, -1], 1, obtener_categorias)

#data frame organizado de las categorias por ind. 
categorias_por_individuo_edit <- as.data.frame(cbind(DIRECTORIO =Tipo_consumo$DIRECTORIO,
                                                     categorias = categorias_por_individuo,
                                                     cantidad = str_count(categorias_por_individuo, ",")+1))

categorias <- unique(categorias_por_individuo)


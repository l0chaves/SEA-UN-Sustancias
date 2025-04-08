library("dplyr")
library("DescTools")
library("readr")

# Vuelve a cargar sin el filtro de los consumidores 
C_f <- read_csv("Datos_originales/f_capitulos.csv")
C_f <- C_f %>%
  mutate_all(as.character) %>% mutate(F_04 = as.integer(F_04))

C_e <- read_csv("Datos_originales/e_capitulos.csv")
C_e <- C_e %>%
  mutate_all(as.character) %>% mutate(E_02 = as.integer(E_02))

# ------------------------------------------------------------------------------ #
# "Factores genéticos": Si hay familiares que consuman sustancias o se emborrachen frecuentemente
familia <- entorno %>%
  left_join(C_f, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  mutate(FG_01 = case_when((G_01 == "1" | F_01 == "1") ~ "1", TRUE ~ "2")) %>%
  dplyr::select(FG_01, DIRECTORIO)

# ------------------------------------------------------------------------------ #
# Se miran las percepción del riesgo de consumo
percepcion <- d %>%
   dplyr::select(D_11_A, D_11_B, D_11_C, D_11_D, D_11_E, D_11_F, D_11_G, D_11_H, D_11_I,
         D_11_J, D_11_K, D_11_L, D_11_M, D_11_N, DIRECTORIO)

# Se verifica si la persona tiene o no conocimiento del riesgo sobre todas las sustancias
percepcion$D_11 <- as.numeric(!apply(percepcion == "5", 1, any))

# Se cuentan con los registros de las personas que si tienen una percepción del riesgo para el puntaje
percepcion2 <- percepcion[!apply(percepcion == "5", 1, any), ]

# al estar en escala likert se calcula la mediana por cada individuo
row_median <- function(row) {
  return(median(as.numeric(row)))
}

percepcion$D_11_P <- ifelse(percepcion$D_11 == 0, NA,
                            apply(percepcion[,-c(15, 16)], 1, row_median))
# ------------------------------------------------------------------------------ #
#edad a la que consumió por primer vez alguna droga incluyendo alcohol y tabaco
edad <- C_g %>%
  left_join(C_e, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(C_f, by=c("DIRECTORIO"="DIRECTORIO")) %>%
   dplyr::select(G_11_A_ANIOS, G_11_B_ANIOS, G_11_C_ANIOS, G_11_D_ANIOS, G_11_E_ANIOS, 
         G_11_F_ANIOS, G_11_G_ANIOS, G_11_H_ANIOS, G_11_I_ANIOS, G_11_J_ANIOS, 
         G_11_K_ANIOS, G_11_L_ANIOS, G_11_M_ANIOS, G_11_N_ANIOS, G_11_O_ANIOS, 
         G_11_P_ANIOS, G_11_Q_ANIOS, G_11_R_ANIOS, G_11_S_ANIOS, G_11_T_ANIOS, 
         G_11_U_ANIOS, G_11_V_ANIOS, E_02, F_04, DIRECTORIO)

#Aquellos que consumieron a los 0 años no han consumido 
edad[edad == 0] <- NA
edad$G_11 <- apply(edad[,-25], 1,  function(x) min(x, na.rm = TRUE))
#Regresa infinito para aquellos no han consumido ningun tipo de sustancia
edad$G_11[!is.finite(edad$G_11)] <- 0
# ------------------------------------------------------------------------------ #

control <- familia %>% 
  left_join(entorno, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(percepcion, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(edad, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(d, by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(FG_01, G_02, D_11, D_11_P, G_11, D_09, DIRECTORIO) %>%
  mutate_at(c(1, 2, 3, 6), as.factor)

summary(control)
rm(familia, percepcion, edad)

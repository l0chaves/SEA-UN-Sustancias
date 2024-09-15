load("C:/Users/luluf/OneDrive - Universidad Nacional de Colombia/Semillero/Limpieza_tablas/tablas.RData")
rm(list = setdiff(ls(), c("C_l", "control", "d", "d2", "encuestas")))

library("MASS")
library("dplyr")
library("DescTools")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Co-variables ----
X <- d %>%
  left_join(d2,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  left_join(encuestas,by=c("DIRECTORIO"="DIRECTORIO")) %>%
  dplyr::select(`DIRECTORIO`, `D_01`, `D_02`, `D_07`, `D_08`, `D_10`,
                `D2_01`, `D2_03`, `D2_05`, TOTAL_PERSONAS, `D_05`) %>%
  mutate_at(vars(2:9), as.factor) %>%
  mutate_at(vars(10, 11), as.numeric)

summary(X)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Prevalence models per municipality ----------------------

# Clear the workspace to start fresh
rm(list = ls())

# Load required packages
library(dplyr)   # for data manipulation (mutate, select, across, etc.)
library(readxl)  # for reading Excel files (not used yet in this script)
library(glmtoolbox) #overdispered models
# ---------------------------------------------------------
# Load ENCSPA data (municipality-level covariates)
Regresores <- readRDS("~/GitHub/SEMILLERO-SEA-UN/Ajuste_de_modelos/Modelos por municipios/Regresores.rds")

# Ensure municipality code (MPIO) is numeric for consistency
Regresores <- within(Regresores, { MPIO <- as.numeric(MPIO) })

# ---------------------------------------------------------
# Reclassify ethnic categories:
# Add counts of "Palenquero de San Basilio" and "Raizal del Archipiélago"
# into the broader "Afrodescendent" category, then remove the original columns
Regresores <- within(Regresores, {
  `Negro(a), mulato(a), afrodescendiente` <- `Negro(a), mulato(a), afrodescendiente` +
    `Palenquero de San Basilio` +
    `Raizal del Archipiélago`
  rm(`Palenquero de San Basilio`, `Raizal del Archipiélago`)
})

# ---------------------------------------------------------
# Define demographic variables (population counts by sex, age, and ethnicity)
vars_demograficas <- c(
  "Pob_Hombres", "Pob_Mujeres", "Teenagers", "Young", "Young_Adult",
  "Adult", "Elderly", "Third_Age", "Indigena", "Gitano(a) o Rrom",
  "Negro(a), mulato(a), afrodescendiente", "Ningún grupo étnico-racial"
)

# Convert raw counts into percentages of total population
# Each variable is divided by Poblacion, multiplied by 100, and rounded to 3 decimals
Regresores <- Regresores %>%
  mutate(across(all_of(vars_demograficas), ~ round(100 * . / Poblacion, 3)))

# ---------------------------------------------------------
# Load survey metadata (DIVIPOLA codes and directories)
# DIVIPOLA = official DANE's code for municipalities in Colombia
setwd("C:/Users/jufem/OneDrive/Documentos/GitHub/SEMILLERO-SEA-UN/Datos_originales")

encuestas <- read.csv("encuestas.csv") %>%
  select(MPIO = 'Depmuni', 'DIRECTORIO')

# ---------------------------------------------------------
# Selected individuals (with survey expansion factors)
# SW = expansion factor (FEX_C), DIRECTORIO = household ID
pselec <- read.csv("personas_seleccionadas.csv") %>%
  select(SW = 'FEX_C', 'DIRECTORIO')

# ---------------------------------------------------------
# Tobacco consumption (E module)
# E_04 = indicator for tobacco use
ecapitulos <- read.csv("e_capitulos.csv") %>%
  select('E_04', 'DIRECTORIO') %>%
  filter(E_04 == 1) %>%                                # keep only tobacco users
  inner_join(pselec, by = "DIRECTORIO") %>%            # add expansion factors
  inner_join(encuestas, by = "DIRECTORIO") %>%         # add municipality codes
  group_by(MPIO) %>%
  summarise(E_04 = round(sum(E_04 * SW), 0))           # weighted count per municipality
summary(ecapitulos)

# ---------------------------------------------------------
# Alcohol consumption (F module)
# F_06 = indicator for alcohol use
fcapitulos <- read.csv("f_capitulos.csv") %>%
  select('F_06', 'DIRECTORIO') %>%
  filter(F_06 == 1) %>%                                # keep only alcohol users
  inner_join(pselec, by = "DIRECTORIO") %>%            # add expansion factors
  inner_join(encuestas, by = "DIRECTORIO") %>%         # add municipality codes
  group_by(MPIO) %>%
  summarise(F_06 = round(sum(F_06 * SW), 0))           # weighted count per municipality
summary(fcapitulos)

# ---------------------------------------------------------
# Alternative version without expansion factors (unweighted counts)
# fcapitulos <- read.csv("f_capitulos.csv") %>%
#   select('F_06','DIRECTORIO') %>%
#   filter(F_06 == 1) %>%
#   inner_join(encuestas, by = "DIRECTORIO") %>%
#   group_by(MPIO) %>%
#   summarise(F_06 = sum(F_06))
# summary(fcapitulos)

# ---------------------------------------------------------
# Marijuana consumption (K module)
# K_03 = indicator for marijuana use
kcapitulos <- read.csv("k_capitulos.csv") %>%
  select('K_03', 'DIRECTORIO') %>%
  filter(K_03 == 1) %>%                                # keep only marijuana users
  inner_join(pselec, by = "DIRECTORIO") %>%            # add expansion factors
  inner_join(encuestas, by = "DIRECTORIO") %>%         # add municipality codes
  group_by(MPIO) %>%
  summarise(K_03 = round(sum(K_03 * SW), 0))           # weighted count per municipality
summary(kcapitulos)

# ---------------------------------------------------------
# Cocaine consumption (L module)
# L_02 = indicator for cocaine use
lcapitulos <- read.csv("l_capitulos.csv") %>%
  select('L_02', 'DIRECTORIO') %>%
  filter(L_02 == 1) %>%                                # keep only cocaine users
  inner_join(pselec, by = "DIRECTORIO") %>%            # add expansion factors
  inner_join(encuestas, by = "DIRECTORIO") %>%         # add municipality codes
  group_by(MPIO) %>%
  summarise(L_02 = round(sum(L_02 * SW), 0))           # weighted count per municipality
summary(lcapitulos)

# ---------------------------------------------------------
# Notes:
# All municipalities have >0 consumers for each substance.
#  This implies that prevalence models must be estimated using  zero-truncated count models (since zeros are not observed).

# Prevalence Models -------------------------------------------------------


# Tobacco -----------------------------------------------------------------


tabaco<-ecapitulos%>%inner_join(Regresores, by='MPIO')

Formula1<-E_04~.-MPIO-DPMP -Poblacion
mod_taba_1_1<-glm(Formula1,offset=log(Poblacion),family=poisson(log),data=tabaco)
#Including variables of Density 
mod_taba_1 <- step(mod_taba_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_taba_1)
#Second automatic variable selection
stepCriterion(mod_taba_1)

#Formula1<-E_04 ~Internet_Coverage  + Domestic_Violence_Rate + Sewer_coverage  + secondary_school_coverage + primary_school_coverage  
Formula1<- E_04~ Domestic_Violence_Rate+ `Negro(a), mulato(a), afrodescendiente`+Sewer_coverage    + Internet_Coverage 

mod_taba_1<-glm(Formula1,offset=log(Poblacion),family=poisson(log),data=tabaco)

#Overdisperesd models

mod_taba_2<-overglm(Formula1  ,offset=log(Poblacion),family = "ztnb1(log)",data=tabaco)
mod_taba_3<-update(mod_taba_2 ,family="ztnb2(log)")
mod_taba_4<-update(mod_taba_2 ,family="ztnbf(log)")
mod_taba_5<-update(mod_taba_1,family=quasipoisson())
AIC(mod_taba_1,mod_taba_2,mod_taba_3,mod_taba_4)
#adjR2(mod_taba_1,mod_taba_5)

summary(mod_taba_4)

#Envelope
set.seed(123)
envelope(mod_taba_2,type="quantile")  #Great fititng


# Alcohol models ----------------------------------------------------------

alcohol<-fcapitulos%>%inner_join(Regresores, by='MPIO') #creating dataframe that joins covariables with alcohol counts

Formula2<-F_06~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda #formula for model

mod_alc_1_1<-glm(Formula2,offset=log(Poblacion),family=poisson(log),data=alcohol) #Initial poisson model 
#First automatic variable selection
mod_alc_1 <- step(mod_alc_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_alc_1)
#Second automatic variable selection
stepCriterion(mod_alc_1)

Formula2<- F_06~  Domestic_Violence_Rate  + Indigena  +   Sewer_coverage  

mod_alc_1_1<-glm(Formula2,offset=log(Poblacion),family=poisson(log),data=alcohol)
#Overdispersed data models:
mod_alc_2<-overglm(Formula2,offset=log(Poblacion),family = "ztnb1(log)",data=alcohol) #ZTNB1
mod_alc_3<-update(mod_alc_2 ,family="ztnb2(log)") #ZTNB2
mod_alc_4<-update(mod_alc_2 ,family="ztnbf(log)") #ZTNBF
#comparing models:
AIC(mod_alc_1,mod_alc_2,mod_alc_3,mod_alc_4)

summary(mod_alc_2)


set.seed(456)
envelope(mod_alc_2)


# Modelos de marihuana ----------------------------------------------------
marihuana<-kcapitulos%>%inner_join(Regresores, by='MPIO')
Formula3<-K_03~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_mari_1_1<-glm(Formula3,offset=log(Poblacion),family=poisson(log),data=marihuana)
summary(mod_mari_1_1)
#First automatic variable selection
mod_mari_1<-step(mod_mari_1_1,direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))

#Second automatic variable selection
stepCriterion(mod_mari_1)

 Formula3<- K_03~  Sewer_coverage + dropout_rate   +Teenagers
mod_mari_1_1<-glm(Formula3,offset=log(Poblacion),family=poisson(log),data=marihuana)
#Overdispersed models
mod_mari_2<-overglm(formula = Formula3,  family = "ztnb1", data = marihuana, offset = log(Poblacion))
mod_mari_3<-update(mod_mari_2,family = "ztnb2" )
mod_mari_4<-update(mod_mari_2,family = "ztnbf" )

AIC(mod_mari_1,mod_mari_2,mod_mari_3,mod_mari_4)


summary(mod_mari_4)


# MOdelos cocaina ---------------------------------------------------------
coca<-lcapitulos%>%inner_join(Regresores, by='MPIO')
Formula4<-L_02~.-MPIO-DPMP -Poblacion-Viviendas2019+Densidad+Densidad_Vivienda
mod_coca_1_1<-glm(Formula4,offset=log(Poblacion),family=poisson(log),data=coca)
#Incluyendo variables de control forsozamente:
mod_coca_1 <- step(mod_coca_1_1, direction = "both", scope = list(lower = . ~ Densidad + Densidad_Vivienda))
summary(mod_coca_1)
set.seed(123)
envelope(mod_coca_1,residuals="standarized")#sobredispersion 

#Second automatic variable selection
stepCriterion(mod_mari_1)

Formula4<- L_02 ~ Neglect_and_abandonment + Sewer_coverage + Domestic_Violence_Rate  +  Teenagers                                  

mod_coca_1 <- glm(Formula4,offset=log(Poblacion),family=poisson(log),data=coca)
summary(mod_coca_1)

mod_coca_2<-overglm(Formula4, family = "ztnb1", data = coca, offset = log(Poblacion)) 
mod_coca_3<-update(mod_coca_2,family = "ztnb2")
mod_coca_4<-update(mod_coca_2,family = "ztnbf")

AIC(mod_coca_1,mod_coca_2,mod_coca_3,mod_coca_4)

summary(mod_coca_4)


 ###########################
 # Segunda entrega EYP1510 #
 ###########################


# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(vtable)

# Importación -------------------------------------------------------------
data = rio::import("Base Salud 2017.xlsx")
data[data == -8888] = NA
data[data == -9999] = NA
data[data == -5555] = NA
data[data$Diabetes == 3,]$Diabetes = NA
data[data$Presión_alta == 4,]$Presión_alta = NA

# Variables ---------------------------------------------------------------

# Exploración
vtable(data)
summary(data)

# Corrección y creación de variables
data = data %>% 
  mutate(Zona = case_when(Region<5~1,
                            Region<7~2,
                            Region<8~0,
                            Region<11~2,
                            Region>=11~3),
         Zona = factor(Zona, levels = c(0,1,2,3),
                         labels = c("RM",
                                    "Norte",
                                    "Centro-sur",
                                    "Sur")),
         Comuna = factor(Comuna),
         Edad_Cat = case_when(Edad<25~1,
                              Edad<45~2,
                              Edad<65~3,
                              Edad>=65~4),
         Edad_Cat = factor(Edad_Cat, levels = c(1,2,3,4),
                           labels = c("15 a 24 años",
                                      "25 a 44 años",
                                      "45 a 64 años",
                                      "65 años o más")),
         Sexo = factor(Sexo, levels = c(1,2),
                       labels = c("Hombre", "Mujer")),
         `Que_diría_de_su_salud?` = factor(`Que_diría_de_su_salud?`,
                                           levels = c(1,2,3,4,5),
                                           labels = c("Muy Buena",
                                                      "Buena",
                                                      "Regular",
                                                      "Mala",
                                                      "Muy Mala")),
         `Usa_lentes?` = factor(`Usa_lentes?`, levels = c(1,2),
                                labels = c("Sí", "No")),
         `Se_ha_sentido_deprimido?` = factor(`Se_ha_sentido_deprimido?`, 
                                             levels = c(1,2,3),
                                             labels = c("Sí",
                                                        "No",
                                                        "Consumo de antidepresivos")),
         Depresión = factor(Depresión, levels = c(1,2),
                            labels = c("Sí", "No")),
         Fuma = factor(Fuma, level = c(1,2),
                       labels = c("Sí", "No")),
         Presión_alta = factor(Presión_alta, levels = c(1,2,3,4),
                               labels = c("Si, una sola vez", 
                                          "Si, mas de una vez", 
                                          "No, nunca", 
                                          "No recuerdo")),
         Diabetes = factor(Diabetes, levels = c(1,2),
                           labels = c("Sí", "No")),
         Fonasa = ifelse(Previsión %in% c(1,2,3,4,5),1,0),
         Previsión = factor(Previsión, levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("Fonasa A",
                                       "Fonasa B", 
                                       "Fonasa C", 
                                       "Fonasa D", 
                                       "Fonasa desconocido", 
                                       "FFAA", 
                                       "Isapre", 
                                       "Ninguno", 
                                       "Otro")),
         Nivel_educacional = case_when(Nivel_educacional == 1~0,
                                       Nivel_educacional <8~1,
                                       Nivel_educacional <10~2,
                                       Nivel_educacional <13~3,
                                       Nivel_educacional >=13~4),
         Nivel_educacional = factor(Nivel_educacional, levels = c(0,1,2,3,4),
                                    labels = c("Nunca asistió", 
                                               "Ed.Basica o menos", 
                                               "Ed.Media", 
                                               "Tecnico", 
                                               "Universitaro")),
         Trabajo = factor(Trabajo, levels = c(1,2),
                          labels = c("Sí", "No")),
         TallaM = Talla/100,
         IMC = Peso/TallaM**2,
         Asma = factor(Asma, levels = c(1,2),
                       labels = c("Sí", "No")),
         Colesterol_alto = ifelse(Colesterol_Total >= 200, "Sí","No"),
         Colesterol_alto = factor(Colesterol_alto, levels = c(1,2),
                                  labels = c("Sí", "No")),
         Sospecha_Depresion = factor(Sospecha_Depresion, levels = c(0,1),
                                     labels = c("No", "Sí")) %>% 
           ifelse(Sospecha_Depresion == 1, "Sí", "No"))  %>% 
  rename("Categoría de Edad" = Edad_Cat,
         "Percepción Salud" = `Que_diría_de_su_salud?`,
         "Lentes" = `Usa_lentes?`,
         "Precepción de Depresión" = `Se_ha_sentido_deprimido?`,
         "N de Cigarrillos" = `n°_cigarrillos`,
         "Presión Alta" = Presión_alta,
         "Nivel educacional" = Nivel_educacional,
         "Presión Arterial Sistólica" = presión_PAS,
         "Presión Arterial Diastólica" = presión_PAD,
         "Colesterol Total" = Colesterol_Total,
         "Colesterol Alto" = Colesterol_alto)




# Variable hiperetnsión ---------------------------------------------------
data$Hipertensión = ifelse(data$presión_PAD > 80 | 
                             data$presión_PAS > 140,1,0)

# Análisis Sociodemográfico -----------------------------------------------

data %>% 
  select(Region, Sexo, Percepción) %>% 
  lapply(count)

# Presión Arterial --------------------------------------------------------

## Sólo edad --------------------------------------------------------------


## Edad y Sexo ------------------------------------------------------------


# Prevalencia de Hipertensión ---------------------------------------------

## Grupos sociodemográficos -----------------------------------------------

## Factores de Riesgo









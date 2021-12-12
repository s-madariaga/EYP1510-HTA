# 
# # Añadir intervalos de confianza
# 
#  ###########################
#  # Segunda entrega EYP1510 #
#  ###########################
# 

icp = function(x, prop){
  require(dplyr)
  x = x %>% na.omit %>% as.numeric
  p = prop
  z = 1.96
  s = p*(1-p)
  s = s**2
  n = length(x)
  ic = round((p + c(-z, z)*s/n**.5)*100,2)
  return(
    print(paste0("[IC95%",ic[1],"%-",ic[2],"%]"))
  )
}

ic = function(x, rounded = 3){
  require(dplyr)
  x = x %>% na.omit %>% as.numeric
  mu = mean(x)
  z = 1.96
  s = sd(x)
  n = length(x)
  ic = round((mu + c(-z, z)*s/n**.5), rounded)
  return(
    paste0("[IC95% ",ic[1],"-",ic[2],"]")
  )
}





# Opciones ----------------------------------------------------------------
options(scipen = 999)
 rm(list = ls())

# Paquetes ----------------------------------------------------------------

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Uso
packages <- c("tidyverse", "vtable", "tableone", "gtsummary",
              "patchwork", "texreg", "finalfit")
ipak(packages)
 

# Importación -------------------------------------------------------------
data = rio::import("Base Salud 2017.xlsx")
data[data == -8888] = NA
data[data == -9999] = NA
data[data == -5555] = NA
data[data$Diabetes == 3,]$Diabetes = NA
data[data$Presión_alta == 4,]$Presión_alta = NA


# Selección ---------------------------------------------------------------
data = data  %>% 
  mutate(Zona = case_when(Region < 5~1,
                          Region <7~3,
                          Region <8~2,
                          Region <11~3,
                          Region >=11~4),
         Zona = factor(Zona, levels = c(1,2,3,4),
                       labels = c("Norte", "Metropolitana",
                                  "Centro-Sur", "Sur")),
         Metropolitana = ifelse(Zona == "Metropolitana", 1,0),
         Metropolitana = factor(Zona, levels = c(0,1)),
         Edad_cat = case_when(Edad<25~1,
                              Edad<45~2,
                              Edad<65~3,
                              Edad>=65~4),
         Edad_cat = factor(Edad_cat, levels = c(1,2,3,4),
                           labels = c("15 a 24 años",
                                      "25 a 44 años",
                                      "45 a 64 años",
                                      "65 años o más")),
         Hombre = factor(2-Sexo),
         Depresión = factor(Depresión-1, levels = c(1,0),
                            labels = c("No", "Sí")),
         Trabajo = factor(Trabajo-1, levels = c(1,0),
                            labels = c("No", "Sí")),
         Talla = Talla/100,
         IMC = Peso/Talla**2,
         IMC_Cat = case_when(IMC<18.5~1,
                             IMC<25~2,
                             IMC<30~3,
                             IMC>=30~4),
         IMC_Cat = factor(IMC_Cat, levels = c(1,2,3,4),
                          labels = c("Peso insuficiente",
                                     "Normopeso",
                                     "Sobrepeso",
                                     "Obesidad")),
         Hipercolesterolemia = ifelse(Colesterol_Total > 200,1,0),
         Hipercolesterolemia = factor(Hipercolesterolemia, levels =c(0,1),
                                   labels = c("No", "Sí")),
         Fuma = ifelse(Fuma == 1 | Fuma == 2, 1,0),
         Fuma  = factor(Fuma, levels = c(0,1),
                        labels = c("No", "Sí")),
         Presión_alta = factor(Presión_alta, levels = c(1,2,3),
                               labels =c("Una vez",
                                         "Más de una vez",
                                         "Nunca")),
        Sexo = factor(Sexo, levels = c(1,2),
                      labels = c("Hombre", "Mujer")),
        N = 1,
        HTA = factor(ifelse(presión_PAD >80 | presión_PAS >140,1,0),
                     levels = c(0,1), labels = c("No", "Sí")),
        Diabetes = factor(2-Diabetes, levels = c(0,1),
                            labels = c("No", "Sí"))) %>% 
  
  select(N,
         HTA,
         Sexo,
         Edad_cat, 
         IMC,
         Hipercolesterolemia,
         Hombre, 
         Edad, 
         IMC_Cat,
         Zona,
         Trabajo,
         presión_PAS,
         presión_PAD,
         Colesterol_Total,
         Diabetes,
         Fuma,
         Depresión, 
         Presión_alta) %>% 
  rename("PA alta" = `Presión_alta`,
         PAS = presión_PAS,
         PAD = presión_PAD,
         "Categoría de edad" = Edad_cat,
         "Categoría de IMC" = IMC_Cat,
         "Colesterol total" = Colesterol_Total)

# Descriptivas ------------------------------------------------------------
data %>% head
### TABLA 1 ---------------------------------------------------------------
tabla = tableone::CreateTableOne(vars = names(data), data = data);tabla
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "latex")


### INTERVALOS DE CONFIANZA -----------------------------------------------

# HIPERTENSIÓN
icp(x = as.numeric(data$HTA), prop = 0.37)

# SISTÓLICA A DIASTÓLICA
ic(data$PAS, rounded = 3)
ic(data$PAD, rounded = 3)


### PRESIÓN ARTERIAL ------------------------------------------------------
data[c("PAS","PAD","Sexo")] %>%
  tbl_summary(by = Sexo, 
              percent = "row",
              missing = "no") %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  cat

#### ** TRAMOS DE EDAD -------------------------------------------------------
data[c("PAS","PAD","Categoría de edad")] %>%
  tbl_summary(by = `Categoría de edad`, 
              percent = "row",
              missing = "no") %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  cat

#### ** ZONA -----------------------------------------------------------------
data[c("IMC","PAS","PAD","Zona")] %>%
  tibble %>% 
  tbl_summary(by = Zona, 
              percent = "row",
              missing = "no") %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  cat

data %>%
  tbl_summary(percent = "col")



# PREVALENCIA -------------------------------------------------------------

## SOCIODEMOGRÁFICOS -------------------------------------------------------
data[,-c(1,5,6,7,8,9,12,13,14,15,16,17,18)] %>% 
  select(HTA, Sexo, `Categoría de edad`, Zona, Trabajo) %>% 
  tbl_summary(by = HTA, percent = "row") %>%
  add_p() %>% 
  as_gt() %>%
  gt::as_latex() %>% 
  cat


## FACTORES DE RIESGO ------------------------------------------------------
data %>%
  select(HTA,`Categoría de edad`, `Categoría de IMC`, 
         Diabetes, Fuma, Hipercolesterolemia, Depresión) %>% 
  tbl_summary(by = HTA, percent = "row",
              missing = "no") %>% 
  add_p %>% 
  as_gt() %>%
  gt::as_latex() %>% 
  cat

data[c()] %>% names

oddsratio(xtabs(~`Categoría de IMC`+HTA, data = data))

oddsratio(xtabs(~`Categoría de edad`+HTA, data = data))


# IDEA DE REGRESIÓN LINEAL ------------------------------------------------

screenreg(l = list(lm(PAD~Edad, data = data),
                   lm(PAD~Edad+Hombre, data = data),
                   lm(PAD~Edad+Hombre+`Colesterol total`, data = data)))

# -------------------------------------------------------------------------

# ODDS

tabla1 = oddsratio.wald(xtabs(~`Categoría de edad`+HTA, data = data))$measure
tabla2 = oddsratio.wald(xtabs(~`Categoría de IMC`+HTA, data = data))$measure
tabla3a = oddsratio.wald(xtabs(~Sexo+HTA, data = data))$measure
tabla3b = oddsratio.wald(xtabs(~Hombre+HTA, data = data))$measure
tabla4 = oddsratio.wald(xtabs(~Diabetes+HTA, data = data))$measure
tabla5 = oddsratio.wald(xtabs(~Fuma+HTA, data = data))$measure
tabla6 = oddsratio.wald(xtabs(~Hipercolesterolemia+HTA, data = data))$measure
tabla7 = oddsratio.wald(xtabs(~Depresión+HTA, data = data))$measure
tabla = rbind(tabla1, tabla2,
              tabla3a, tabla3b,
              tabla4, tabla5,
              tabla6, tabla7) %>% round(2) ;tabla
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "latex")


# valor-p
tabla1 = oddsratio.wald(xtabs(~`Categoría de edad`+HTA, data = data))$p.value
tabla2 = oddsratio.wald(xtabs(~`Categoría de IMC`+HTA, data = data))$p.value
tabla3a = oddsratio.wald(xtabs(~Sexo+HTA, data = data))$p.value
tabla3b = oddsratio.wald(xtabs(~Hombre+HTA, data = data))$p.value
tabla4 = oddsratio.wald(xtabs(~Diabetes+HTA, data = data))$p.value
tabla5 = oddsratio.wald(xtabs(~Fuma+HTA, data = data))$p.value
tabla6 = oddsratio.wald(xtabs(~Hipercolesterolemia+HTA, data = data))$p.value
tabla7 = oddsratio.wald(xtabs(~Depresión+HTA, data = data))$p.value
tabla = rbind(tabla1, tabla2,
              tabla3a, tabla3b,
              tabla4, tabla5,
              tabla6, tabla7) %>% round(2) ;tabla
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "simple")


### LOGÍSTICA ---------------------------------------------------------------
# (no incluir)
m1 = glm(HTA~Edad, data = data,  family = binomial)
m2 = glm(HTA~Edad+Fuma+Trabajo, data = data,  family = binomial)
m3 = glm(HTA~IMC+Diabetes++Hipercolesterolemia, data = data,  family = binomial)
m4 = glm(HTA~Edad+Fuma+Trabajo+IMC+Diabetes+Hipercolesterolemia, data = data,  family = binomial)
screenreg(l = list(m1, m2, m3, m4))
texreg(list(m1,m2,m3,m4), dcolumn = TRUE, booktabs = TRUE,
use.packages = FALSE, label = "tab:3", caption = "Two linear models.",
float.pos = "h")



### FIGURA 1 --------------------------------------------------------------
figura11 = data %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAS, group = Sexo, 
             color = Sexo)) +
  geom_hline(yintercept = 140, color = "grey70") +
  geom_point(stat = "summary", fun = mean, pch = 15, size = 2) +
  geom_line(stat = "summary", fun = mean, pch = 15, size = 1) + xlab("")+
  stat_summary(fun = mean,
               geom = "errorbar",
               fun.max = function(x) mean(x) + 1.96*sd(x)/length(x)**.5,
               fun.min = function(x) mean(x) - 1.96*sd(x)/length(x)**.5,
               width = .4, color = 1) +
  theme_test() + 
  geom_text(stat = 'summary', fun = mean, aes(label = round(..y.., 2)), 
            nudge_x = 0.5, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) +
  theme(legend.position = "none")


figura12 =  data %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAD, group = Sexo, 
             color = Sexo)) +
  geom_hline(yintercept = 80, color = "grey70") +
  geom_point(stat = "summary", fun = mean, pch = 15, size = 2) +
  geom_line(stat = "summary", fun = mean, pch = 15, size = 1) + xlab("")+
  stat_summary(fun = mean,
               geom = "errorbar",
               fun.max = function(x) mean(x) + 1.96*sd(x)/length(x)**.5,
               fun.min = function(x) mean(x) - 1.96*sd(x)/length(x)**.5,
               width = .4, color = 1) +
  theme_test() +
   geom_text(stat = 'summary', fun = mean, aes(label = round(..y.., 2)), 
            nudge_x = 0.4, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) +
  theme(legend.position = "bottom")

figura11 / figura12

data %>% names %>% cat(sep = "\n")



# HTA
# Sexo
# Categoría de edad
# IMC
# Hipercolesterolemia
# Hombre
# Edad
# Categoría de IMC
# Zona
# Trabajo
# PAS
# PAD
# Colesterol total
# Diabetes
# Fuma
# Depresión
# PA alta
# Edad>40
# Edad de riesgo
# Mayor



data %>%
  tbl_summary

data %>%
  tbl_summary(percent = "row", by = HTA) %>% 
  add_p()


data %>%
  tbl_cross(
    row = Diabetes,
    col = HTA,
    percent = "row",
    missing = "no"
  ) %>%
  add_p()


  
stargazer(data, type = "text",
          omit.summary.stat = c("p25", "p75","min", "max"))

?stargazer
## Edad y Sexo ------------------------------------------------------------


# Prevalencia de Hipertensión ---------------------------------------------

## Grupos sociodemográficos -----------------------------------------------

## Factores de Riesgo

dependent = "HTA"
explanatory = c("`Categoría de edad`", "`Categoría de IMC`", 
                "Diabetes", "Fuma", "Hipercolesterolemia", "Depresión")
Exp = c("Categoría de edad", "Categoría de IMC", 
                "Diabetes", "Fuma", "Hipercolesterolemia", "Depresión")
data[c(Exp, dependent)] %>%
  tbl_summary(percent = "row", by = HTA) %>% 
  add_p()
data %>%
   or_plot(dependent, explanatory)


data[exploratory] %>% 
  head




# -------------------------------------------------------------------------









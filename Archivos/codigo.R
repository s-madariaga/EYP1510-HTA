# 
# 
#  ###########################.
#  # Segunda entrega EYP1510 #
#  ###########################.
# 

# Función -----------------------------------------------------------------
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


# Preparación de las variables --------------------------------------------
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



# ESTADÍSTICOS DESCRIPTIVOS -----------------------------------------------

## Tabla 1: Estadísticos Descriptivos -------------------------------------

# Tabla
set = data[,- c(3,4,5,19)]
tabla = tableone::CreateTableOne(vars = names(set), 
                                 data = set)
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "simple")

# Latex
knitr::kable(p, booktabs = TRUE, format = "latex")

# Tabla alternativa
set %>% 
  tbl_summary(missing = "no")



## Presión Arterial -------------------------------------------------------


### Tabla 2: PA, comparación de sexo --------------------------------------

# Tabla
data[c("PAS","PAD","Sexo")] %>%
  tbl_summary(by = Sexo, 
              percent = "row",
              missing = "no")

# Latex
data[c("PAS","PAD","Sexo")] %>%
  tbl_summary(by = Sexo, 
              percent = "row",
              missing = "no") %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  cat


### Figura 1. PA por edad y sexo ------------------------------------------
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
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))

# Vertical
figura11 / figura12

# Horizontal
figura11 + figura12


### Tablas alternativas (no mostrar) --------------------------------------


#### Tabla 2b: PA, comparación por edad -----------------------------------

# Tabla
data[c("IMC", "PAS","PAD","Categoría de edad")] %>%
  tbl_summary(by = `Categoría de edad`, 
              percent = "row",
              missing = "no")

# Latex
data[c("IMC", "PAS","PAD","Categoría de edad")] %>%
  tbl_summary(by = `Categoría de edad`, 
              percent = "row",
              missing = "no") %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  cat



#### Tabla 2c: PA, comparación por zona -----------------------------------

# Tabla
data[c("IMC","PAS","PAD","Zona")] %>%
  tibble %>% 
  tbl_summary(by = Zona, 
              percent = "row",
              missing = "no")

# Latex
data[c("IMC","PAS","PAD","Zona")] %>%
  tibble %>% 
  tbl_summary(by = Zona, 
              percent = "row",
              missing = "no") %>%
  as_gt() %>% 
  gt::as_latex() %>% 
  cat


### Visualizaciones alternativas (no mostrar) -----------------------------

# Histograma de edad
data %>% 
  ggplot(aes(x = Edad)) +
  geom_histogram(fill = h)+
  theme_classic()


#### Boxplots de presión arterial, muestra --------------------------------

# Hombre = 1
data %>% 
  select(`Categoría de edad`, Hombre,PAD, PAS) %>% 
  na.omit %>% 
  tidyr::gather(value = "valor", key = "variable", -c(Hombre, `Categoría de edad`)) %>% 
  ggplot(aes(x = `Categoría de edad`, y = valor, fill = Hombre, color = Hombre)) +
  geom_hline(yintercept = 80, color = "blue") +
  geom_hline(yintercept = 140, color = "red") +
  geom_boxplot() +
  facet_wrap(~variable, ncol=2) +
  scale_fill_manual(values = c("grey", h)) +
  scale_color_manual(values = c("grey50",1))

# Diabetes
data %>% 
  select(`Categoría de edad`, Diabetes,PAD, PAS) %>% 
  na.omit %>% 
  tidyr::gather(value = "valor", key = "variable", -c(Diabetes, `Categoría de edad`)) %>% 
  ggplot(aes(x = `Categoría de edad`, y = valor, fill = Diabetes, color = Diabetes)) +
  geom_hline(yintercept = 80, color = "blue") +
  geom_hline(yintercept = 140, color = "red") +
  geom_boxplot() +
  facet_wrap(~variable, ncol=2) +
  scale_fill_manual(values = c("grey", h)) +
  scale_color_manual(values = c("grey50",1))

# Colesterol Alto
data %>% 
  select(`Categoría de edad`, Hipercolesterolemia,PAD, PAS) %>% 
  na.omit %>% 
  tidyr::gather(value = "valor", key = "variable", -c(Hipercolesterolemia, `Categoría de edad`)) %>% 
  ggplot(aes(x = `Categoría de edad`, y = valor, fill = Hipercolesterolemia, color = Hipercolesterolemia)) +
  geom_hline(yintercept = 80, color = "blue") +
  geom_hline(yintercept = 140, color = "red") +
  geom_boxplot() +
  facet_wrap(~variable, ncol=2) +
  scale_fill_manual(values = c("grey", h)) +
  scale_color_manual(values = c("grey50",1))

# Fuma
data %>% 
  select(`Categoría de edad`, Fuma,PAD, PAS) %>% 
  na.omit %>% 
  tidyr::gather(value = "valor", key = "variable", -c(Fuma, `Categoría de edad`)) %>% 
  ggplot(aes(x = `Categoría de edad`, y = valor, fill = Fuma, color = Fuma)) +
  geom_hline(yintercept = 80, color = "blue") +
  geom_hline(yintercept = 140, color = "red") +
  geom_boxplot() +
  facet_wrap(~variable, ncol=2) +
  scale_fill_manual(values = c("grey", h)) +
  scale_color_manual(values = c("grey50",1))


### Comparación de medias por FR ------------------------------------------

# Diabetes
g1 = data %>% 
  select(`Categoría de edad`, PAS, Diabetes) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAS, group = Diabetes, 
             color = Diabetes)) +
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
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))+
  ggtitle("Diabetes") + theme(legend.position = "none")
g2 = data %>% 
  select(`Categoría de edad`, PAD, Diabetes) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAD, group = Diabetes, 
             color = Diabetes)) +
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
            nudge_x = 0.5, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))
g1+g2

# Fumar
g1 = data %>% 
  select(`Categoría de edad`, PAS, Fuma) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAS, group = Fuma, 
             color = Fuma)) +
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
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))+
  ggtitle("Fuma") + theme(legend.position = "none")
g2 = data %>% 
  select(`Categoría de edad`, PAD, Fuma) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAD, group = Fuma, 
             color = Fuma)) +
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
            nudge_x = 0.5, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))
g1+g2

# IMC
data$imc25 = ifelse(data$IMC >25,1,0) %>% 
  factor
g1 = data %>% 
  select(`Categoría de edad`, PAS, imc25) %>%  
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAS, group = imc25, 
             color = imc25)) +
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
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))+
  ggtitle("IMC > 25") + theme(legend.position = "none")
g2 = data %>% 
  select(`Categoría de edad`, PAD, imc25) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAD, group = imc25, 
             color = imc25)) +
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
            nudge_x = 0.5, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))
g1+g2


# Colesterol alto
g1 = data %>% 
  select(`Categoría de edad`, PAS, Hipercolesterolemia) %>%  
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAS, group = Hipercolesterolemia, 
             color = Hipercolesterolemia)) +
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
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))+
  ggtitle("Colesterol Alto") + theme(legend.position = "none")
g2 = data %>% 
  select(`Categoría de edad`, PAD, Hipercolesterolemia) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAD, group = Hipercolesterolemia, 
             color = Hipercolesterolemia)) +
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
            nudge_x = 0.5, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))
g1+g2


# Depresión
g1 = data %>% 
  select(`Categoría de edad`, PAS, Depresión) %>%  
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAS, group = Depresión, 
             color = Depresión)) +
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
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))+
  ggtitle("Depresión") + theme(legend.position = "none")
g2 = data %>% 
  select(`Categoría de edad`, PAD, Depresión) %>% 
  na.omit %>% 
  ggplot(aes(x = `Categoría de edad`, y = PAD, group = Depresión, 
             color = Depresión)) +
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
            nudge_x = 0.5, show_guide  = FALSE, size = 3, color = "grey38") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E"))
g1+g2


# PARTE I. PREVALENCIA ----------------------------------------------------

## Tabla 3. Prevalencia HTA por factores sociodemográficos ----------------

# Tabla
data %>% 
  tbl_summary(by = HTA, percent = "row", missing = "no") %>%
  add_p()

# Latex
data %>% 
  tbl_summary(by = HTA, percent = "row", missing = "no") %>%
  add_p() %>% 
  as_gt() %>%
  gt::as_latex() %>% 
  cat

## Tabla 4. Prevalencia HTA por FR ----------------------------------------

# Tabla
data %>%
  select(HTA, `Categoría de IMC`, 
         Diabetes, Fuma, Hipercolesterolemia, Depresión) %>% 
  tbl_summary(by = HTA, percent = "row",
              missing = "no") %>% 
  add_p

# Latex
data %>%
  select(HTA, `Categoría de IMC`, 
         Diabetes, Fuma, Hipercolesterolemia, Depresión) %>% 
  tbl_summary(by = HTA, percent = "row",
              missing = "no") %>% 
  add_p %>% 
  as_gt() %>%
  gt::as_latex() %>% 
  cat


# PARTE II. FACTORES DE RIESGO (predicción) -------------------------------

## Tabla 5. Odds Ratio FR -------------------------------------------------

# odds, Tabla
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
              tabla6, tabla7) %>% round(2)
tabla

# Latex
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "latex")


# valor-p, Tabla
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
              tabla6, tabla7) %>% round(2)
tabla

# Latex
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "simple")


## Odds plot

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



# PREDICCIÓN ANEXO --------------------------------------------------------
## Regresión Lineal -------------------------------------------------------
stargazer::stargazer(lm(PAD~Edad, data = data), lm(PAD~Edad+Hombre, data = data),
                   lm(PAD~Edad+Hombre+`Colesterol total`, data = data),
          type = "text")

# Latex
p <- print(tabla, printToggle = FALSE, noSpaces = TRUE)
knitr::kable(p, booktabs = TRUE, format = "latex")

## Regresión Logística ----------------------------------------------------

# Tabla
m1 = glm(HTA~Edad, data = data,  family = binomial)
m2 = glm(HTA~Edad+Fuma+Trabajo, data = data,  family = binomial)
m3 = glm(HTA~IMC+Diabetes++Hipercolesterolemia, data = data,  family = binomial)
m4 = glm(HTA~Edad+Fuma+Trabajo+IMC+Diabetes+Hipercolesterolemia, data = data,  family = binomial)
screenreg(l = list(m1, m2, m3, m4))

# Latex
texreg(list(m1,m2,m3,m4), dcolumn = TRUE, booktabs = TRUE,
use.packages = FALSE, label = "tab:3", caption = "Modelos Lineales",
float.pos = "h")

# html version
htmlreg(list(m1,m2,m3,m4), dcolumn = TRUE, booktabs = TRUE,
use.packages = FALSE, label = "tab:3", caption = "Modelos Lineales",
float.pos = "h")





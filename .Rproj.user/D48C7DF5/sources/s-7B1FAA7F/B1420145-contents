
# En este código, hemos utilizado la función ipak divulgado por el doctor en 
# psicología Pablo Vallejo Medina (2020), cuya autoría se remonta al foro
# stack overflow.

# Función
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Uso
packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
ipak(packages)


# Fuente Secundaria -------------------------------------------------------

# Video de Pablo Vallejo Medina (2020). Consultado el 8 de diciembre del 2021
# Recuperado de: https://www.youtube.com/watch?v=UjQz9SxG9rk&t=18s


# Fuente Original ---------------------------------------------------------

# once, L., Rinker, T., Rinker, T., & O&#39;Brien, J. (2011). [StackOverflow] 
# Load multiple packages at once. Consultado el 8 de diciembre del 2021, 
# recuperado: https://stackoverflow.com/questions/8175912/load-multiple-packages-at-once

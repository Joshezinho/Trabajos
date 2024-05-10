### Práctico 3       ###
### Joshe Norambuena ###
########################

#### 0. Setear directorio de trabajo ####
setwd("C:/Users/Asus/Documents/GitHub/Trabalho")


#### 1. Instalación de paquetes ####
install.packages("pacman")

# 1.1. Cargar paquetes
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#### 2. Cargar base de datos ####
# 2.1 Pasos previos 
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

# 2.2 Cargar la base de datos desde internet
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))
load("input/latinobarometro2020.rdata")

#### 3. Selección de variables ####
find_var(data = latinobarometro2020,"Confianza")

# 3.1 Seleccionar variable de interés y crear nueva base de datos
proc_data <- latinobarometro2020 %>% select(p13st_e, # Confianza en el Gobierno
                                            p13st_d, # Confianza en el congreso
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g, # Confianza en los partidos políticos
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais 

# Comprobar
names(proc_data)

# 3.2 Atributo de las variables
sjlabelled::get_label(proc_data)

# 3.3 Filtrar por casos de Chile
proc_data <- proc_data %>% dplyr::filter(idenpa==152)

#### 4. Procesamiento de variables ####
## 4.1 Confianza en el gobierno
# 4.1.2 Descriptivos de la variable 
frq(proc_data$p13st_e)

# 4.1.3 Recodificación de casos perdidos a NA
proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")

# 4.1.4 Recodificación de toda la base 
proc_data <- proc_data %>% set_na(., na = c(-2, -1))

# 4.1.5 Reordenar categorías 
proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")

# 4.1.6 Renombrar variables
proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g) # Confianza en los partidos políticos 

# 4.1.7 Etiquetar variables
#Variable Confianza en el Gobierno
proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

#Variable Confianza en el Congreso
proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)

#Variable Confianza en el Poder Judicial
proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)

#Variable Confianza en los Partidos Políticos
proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

# 4.1.8 Crear variable que suma las cuatro de confianza
proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol)
summary(proc_data$conf_inst)
# Visualizar categoría
get_label(proc_data$conf_inst)
# Cambiar categoría a variable nueva
proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "Confianza en instituciones")
get_label(proc_data$conf_inst)

# 4.1.9 Revisión Final
## Descriptivo para asegurar recodificación
# Confianza en el Gobierno
frq(proc_data$conf_gob)

# Confianza en el Congreso
frq(proc_data$conf_cong)

# Confianza en el Poder Judicial
frq(proc_data$conf_jud)

# Confianza en los Partidos Políticos
frq(proc_data$conf_partpol)

# Confianza en las instituciones
frq(proc_data$conf_inst)

# 4.1.10 Asignar etiquetas a los valores de las variables
proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_cong <- set_labels(proc_data$conf_cong,
                                  labels=c( "Ninguna"=0,
                                            "Poca"=1,
                                            "Algo"=2,
                                            "Mucha"=3))

proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                     labels=c( "Ninguna"=0,
                                               "Poca"=1,
                                               "Algo"=2,
                                               "Mucha"=3))
#4.1.10.2 Chequear las categorías de respuesta
# Confianza en el Gobierno
frq(proc_data$conf_gob)

# Confianza en el Congreso
frq(proc_data$conf_cong)

# Confianza en el Poder Judicial
frq(proc_data$conf_jud)

# Confianza en los Partidos Políticos
frq(proc_data$conf_partpol)

## 4.2. Educación
# 4.2.1 Descriptivos de la variable
frq(proc_data$reeduc_1)

# 4.2.2 Recodificación de la variable
proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1,2,3)=1; c(4,5)=2; c(6,7)=3")

# 4.2.3 Revisión de descriptivos
frq(proc_data$reeduc_1)

# 4.2.4 Etiquetado
proc_data$reeduc_1 <- factor(proc_data$reeduc_1,
                             labels = c("Educacion basica", "Educacion media", "Educacion superior"),
                             levels = c(1, 2, 3))

# 4.2.5 Renombrar variable
proc_data <- rename(proc_data,"educacion"=reeduc_1)

# 4.2.6 Etiquetar variable
# 4.2.6.1 Comprobar etiqueta existente
get_label(proc_data$educacion)

# 4.2.6.2 Asignar etiqueta a la variable 
proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")

## 4.3 Sexo
# 4.3.1 Descriptivos de la variable
frq(proc_data$sexo)

# 4.3.2 Recodificación de la variable
proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

# 4.3.3 Etiquetado 
proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))

# 4.3.4 Etiquetar variable
# 4.3.4.1 Comprobar etiqueta existente
get_label(proc_data$sexo)

# 4.3.4.2 Asignar etiqueta a la variable 
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")

## 4.4 Edad
# 4.4.1 Descriptivos
frq(proc_data$edad)

# 4.4.2 Etiquetar variable
# 4.4.2.1 Comprobar etiqueta existente
get_label(proc_data$edad)

# 4.4.2.2 Asignar etiqueta a la variable 
proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

#### 5. Generar base de datos procesada para el análisis ####
## 5.1 Reformatear objeto (proc_data) a BBDD
proc_data <-as.data.frame(proc_data)

## 5.2 Generar tabla descriptiva general
stargazer(proc_data, type="text")

## 5.3 Guardar base de datos en una ruta particular
save(proc_data, file ="input/proc_data.rdata")

#### Descriptivos básicos de las variables ####
## Medias por grupos 
# Media por sexo de confianza en las instituciones
proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(conf_inst, na.rm=TRUE))

# Media por nivel educativo de confianza en las instituciones 
proc_data %>% dplyr::group_by(educacion) %>% summarise(mean(conf_inst, na.rm=TRUE))

# Representación de las medias
# Instalar y llamar al paquete sjPlot
install.packages("sjPlot")
library(sjPlot)

# Generar tabla de representación 
sjt.xtab(proc_data$educacion, proc_data$conf_inst, encoding = "UTF-8")

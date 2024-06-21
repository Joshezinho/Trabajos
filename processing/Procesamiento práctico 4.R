## Procesamiento de datos ##
##    José Norambuena     ##
##      Práctico 4        ##
############################

#### 1. Instalación de paquetes ####
install.packages("pacman")

# 1.1. Cargar paquetes
pacman::p_load(sjlabelled, dplyr, car, stargazer, sjmisc, summarytools, kableExtra, sjPlot, corrplot, sessioninfo, ggplot2, fastDummies, ggeffects, texreg)

# 1.2 Pasos previos 
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

#### 2. Cargar base de datos ####
load(url("https://github.com/Joshezinho/Trabajos/raw/main/input/elsoc_reg.rdata")) #Cargar base de datos

#### 3. Recodificar variables para la regresión ####
names(elsoc_reg)

## 3.1 Etiquetar variables
# Edad
elsoc_reg$edad <- set_label(x = elsoc_reg$edad,label = "Edad")
get_label(elsoc_reg$edad)
# Sexo
elsoc_reg$sexo <- set_label(x = elsoc_reg$sexo,label = "Sexo")
get_label(elsoc_reg$sexo)
# Nivel educativo
elsoc_reg$educ <- set_label(x = elsoc_reg$educ,label = "Nivel educativo")
get_label(elsoc_reg$educ)
# Sentimiento de compromiso con el movimiento
elsoc_reg$comp <- set_label(x = elsoc_reg$comp, label = "Sentimiento de compromiso con el movimiento")
get_label(elsoc_reg$comp)
# Identificación con el movimiento
elsoc_reg$ident <- set_label(x = elsoc_reg$ident, label = "Identificación con el movimiento")
get_label(elsoc_reg$ident)
# El	futuro	del	movimiento	genera esperanza
elsoc_reg$esperanza <- set_label(x = elsoc_reg$esperanza, label = "El futuro del movimiento genera esperanza")
get_label(elsoc_reg$esperanza)
# Las	acciones	y	protestas	de	este	movimiento pueden	generar	un	cambio	social
elsoc_reg$cambio_social <- set_label(x = elsoc_reg$cambio_social, label = "Las acciones del movimiento pueden generar un cambio social")
get_label(elsoc_reg$cambio_social)
# El	 propósito	 del	 movimiento	 está	alineado	con	los valores de los participantes
elsoc_reg$valores <- set_label(x = elsoc_reg$valores, label = "El propósito del movimiento está alineado con los valores de los participantes")
get_label(elsoc_reg$valores)

## 3.2 Apoyo al movimiento estudiantil
elsoc_reg$apoyo <- (elsoc_reg$comp+elsoc_reg$ident+elsoc_reg$esperanza+elsoc_reg$cambio_social+elsoc_reg$valores)
summary(elsoc_reg$apoyo)

# Etiquetado
elsoc_reg$apoyo <- set_label(x = elsoc_reg$apoyo, label = "Apoyo al movimiento social estudiantil")
get_label(elsoc_reg$apoyo)

## 3.3 Sexo
frq(elsoc_reg$sexo)
elsoc_reg$sexo <- recode(elsoc_reg$sexo, "1=1; 2=0") %>% as.numeric()

## 3.4 Nivel educativo
## Recodificación
elsoc_reg$educ <- car::recode(elsoc_reg$educ, "c(1,2)=0; c(3,4)=1; c(5,6,8)=2; c(7,9,10)=3")

# Etiquetado
elsoc_reg$educ <- factor(elsoc_reg$educ,
                     labels = c("Sin Estudios", "Básica", "Media", "Superior"),
                     levels = c(0, 1, 2, 3))


#### 4. Seleccionar sólo variables a emplear
elsoc_reg <- select(elsoc_reg, edad, sexo, educ, apoyo)

#### 5. Regresión ####
fit01<- lm(apoyo~sexo,data=elsoc_reg)
fit02<- lm(apoyo~sexo+edad,data=elsoc_reg)
fit03<- lm(apoyo~sexo+edad+educ,data=elsoc_reg)

labs01 <- c("Intercepto","Sexo (mujer)","Edad", "Educación Básica (Sin estudios)", "Educación Media", "Educación Superior")

#screenreg para que se vea en R
screenreg(list(fit01,fit02,fit03),custom.coef.names = labs01)

# htmlreg para que se vea en el sitio web
knitreg(list(fit01,fit02,fit03),
        custom.model.names = c("Modelo 1","Modelo 2","Modelo 3"),
        custom.coef.names = c("Intercepto",
                              "Sexo (mujer)",
                              "Edad",
                              "Educación Básica",
                              "Educación Media",
                              "Educación Superior"))

#### 5. Generar base de datos procesada para el análisis ####
## 5.1 Reformatear objeto (elsoc y escala_elsoc) a BBDD
elsoc_reg <-as.data.frame(elsoc_reg)

## 5.2 Guardar base de datos en una ruta particular
save(elsoc_reg, file ="input/elsoc_reg.rdata")

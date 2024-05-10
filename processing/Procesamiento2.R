## Procesamiento de datos ##
##    José Norambuena     ##
##      Práctico 3        ##
############################

#### 1. Instalación de paquetes ####
install.packages("pacman")

# 1.1. Cargar paquetes
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, sjPlot, ggplot2, psych)

# 1.2 Pasos previos 
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

#### 2. Cargar base de datos ####
load(url("https://dataverse.harvard.edu/api/access/datafile/7245118")) #Cargar base de datos

# 2.1 Filtrar base de datos por personas que apoyan el movimiento estudiantil
elsoc <- elsoc_long_2016_2022.2 %>% dplyr::filter(c20==1)
remove(elsoc_long_2016_2022.2)

#### 3. Seleccionar variables ####
#m0_edad --> edad
#m0_sexo --> sexo
#m01 --> nivel educacional del entrevistado
#c21_01 --> Sentimiento de compromiso con el movimiento
#c21_02 --> Identificación con el movimiento
#c21_03 --> Estoy	 de	 acuerdo	 con	 las	 acciones	 de	 este	movimiento
#c21_04 --> Pensar	acerca	del	futuro	de	este	movimiento	me	hace	sentir	esperanzado
#c21_05 --> Las	acciones	y	protestas	de	este	movimiento pueden	generar	un	cambio	social
#c21_06 --> Los	participantes	de	este	movimiento	me	ven	como	un	miembro	más
#c21_07 --> El	 propósito	 de	 este	 movimiento	 está	alineado	con	mis	valores
elsoc_long_2016_2022.2$m01
# 3.1 Seleccionar variable de interés y crear nueva base de datos
elsoc <- elsoc %>% select(m0_edad, # Edad
                          m0_sexo, # Sexo
                          m01, # Nivel educacional
                          c21_01,# Sentimiento de compromiso con el movimiento
                          c21_02,# Identificación con el movimiento
                          c21_03,# Estoy	 de	 acuerdo	 con	 las	 acciones	 de	 este	movimiento
                          c21_04,# Pensar	acerca	del	futuro	de	este	movimiento	me	hace	sentir	esperanzado
                          c21_05,# Las	acciones	y	protestas	de	este	movimiento pueden	generar	un	cambio	social
                          c21_06,# Los	participantes	de	este	movimiento	me	ven	como	un	miembro	más
                          c21_07) # El	 propósito	 de	 este	 movimiento	 está	alineado	con	mis	valores

# Comprobar
names(elsoc)

# 3.2 Atributo de las variables
sjlabelled::get_label(elsoc)

#### 4. Procesamiento de variables ####
## 4.1 Renombrar y re-etiquetar variables ##
elsoc <- elsoc %>% rename("edad"=m0_edad, 
                          "sexo"=m0_sexo, 
                          "educ"=m01, 
                          "comp" =c21_01,
                          "ident" = c21_02, 
                          "acciones" = c21_03, 
                          "esperanza" = c21_04,
                          "cambio_social" = c21_05,
                          "pertenencia" = c21_06,
                          "valores" = c21_07)

# Comprobar cambio de nombre
names(elsoc)

## 4.1.1 Re-etiquetar variables 
# Edad
elsoc$edad <- set_label(x = elsoc$edad,label = "Edad")
get_label(elsoc$edad)
# Sexo
elsoc$sexo <- set_label(x = elsoc$sexo,label = "Sexo")
get_label(elsoc$sexo)
# Nivel educativo
elsoc$educ <- set_label(x = elsoc$educ,label = "Nivel educativo")
get_label(elsoc$educ)
# Sentimiento de compromiso con el movimiento
elsoc$comp <- set_label(x = elsoc$comp, label = "Sentimiento de compromiso con el movimiento")
get_label(elsoc$comp)
# Identificación con el movimiento
elsoc$ident <- set_label(x = elsoc$ident, label = "Identificación con el movimiento")
get_label(elsoc$ident)
# De	 acuerdo	 con	 las	 acciones	 del	movimiento
elsoc$acciones <- set_label(x = elsoc$acciones, label = "Estoy	 de	 acuerdo	 con	 las	 acciones	 de	 este	movimiento")
get_label(elsoc$acciones)
# El	futuro	del	movimiento	genera esperanza
elsoc$esperanza <- set_label(x = elsoc$esperanza, label = "El	futuro	del	movimiento	genera esperanza")
get_label(elsoc$esperanza)
# Las	acciones	y	protestas	de	este	movimiento pueden	generar	un	cambio	social
elsoc$cambio_social <- set_label(x = elsoc$cambio_social, label = "Las acciones del movimiento pueden generar un cambio social")
get_label(elsoc$cambio_social)
# Los	participantes	del	movimiento	generan sentido de pertenencia
elsoc$pertenencia <- set_label(x = elsoc$pertenencia, label = "Los participantes del movimiento generan sentido de pertenencia")
get_label(elsoc$pertenencia)
# El	 propósito	 del	 movimiento	 está	alineado	con	los valores de los participantes
elsoc$valores <- set_label(x = elsoc$valores, label = "El propósito del movimiento está alineado con los valores de los participantes")
get_label(elsoc$valores)

## 4.2 Recodificación de casos perdidos a NA 
# Edad
frq(elsoc$edad) #No hay que recodificar
# Sexo
frq(elsoc$sexo) #No hay que recodificar
# Nivel educativo
frq(elsoc$educ)
# Sentimiento de compromiso con el movimiento
frq(elsoc$comp)
elsoc$comp <- recode(elsoc$comp, "c(-999,-888,-777,-666)=NA")
# Identificación con el movimiento
frq(elsoc$ident)
elsoc$ident <- recode(elsoc$ident, "c(-999,-888,-777,-666)=NA")
# De	 acuerdo	 con	 las	 acciones	 del	movimiento
frq(elsoc$acciones)
elsoc$acciones <- recode(elsoc$acciones, "c(-999,-888,-777,-666)=NA")
# El	futuro	del	movimiento	genera esperanza
frq(elsoc$esperanza)
elsoc$esperanza <- recode(elsoc$esperanza, "c(-999,-888,-777,-666)=NA")
# Las	acciones	y	protestas	de	este	movimiento pueden	generar	un	cambio	social
frq(elsoc$cambio_social)
elsoc$cambio_social <- recode(elsoc$cambio_social, "c(-999,-888,-777,-666)=NA")
# Los	participantes	del	movimiento	generan sentido de pertenencia
frq(elsoc$pertenencia)
elsoc$pertenencia <- recode(elsoc$pertenencia, "c(-999,-888,-777,-666)=NA")
# El	 propósito	 del	 movimiento	 está	alineado	con	los valores de los participantes
frq(elsoc$valores)
elsoc$valores <- recode(elsoc$valores, "c(-999,-888,-777,-666)=NA")

## 4.3 Recodificación de casos perdidos para toda la base
elsoc <- elsoc %>% set_na(., na = c(-666, -777, -888, -999))

## 4.4 Reordenar categorías
elsoc$comp <- recode(elsoc$comp, "1=0; 2=1; 3=2; 4=3; 5=4")
elsoc$ident <- recode(elsoc$ident, "1=0; 2=1; 3=2; 4=3; 5=4")
elsoc$acciones <- recode(elsoc$acciones, "1=0; 2=1; 3=2; 4=3; 5=4")
elsoc$esperanza <- recode(elsoc$esperanza, "1=0; 2=1; 3=2; 4=3; 5=4")
elsoc$cambio_social <- recode(elsoc$cambio_social, "1=0; 2=1; 3=2; 4=3; 5=4")
elsoc$pertenencia <- recode(elsoc$pertenencia, "1=0; 2=1; 3=2; 4=3; 5=4")
elsoc$valores <- recode(elsoc$valores, "1=0; 2=1; 3=2; 4=3; 5=4")

## 4.5 Asignar etiquetas a los valores de las variables
elsoc$comp <- set_labels(elsoc$comp,
                         labels=c( "Totalmente en desacuerdo"=0,
                                   "En desacuerdo"=1,
                                   "Ni en desacuerdo ni de acuerdo"=2,
                                   "De acuerdo"=3,
                                   "Totalmente de acuerdo"=4))

elsoc$ident <- set_labels(elsoc$ident,
                          labels=c( "Totalmente en desacuerdo"=0,
                                    "En desacuerdo"=1,
                                    "Ni en desacuerdo ni de acuerdo"=2,
                                    "De acuerdo"=3,
                                    "Totalmente de acuerdo"=4))

elsoc$acciones <- set_labels(elsoc$acciones,
                             labels=c( "Totalmente en desacuerdo"=0,
                                       "En desacuerdo"=1,
                                       "Ni en desacuerdo ni de acuerdo"=2,
                                       "De acuerdo"=3,
                                       "Totalmente de acuerdo"=4))

elsoc$esperanza <- set_labels(elsoc$esperanza,
                              labels=c( "Totalmente en desacuerdo"=0,
                                        "En desacuerdo"=1,
                                        "Ni en desacuerdo ni de acuerdo"=2,
                                        "De acuerdo"=3,
                                        "Totalmente de acuerdo"=4))

elsoc$cambio_social <- set_labels(elsoc$cambio_social,
                                  labels=c( "Totalmente en desacuerdo"=0,
                                            "En desacuerdo"=1,
                                            "Ni en desacuerdo ni de acuerdo"=2,
                                            "De acuerdo"=3,
                                            "Totalmente de acuerdo"=4))

elsoc$pertenencia <- set_labels(elsoc$pertenencia,
                                labels=c( "Totalmente en desacuerdo"=0,
                                          "En desacuerdo"=1,
                                          "Ni en desacuerdo ni de acuerdo"=2,
                                          "De acuerdo"=3,
                                          "Totalmente de acuerdo"=4))

elsoc$valores <- set_labels(elsoc$valores,
                            labels=c( "Totalmente en desacuerdo"=0,
                                      "En desacuerdo"=1,
                                      "Ni en desacuerdo ni de acuerdo"=2,
                                      "De acuerdo"=3,
                                      "Totalmente de acuerdo"=4))

# 4.5.1 Sexo
# Recodificación
elsoc$sexo <- car::recode(elsoc$sexo, "1=1; 2=0")

# Etiquetado
elsoc$sexo <- factor(elsoc$sexo,
                     labels = c("Hombre", "Mujer"),
                     levels = c(1, 0))

# 4.5.2 
# Recodificación
elsoc$educ <- car::recode(elsoc$educ, "c(1,2)=0; c(3,4)=1; c(5,6,8)=2; c(7,9,10)=3")

# Etiquetado
elsoc$educ <- factor(elsoc$educ,
                     labels = c("Sin Estudios", "Básica", "Media", "Superior"),
                     levels = c(0, 1, 2, 3))

#### 5. Generar base de datos procesada para el análisis ####
## 5.1 Reformatear objeto (CASEN) a BBDD
elsoc <-as.data.frame(elsoc)

## 5.2 Generar tabla descriptiva general
stargazer(elsoc, type="text")

## 5.3 Guardar base de datos en una ruta particular
save(CASEN, file ="input/CASEN.rdata")




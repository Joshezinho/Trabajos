### Práctico 4       ###
### Joshe Norambuena ###
########################

#### 1. Cargar librerías ####
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos

#### 2. Cargar base de datos ####
load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData")) #Cargar base de datos

#### 3. Descripción de variables ####
# 3.1 Tabla descriptiva con stargazer
stargazer(proc_data,type = "text")

# 3.2 Tabla descriptiva con descr
sjmisc::descr(proc_data)

# 3.2.1 Seleccionar sólo columnas que nos interesan
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

# 3.3 Tabla descriptiva con summarytools
summarytools::dfSummary(proc_data, plain.ascii = FALSE)

# 3.3.1 Tabla pequeña
view(dfSummary(proc_data, headings=FALSE))

# 3.4 Casos perdidos
# 3.4.1 Dimensiones de la BBDD
proc_data_original <-proc_data
dim(proc_data)

# 3.4.2 Sumatoria de todos los NA
sum(is.na(proc_data))

# 3.4.3 Omitir casos perdidos
proc_data <-na.omit(proc_data)
dim(proc_data)

# 3.4.4 Recuperar etiqueta 
proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)

# 3.5 Visualización de variables
# ggplot funcina por capas, por ende hay que ir guíandolo paso a paso
proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "coral")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia")

# 3.5.1 Crear el gráfico usando ggplot2
graph1 <- proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "coral")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia") +
  theme_bw()

graph1

# 3.5.1.2 Guardar el gráfico
ggsave(graph1, file="files/img/graph1.png")

# 3.6 Exploración de asociación entre variables
# 3.6.1 Tablas de contingencia para variables categóricas
sjt.xtab(proc_data$educacion, proc_data$sexo)

# 3.6.2 Tablas de contingencia con porcentaje fila/columna
sjt.xtab(proc_data$educacion, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

# 3.6.3 Tabla de contingencia para variables categóricas
sjt.xtab(proc_data$educacion, proc_data$sexo)

#3.6.3.1 Agregar porcentaje fila/columna
sjt.xtab(proc_data$educacion, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

# 3,7 Tablas de promedio de variable continua por una categóricas
tapply(proc_data$conf_inst, proc_data$educacion, mean)

# 3.7.1 Tabla de promedio con mayor información
proc_data %>% # se especifica la base de datos
  select(conf_inst,educacion) %>% # se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(educacion)) %>% # se agrupan por la variable categórica y se usan sus etiquetas con as_label
  dplyr::summarise(Obs.=n(),Promedio=mean(conf_inst),SD=sd(conf_inst)) %>% # se agregan las operaciones a presentar en la tabla
  kable(., format = "markdown") # se genera la tabla


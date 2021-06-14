#https://datos.cdmx.gob.mx/dataset/pasajeros-transportados-por-centrales-de-autobuses
#Esta base de datos muestra el número total de pasajeros transportados por centrales de autobuses de la Ciudad de México. 2010-2020
#El objetivo es comparar los datos para ver si durante la pandemia hubo una disminucion notoria en el numero de
#pasajeros transportados.

library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
#library("writexl")

pasajeros <- read.csv("pasajeros-transportados-por-centrales-de-autobuses.csv", header = TRUE) #leemos archivo csv

PJ <- select(pasajeros, fecha : total) #Seleccionamos columnas necesarias
class(PJ)
str(PJ)

pj <- na.omit(PJ) #Eliminamos filas que contengan NA's
str(pj)

#Cambiamos el tipo de dato de cada variable y eliminamos comas y puntos de los numeros

pj <- mutate(pj, fecha = as.Date(fecha, "%d/%m/%Y"),
             central_del_norte = as.numeric(gsub(",","",central_del_norte,fixed=TRUE)),
             central_poniente = as.numeric(gsub(",","",central_poniente,fixed=TRUE)),
             central_oriente = as.numeric(gsub(".","",central_oriente,fixed=TRUE)),
             central_sur = as.numeric(gsub(".","",central_sur,fixed=TRUE)),
             total = as.numeric(gsub(",","",total,fixed=TRUE)))

#ts.plot(pj[-1],col=c(rep("green"),rep("gray"),rep("yellow"),rep("blue"),rep("red")))

str(pj)   

pj <- pj[order(pj$fecha), ] #Ordenamos por fecha

head(pj)
tail(pj)


#write_xlsx(pj, "datos.xlsx") #Para guardar tabla

#Convertimos el dataframe en serie de tiempo

norte <- ts(pj[, 2], start = 2010, frequency = 12)
poniente <- ts(pj[, 3], start = 2010, frequency = 12)
oriente <- ts(pj[, 4], start = 2010, frequency = 12)
sur <- ts(pj[, 5], start = 2010, frequency = 12)
t <- ts(pj[, 6], start = 2010, frequency = 12)

#Graficamos para comparar las 4 centrales de autobuses

plot(cbind(norte, poniente, oriente, sur), 
     main = "Pasajeros transportados por centrales de autobuses de la CDMX", 
     xlab = "Tiempo",
     sub = "Enero de 2010 - Diciembre de 2018")


#Graficamos el total de pasajeros de las 4 centrales de 2010 a 2018

ggplot(data = pj, aes(x = fecha, y = total)) +
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle ("Total de pasajeros transportados en centrales de autobuses.") +
  theme(panel.background = element_rect(fill = '#D5E2ED', colour = '#7A91A1')) +
  theme (plot.title = element_text(family ="Times New Roman",
                                   size = rel(2), #Tamaño relativo de la letra del título
                                   vjust = 1, #Justificación vertical, para separarlo del gráfico
                                   hjust = 1,
                                   face = "bold",
                                   color = "#0A90D1", #Color del texto
                                   lineheight=1.5))#Separacion entre lineas


#No pude llegar al objetivo de ver si durante la pandemia hubo una disminucion notoria en el numero de
#pasajeros transportados. A pesar de que el csv tenia registrada la fecha hasta diciembre 2020, desde 
#julio 2018 ya no hubo informacion de pasajeros.
#Podemos observar que en casi todos los anos hay una disminucion en el mes de febrero y aumento en diciembre.

#Graficas de cada central de autobuses

a <- ggplot(data = pj, aes(x = fecha, y = central_del_norte)) +
  geom_line(color = "#71C735", size = 1) +
  theme(panel.background = element_rect(fill = '#D5E2ED', colour = '#7A91A1'))

b <- ggplot(data = pj, aes(x = fecha, y = central_poniente)) +
  geom_line(color = "#31C2DE", size = 1) +
  theme(panel.background = element_rect(fill = '#D5E2ED', colour = '#7A91A1'))

c <- ggplot(data = pj, aes(x = fecha, y = central_oriente)) +
  geom_line(color = "#DE9E31", size = 1) +
  theme(panel.background = element_rect(fill = '#D5E2ED', colour = '#7A91A1'))

d <- ggplot(data = pj, aes(x = fecha, y = central_sur)) +
  geom_line(color = "#AF79D6", size = 1) +
  theme(panel.background = element_rect(fill = '#D5E2ED', colour = '#7A91A1'))

grid.arrange(a, b, c, d, ncol=2) #Multiples graficos en la misma pagina


library(tidyverse)
library(ggfortify)
library(lubridate)
library(forecast)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2("TICKETS CERRADOS.csv", encoding = "UTF-8") 

###################
## Seri temporal ##
###################

Creacion <- Tickets %>%
  select(Creacion) %>%
  group_by(Creacion) %>%
  summarise(N=n())

# Diario
ts_Creacion_Diarios <- ts(Creacion$N, start=c(2015,7), frequency=365)

autoplot(ts_Creacion_Diarios) +
  ggtitle("Tickets Creacions por dia") +
  ylab("# de tickets") +
  xlab("dias") 

# Mensual
Creacion_Mensual <- Creacion %>%
  mutate(MES = month(Creacion), ANNO = year(Creacion)) %>%
  group_by(ANNO, MES) %>%
  select(ANNO, MES, N) %>%
  summarise(N = sum(N))

ts_Creacion_Mensual <- ts(Creacion_Mensual$N, start=c(2018,1), frequency=12)

autoplot(ts_Creacion_Mensual) +
  ggtitle("Tickets Creacions por mes") +
  ylab("# de tickets") +
  xlab("Mes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y/%m", date_breaks = "1 months")



###############################
# Numero de d?as de respuesta #
###############################

dias_resolucion <- Tickets %>%
  select(Creacion, Cierre) %>%
  mutate(DIA = as.Date(Cierre) - as.Date(Creacion)) %>%
  group_by(DIA) %>%
  summarise(N = n())

ggplot(dias_resolucion, aes(x=DIA, y=N)) +
  geom_bar(stat="identity") +
  ggtitle("Demora en la resolucion de tickets") +
  ylab("# de tickets") +
  xlab("Dias") +
  coord_cartesian(xlim=c(0,25))

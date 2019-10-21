library(tidyverse)
library(ggfortify)
library(lubridate)
library(forecast)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2("TICKETS CERRADOS.csv", encoding = "UTF-8") 

###################
## Seri temporal ##
###################

Creado <- Tickets %>%
  select(Creado) %>%
  group_by(Creado) %>%
  summarise(N=n())

# Diario
ts_Creado_Diarios <- ts(Creado$N, start=c(2015,7), frequency=365)

autoplot(ts_Creado_Diarios) +
  ggtitle("Tickets creados por dia") +
  ylab("# de tickets") +
  xlab("dias") 

# Mensual
Creado_Mensual <- Creado %>%
  mutate(MES = month(Creado), ANNO = year(Creado)) %>%
  group_by(ANNO, MES) %>%
  select(ANNO, MES, N) %>%
  summarise(N = sum(N))

ts_Creado_Mensual <- ts(Creado_Mensual$N, start=c(2018,1), frequency=12)

autoplot(ts_Creado_Mensual) +
  ggtitle("Tickets creados por mes") +
  ylab("# de tickets") +
  xlab("Mes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y/%m", date_breaks = "1 months")



###############################
# Numero de d?as de respuesta #
###############################

dias_resolucion <- Tickets %>%
  select(Creado, Fecha.de.cierre) %>%
  mutate(DIA = as.Date(Fecha.de.cierre) - as.Date(Creado)) %>%
  group_by(DIA) %>%
  summarise(N = n())

ggplot(dias_resolucion, aes(x=DIA, y=N)) +
  geom_bar(stat="identity") +
  ggtitle("Demora en la resolucion de tickets") +
  ylab("# de tickets") +
  xlab("Dias") +
  coord_cartesian(xlim=c(0,25))

library(tidyverse)
library(ggfortify)
library(lubridate)
library(forecast)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 
  
Tickets <-  Tickets[-c(11206),]

Tickets$Creado <- 
  format(as.Date(Tickets$Creado), format='%Y-%m-%d')
Tickets$Fecha.de.cierre <- 
  format(as.Date(Tickets$Fecha.de.cierre), format='%Y-%m-%d')

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

# Semanal
Creado_Semanal <- Creado %>%
  mutate(SEMANA = week(Creado), ANNO = year(Creado)) %>%
  group_by(ANNO, SEMANA) %>%
  select(ANNO, SEMANA, N) %>%
  summarise(N = sum(N))

ts_Creado_Semanal <- ts(Creado_Semanal$N, start=c(2018,1), frequency=52)

autoplot(ts_Creado_Semanal) +
  ggtitle("Tickets creados por semana") +
  ylab("# de tickets") +
  xlab("Semanas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(breaks = "2 weeks")

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
# Numero de días de respuesta #
###############################

dias_resolucion <- Tickets %>%
  filter(Tickets$Fecha.de.cierre != "") %>%
  select(Creado, Fecha.de.cierre) %>%
  mutate(DIA = as.Date(Fecha.de.cierre) - as.Date(Creado)) %>%
  group_by(DIA) %>%
  summarise(N = n())

ggplot(dias_resolucion, aes(x=DIA, y=N)) +
  geom_bar(stat="identity") +
  ggtitle("Demora de resolución de ticket") +
  ylab("# de tickets") +
  xlab("Días") +
  coord_cartesian(xlim=c(0,15))

library(tidyverse)
library(ggfortify)
library(lubridate)
library(forecast)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") %>%
  
Tickets <-  Tickets[-c(11206),]

Tickets$Creado <- format(as.Date(Tickets$Creado), format='%Y-%m-%d')

Creado <- Tickets %>%
  select(Creado) %>%
  group_by(Creado) %>%
  summarise(N=n())

# Diario
ts_Creado_Diarios <- ts(Creado$N, start=c(2015,7), frequency=365)

autoplot(ts_Creado_Diarios) +
  ggtitle("Tickets creados por día") +
  ylab("# de tickets") +
  xlab("días") 

# Mensual
Creado_Mensual <- Creado %>%
  mutate(MES = month(Creado), ANNO = year(Creado)) %>%
  group_by(ANNO, MES) %>%
  select(ANNO, MES, N) %>%
  summarise(N = sum(N))

Creado_Mensual <- Creado_Mensual[-1,]

ts_Creado_Mensual <- ts(Creado_Mensual$N, start=c(2018,1), frequency=12)
ts_Creado_Mensual

autoplot(ts_Creado_Mensual) +
  ggtitle("Tickets creados por mes") +
  ylab("# de tickets") +
  xlab("Mes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y/%m", date_breaks = "1 months")

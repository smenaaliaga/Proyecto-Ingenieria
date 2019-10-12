library(tidyverse)

setwd("C:/Users/smena/Desktop/Proyecto Ingenieria EII/Origen de datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8")

### SERVICIO
Servicios <- Tickets %>%
  select(Servicio) %>%
  group_by(Servicio) %>%
  summarise(N=n())

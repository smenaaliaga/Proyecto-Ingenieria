library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 

Tickets <- Tickets %>%
  filter(Fecha.de.cierre != "", Número != 11206) %>% 
  mutate(Servicio = replace(as.character(Servicio), 
                            Servicio == "", 
                            "No Clasificado"))

Tickets$Creado <- 
  format(as.Date(Tickets$Creado), format='%Y-%m-%d')

Tickets$Fecha.de.cierre <- 
  format(as.Date(Tickets$Fecha.de.cierre), format='%Y-%m-%d')

write.csv2(Tickets, "TICKETS CERRADOS.csv")

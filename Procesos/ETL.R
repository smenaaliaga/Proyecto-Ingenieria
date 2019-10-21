library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 

Tickets <- Tickets %>%
  select(Título, Creado, Fecha.de.cierre, Cola, 
         Prioridad, Cliente, ID.del.cliente, Servicio, Tipo) %>%
  rename(Tituo = Título, Creacion = Creado, 
         Cierre = Fecha.de.cierre, ID.Cliente = ID.del.cliente) %>%
  mutate(Servicio = replace(as.character(Servicio), 
                            Servicio == "", "No Clasificado")) %>%
  filter(Cierre != "", Cliente != "") 

Tickets$Creado <- 
  format(as.Date(Tickets$Creacion), format='%Y-%m-%d')

Tickets$Fecha.de.cierre <- 
  format(as.Date(Tickets$Cierre), format='%Y-%m-%d')

write.csv2(Tickets, "TICKETS CERRADOS.csv")


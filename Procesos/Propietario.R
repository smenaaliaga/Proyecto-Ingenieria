library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 
  
Tickets <-  Tickets[-c(11206),]

##################
## Propietarios ##
##################

Propietario <- Tickets %>%
  select(Agente.Propietario) %>%
  group_by(Agente.Propietario) %>%
  summarise(N=n())

ggplot(Propietario, aes(x=Agente.Propietario, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Propietario") +
  ylab("# de tickets") +
  xlab("Propietarios") 

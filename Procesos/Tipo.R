library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 

Tickets <- Tickets %>%
  filter(Fecha.de.cierre != "", Número != 11206)


##########
## Tipo ##
##########

Tipo <- Tickets %>%
  select(Tipo) %>%
  group_by(Tipo) %>%
  summarise(N=n())

ggplot(Tipo, aes(x=Tipo, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de tickets por tipo") +
  ylab("# de tickets") +
  xlab("Tipos") +
  geom_text(aes(x = Tipo, y = N, label = N), vjust = -0.5)

#####################
## Tipo - Servicio ##
#####################

Tipo_Servicio <- Tickets %>%
  select(Tipo, Servicio) %>%
  filter(Servicio != "", Servicio != "Software") %>%
  group_by(Tipo, Servicio) %>%
  summarise(N=n())

Tipo_Servicio_Total <- 
  Tipo_Servicio[!grepl("::", Tipo_Servicio$Servicio),]

## Tipos por servicios
ggplot(Tipo_Servicio_Total, aes(x=Tipo, y=N, fill=Servicio)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Tipo de ticket por servicio") +
  ylab("# de tickets") +
  xlab("Tipos") +
  scale_fill_discrete(name="Servicio",
                      breaks=c("Hardware", "Redes"),
                      labels=c("Hardware", "Red"))

##################################################################
# Tipo - Hardware
Tipo_Hardware <- Tipo_Servicio %>%
  filter(str_detect(Servicio, "Hardware"), Servicio != "Hardware",
         Tipo %in% c("Incidente","Requerimiento"))

Tipo_Hardware$Servicio <- 
  gsub("Hardware::", "", Tipo_Hardware$Servicio)

Tipo_Hardware <- Tipo_Hardware[!grepl("::", Tipo_Hardware$Servicio),]

ggplot(Tipo_Hardware, aes(x=Servicio, y=N, fill=factor(Tipo))) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Tipo de ticket por servicio de Hardware") +
  ylab("# de tickets") +
  xlab("Hardware") +
  scale_fill_discrete(name="Servicio",
                      breaks=c("Incidente", "Requerimiento"),
                      labels=c("Incidente", "Requerimiento"))

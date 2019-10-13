library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 
  
Tickets <-  Tickets[-c(11206),]

##############
## SERVICIO ##
##############

Servicios <- Tickets %>%
  select(Servicio) %>%
  group_by(Servicio) %>%
  summarise(N=n())

Servicios <- Servicios[-1,]

Servicios_Total <- Servicios[!grepl("::", Servicios$Servicio),]

ggplot(Servicios_Total, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Servicios") +
  ylab("# de tickets") +
  xlab("Servicios") 


### SERVICIO - HARDWARE
Hardware <- Servicios %>%
  filter(str_detect(Servicio, "Hardware"))

Hardware$Servicio <- gsub("Hardware::", "", Hardware$Servicio)

Hardware <- Hardware[-1,]

Hardware_Total <- Hardware[!grepl("::", Hardware$Servicio),]

Hardware_Total <- Hardware_Total[-1,]

ggplot(Hardware_Total, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Hardware") +
  ylab("# de tickets") +
  xlab("Servicios") 


## SERVICIO - HARDWARE - IMPRESORA
Hardware_Impresora <- Hardware %>%
  filter(str_detect(Servicio, "IMPRESORAS")) 

Hardware_Impresora$Servicio <- 
  gsub("IMPRESORAS::", "", Hardware_Impresora$Servicio)  

Hardware_Impresora_Total <- 
  Hardware_Impresora[!grepl("::", Hardware_Impresora$Servicio),]

Hardware_Impresora_Total <- Hardware_Impresora_Total[-1,]

ggplot(Hardware_Impresora_Total, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Hardware - Impresora") +
  ylab("# de tickets") +
  xlab("Servicios") 

## SERVICIO - HARDWARE - IMPRESORA - LASER
Hardware_Impresora_Laser <- Hardware_Impresora %>%
  filter(str_detect(Servicio, "LASER")) 

Hardware_Impresora_Laser$Servicio <- 
  gsub("LASER::", "", Hardware_Impresora_Laser$Servicio)  

ggplot(Hardware_Impresora_Laser, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Hardware - Impresora - Laser") +
  ylab("# de tickets") +
  xlab("Servicios") 

## SERVICIO - HARDWARE - IMPRESORA - MULTIFUNCIONAL
Hardware_Impresora_Multifuncional <- Hardware_Impresora %>%
  filter(str_detect(Servicio, "MULTIFUNCIONAL")) 

Hardware_Impresora_Multifuncional$Servicio <- 
  gsub("MULTIFUNCIONAL::", "", Hardware_Impresora_Multifuncional$Servicio)  

ggplot(Hardware_Impresora_Multifuncional, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Hardware - Impresora - Multifuncional") +
  ylab("# de tickets") +
  xlab("Servicios") 

### SERVICIO - RED
Redes <- Servicios %>%
  filter(str_detect(Servicio, "Redes"))

Redes$Servicio <- gsub("Redes::", "", Redes$Servicio)

Redes <- Redes[-1,]

ggplot(Redes, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Redes") +
  ylab("# de tickets") +
  xlab("Servicios") 

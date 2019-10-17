library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8") 

Tickets <- Tickets %>%
  filter(Fecha.de.cierre != "", Número != 11206)

##############
## SERVICIO ##
##############

Servicios <- Tickets %>%
  select(Servicio, ) %>%
  group_by(Servicio) %>%
  summarise(N=n()) 

Servicios_Agregados <- Tickets %>%
  select(Servicio) 

Servicios_Agregados$Servicio <- gsub("\\::.*","",Servicios_Agregados$Servicio)

Servicios_Agregados <- Servicios_Agregados %>%
  group_by(Servicio) %>%
  summarise(N=n()) 

Servicios_Total <- Servicios[!grepl("::", Servicios$Servicio),]
 
ggplot(Servicios_Agregados, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por servicios solicitados") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)

# Eliminar Software y no clasificados
Servicios_Alcance <- Servicios_Agregados[-1,]
Servicios_Alcance <- Servicios_Alcance[!grepl("Software", Servicios_Alcance$Servicio),]

ggplot(Servicios_Alcance, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por servicios solicitados") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)


### SERVICIO - HARDWARE
Hardware <- Servicios %>%
  filter(str_detect(Servicio, "Hardware"))

Hardware$Servicio <- gsub("Hardware::", "", Hardware$Servicio)

Hardware_Agregado <- Servicios %>%
  filter(str_detect(Servicio, "Hardware"))

Hardware_Agregado$Servicio <- gsub("Hardware::", "", Hardware_Agregado$Servicio)

Hardware_Agregado$Servicio <- gsub("\\::.*","",Hardware_Agregado$Servicio)

Hardware_Agregado <- Hardware_Agregado %>%
  group_by(Servicio) %>%
  summarise(N = sum(N)) 

ggplot(Hardware_Agregado, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de tickets por servicio de hardware solicitado") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)


## SERVICIO - HARDWARE - IMPRESORA
Hardware_Impresora <- Hardware %>%
  filter(str_detect(Servicio, "IMPRESORAS"))

Hardware_Impresora$Servicio <- 
  gsub("IMPRESORAS::", "", Hardware_Impresora$Servicio)  

Hardware_Impresora_Agregado <- Hardware %>%
  filter(str_detect(Servicio, "IMPRESORAS")) 

Hardware_Impresora_Agregado$Servicio <- 
  gsub("IMPRESORAS::", "", Hardware_Impresora_Agregado$Servicio)  

Hardware_Impresora_Agregado$Servicio <- 
  gsub("\\::.*","",Hardware_Impresora_Agregado$Servicio)

Hardware_Impresora_Agregado <- Hardware_Impresora_Agregado %>%
  group_by(Servicio) %>%
  summarise(N = sum(N)) 

ggplot(Hardware_Impresora_Agregado, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por servicio de impresora solicitado") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)

## SERVICIO - HARDWARE - IMPRESORA - LASER
Hardware_Impresora_Laser <- Hardware_Impresora %>%
  filter(str_detect(Servicio, "LASER")) 

Hardware_Impresora_Laser$Servicio <- 
  gsub("LASER::", "", Hardware_Impresora_Laser$Servicio)  

ggplot(Hardware_Impresora_Laser, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por servicio de impresora laser solicitado") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)

## SERVICIO - HARDWARE - IMPRESORA - MULTIFUNCIONAL
Hardware_Impresora_Multifuncional <- Hardware_Impresora %>%
  filter(str_detect(Servicio, "MULTIFUNCIONAL")) 

Hardware_Impresora_Multifuncional$Servicio <- 
  gsub("MULTIFUNCIONAL::", "", Hardware_Impresora_Multifuncional$Servicio)  

ggplot(Hardware_Impresora_Multifuncional, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por servicio de impresora multifuncional solicitado") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)

### SERVICIO - RED
Redes <- Servicios %>%
  filter(str_detect(Servicio, "Redes"))

Redes$Servicio <- gsub("Redes::", "", Redes$Servicio)

ggplot(Redes, aes(x=Servicio, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por servicio de red solicitado") +
  ylab("# de tickets") +
  xlab("Servicios") +
  geom_text(aes(x = Servicio, y = N, label = N), vjust = -0.5)

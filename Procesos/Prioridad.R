library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8")
  
Tickets <-  Tickets[-c(11206),]

###############
## Prioridad ##
###############

Prioridad <- Tickets %>%
  select(Prioridad) %>%
  group_by(Prioridad) %>%
  summarise(N=n())

ggplot(Prioridad, aes(x=Prioridad, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por prioridad") +
  ylab("# de tickets") +
  xlab("Prioridades") +
  geom_text(aes(x = Prioridad, y = N, label = N), vjust = -0.5)

##########################
## Prioridad - Servicio ##
##########################

Prioridad_Servicio <- Tickets %>%
  filter(Servicio != "", Servicio != "Software") %>%
  group_by(Prioridad, Servicio) %>%
  summarise(N=n())

Prioridad_Servicio <- Prioridad_Servicio[!grepl("::", Prioridad_Servicio$Servicio),]

ggplot(Prioridad_Servicio, aes(x=Prioridad, y=N, fill = Servicio)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cantidad de ticket por prioridad y servicio") +
  ylab("# de tickets") +
  xlab("Prioridades") +
  scale_fill_discrete(name="Servicio",
                      breaks=c("Hardware", "Redes"),
                      labels=c("Hardware", "Red"))

###########################################
## Dias resolucion - Prioridad- Servicio ##
###########################################

resol_prioridad_servicio <- Tickets %>%
  filter(Tickets$Fecha.de.cierre != "", Servicio != "", Servicio != "Software") %>%
  mutate(Dia_Resol = as.Date(Fecha.de.cierre) - as.Date(Creado)) %>%
  select(Prioridad, Servicio, Dia_Resol) 

resol_prioridad_servicio <- 
  resol_prioridad_servicio[!grepl("::", resol_prioridad_servicio$Servicio),]

ggplot(resol_prioridad_servicio, 
       aes(x=Prioridad, y=Dia_Resol, fill=Servicio)) + 
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,100)) +
  ggtitle("Demora de resolución por prioridad y servicio") +
  ylab("Días") +
  xlab("Prioridades") 

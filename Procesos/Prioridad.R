library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Base de Datos")

Tickets <- read.csv2("TICKETS CERRADOS.csv", encoding = "UTF-8") 

###############
## Prioridad ##
###############

Prioridad <- Tickets %>%
  select(Prioridad) %>%
  group_by(Prioridad) %>%
  summarise(N=n())

ggplot(Prioridad, aes(x=Prioridad, y=N)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Cantidad de ticket por prioridad asignada") +
  ylab("# de tickets") +
  xlab("Prioridades") +
  geom_text(aes(x = Prioridad, y = N, label = N), vjust = -0.5)

##########################
## Prioridad - Servicio ##
##########################

Prioridad_Servicio <- Tickets
Prioridad_Servicio$Servicio <- 
  gsub("\\::.*","",Prioridad_Servicio$Servicio)

Prioridad_Servicio <- Prioridad_Servicio %>%
  filter(Servicio != "No Clasificado", Servicio != "Software") %>%
  group_by(Prioridad, Servicio) %>%
  summarise(N=n())

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
  ggtitle("Demora de resoluci?n por prioridad y servicio") +
  ylab("D?as") +
  xlab("Prioridades") 

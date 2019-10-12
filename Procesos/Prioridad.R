library(tidyverse)

setwd("C:/Users/smena/Documents/Proyecto-Ingenieria/Origen de datos")

Tickets <- read.csv2(
  "TICKET_CREADOS_POR_FECHA_Created_2019-10-11_21-27.csv", 
  encoding = "UTF-8")

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
  ggtitle("Prioridad") +
  ylab("# de tickets") +
  xlab("Prioridades") 

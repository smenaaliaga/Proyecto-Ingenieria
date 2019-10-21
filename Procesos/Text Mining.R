# Se utiliza este script para entender la naturaleza de los titulos de
# tickets clasificados y asi poder categorizar los no clasificados

# Funcion para estandarizar titulos de tickets en tokens
limpieza <- function(texto){
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}



# Aplicación de funcion

## G1: Filtrar tickets por servicio clasificado y tipo requerimiento
Tickets_SoloReq_Clas <- Tickets %>% 
  filter(Servicio != "No Clasificado", Tipo == "Requerimiento") %>% 
  mutate(Token = map(.x = Titulo, .f = limpieza))

Tickets_SoloReq_Clas %>% select(Token) %>% head()

## G1: Se expande Token en N column igual a largo del Token
Tickets_SoloReq_Clas <- Tickets_SoloReq_Clas  %>% unnest()

## G2: Filtrar tickets por servicio NO clasificado y tipo requerimiento
Tickets_SoloReq_NoClas <- Tickets %>% 
  filter(Servicio == "No Clasificado", Tipo == "Requerimiento") %>% 
  mutate(Token = map(.x = Titulo, .f = limpieza))

tweets %>% select(texto_tokenizado) %>% head()

## G2: Se expande Token en N column igual a largo del Token
Tickets_SoloReq_NoClas <- Tickets_SoloReq_NoClas  %>% unnest()



# Palabras mas utilizadas

## en G1
Tickets_SoloReq_Clas %>% group_by(Token) %>% count(Token) %>%
  arrange(desc(n)) %>% print(n=30)
## en G2
Tickets_SoloReq_NoClas %>% group_by(Token) %>% count(Token) %>%
  arrange(desc(n)) %>% print(n=30)



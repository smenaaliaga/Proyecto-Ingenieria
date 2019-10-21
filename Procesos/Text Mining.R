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

Tickets <- Tickets %>% 
  mutate(Titulo_Tranform = map(.x = Titulo, .f = limpieza))

Tickets_Clas_SoloReq <- Tickets %>% 
  filter(Servicio != "No Clasificado", Tipo == "Requerimiento") %>% 
  mutate(Titulo_Tranform = map(.x = Titulo, .f = limpieza))

Tickets_NoClas_SoloReq <- Tickets %>% 
  filter(Servicio == "No Clasificado", Tipo == "Requerimiento") %>% 
  mutate(Titulo_Tranform = map(.x = Titulo, .f = limpieza))

Tickets_NC$Titulo_Tranform

Tickets_NC %>% 
  group_by(ID.del.cliente) %>%
  count(N = n()) %>%
  arrange(desc(n))

tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)
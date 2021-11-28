library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(xts)
library(infer)



url_resetera <- "https://www.resetera.com/threads/cyberpunk-2077-twitter-account-makes-transphobic-joke-read-op.63305/"

resetera_xml <- read_html(url_resetera)

mensajes <- extract_posts(url_resetera)

head(mensajes)


#Tokenización de palabras
cdproj_tibble <- mensajes %>% bind_rows() %>% unnest_tokens(word, Texto)

#Filtramos segun lexicones
cdproj_nrc <- cdproj_tibble %>% inner_join(get_sentiments("nrc"))
cdproj_loughran <- cdproj_tibble %>% inner_join(get_sentiments("loughran"))
cdproj_bing <- cdproj_tibble %>% inner_join(get_sentiments("bing"))
cdproj_afinn <- cdproj_tibble %>% inner_join(get_sentiments("afinn"))



cdproj_nrc %>% count(word) %>% arrange(desc(n)) %>% top_n(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) + geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Nº palabras",
       x = "Palabra")


cdproj_nrc %>% count(sentiment) %>% arrange(desc(n)) %>%
  ggplot(aes(x = reorder(sentiment, n), y = n)) + geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Nº palabras",
       x = "Sentimientos")


cdproj_loughran %>% count(sentiment) %>% arrange(desc(n)) %>%
  ggplot(aes(x = reorder(sentiment, n), y = n)) + geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Nº palabras",
       x = "Sentimientos",
       title = "Sentimientos en el hilo 'Cyberpunk 2077 Twitter account\n makes transphobic joke (READ OP)', lexicon de Loughran") +
  theme_bw()

cdproj_bing %>% count(sentiment) %>% arrange(desc(n)) %>%
  ggplot(aes(x = reorder(sentiment, n), y = n)) + geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Nº palabras",
       x = "Sentimientos",
       title = "Sentimientos en el hilo 'Cyberpunk 2077 Twitter account\n makes transphobic joke (READ OP)', lexicon de Bing") +
  theme_bw()

cdproj_afinn %>% group_by(score) %>% count(word) %>% arrange(desc(n)) %>% top_n(5) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(. ~ score, scales = "free_y") +
  labs(y = "Nº palabras",
       x = "Sentimientos",
       title = "Sentimientos en el hilo 'Cyberpunk 2077 Twitter account\n makes transphobic joke (READ OP)' según puntuación, \n lexicon de afinn") +
  theme_bw() 


cdproj_afinn %>% summarise(suma = sum(score))
cdproj_afinn %>% count(score) %>% summarise(Sumatotal = sum(score * n))

#¿Analisis temporal afinn?

cdproj_afinn %>% group_by(Fecha) %>%
  summarise(Puntuacion = sum(score)) %>%
  mutate(Es_positivo = ifelse(Puntuacion >= 0, "Si", "No")) %>%
  ggplot(aes(x = Fecha, y = Puntuacion, fill = Es_positivo)) + 
  geom_bar(stat = "identity") +
  labs(title = "Sentimientos en ese HILO por minuto (holaaa ;D)")

cdproj_afinn %>% mutate(Tiempo = floor_date(Fecha, unit = "hour")) %>% 
  group_by(Tiempo) %>% 
  summarise(Puntuacion = sum(score)) %>%
  mutate(Es_positivo = ifelse(Puntuacion >= 0, "Si", "No"))%>%
  ggplot(aes(x = Tiempo, y = Puntuacion, fill = Es_positivo)) + 
  geom_bar(stat = "identity") +
  labs(title = "Sentimientos en ese HILO por HORA (Ahora si ;D)")


## Importación paquetes
library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(xts)
library(infer)

## Loading our own functions
source(file.path("input", "ReseteraFunctions.R"))

## Extracción mensajes foro ====
# Escogemos una página con multicita, que es el mensaje número 13
url_resetera <- "https://www.resetera.com/threads/no-mans-sky-next-ot-a-korvax-a-gek-and-a-vykeen-walk-into-a-base.57062/page-2"

resetera_xml <- read_html(url_resetera)

test_cita <- (html_nodes(resetera_xml, ".messageText.SelectQuoteContainer.ugc.baseHtml") %>% html_text() %>% str_trim())[13]
test_cita_2 <- (html_nodes(resetera_xml, ".messageText.SelectQuoteContainer.ugc.baseHtml") %>% html_text() %>% str_trim())[1]

texto_posts_raw <- html_nodes(resetera_xml, ".messageText.SelectQuoteContainer.ugc.baseHtml") %>% html_text() %>% str_trim()


start_quote <- one_or_more(WRD) %R% SPACE %R% "said:"
end_quote <- "false" %R% SPACE %R% "\\}" %R% SPACE

str_view(test_cita, start_quote)
str_view(test_cita, end_quote)

# Localizaciones de inicio y final de citas. Nos interesa la segunda y la primera columna, respectivamente.
str_locate_all(test_cita, start_quote)[[1]]
str_locate_all(test_cita, end_quote)[[1]]

citas_rango <- cbind(str_locate_all(test_cita, start_quote)[[1]][, 1] - 1 ,
                     str_locate_all(test_cita, end_quote)[[1]][, 1])


citas_rango_final <- c(1,
                       citas_rango %>% t() %>% as.numeric(),
                       str_length(test_cita)) %>% matrix(byrow = T,
                                                         ncol = 2)

test_cita %>% str_sub(citas_rango[[1]][1], citas_rango[[1]][2])

test_cita %>% map2(citas_rango, function(.x, .y) str_sub(.x, .y[[1]]) )

# Esto debería de extraer las citas textuales
str_sub(test_cita,
        citas_rango[, 1],
        citas_rango[, 2])

# Y esto, extraer todo el texto que no son las citas
texto_sin_citas  <- str_sub(test_cita,
                            citas_rango_final[, 1],
                            citas_rango_final[, 2]) %>% str_trim()

# Probablemente nos interese que todos los mensajes de un post, que no son las
# citas, estén en un único elemento del vector.
texto_sin_citas <- texto_sin_citas %>% str_c(collapse = " ")

# Ya hemos fabricado la función rm_citas() para extraer citas textuales de una lista de mensajes.

# Nos interesaría extraer la información de todas las páginas del hilo
# La última página siempre va a provenir 
pagina <- html_nodes(resetera_xml, ".pageNavLinkGroup") %>% html_text() %>% str_squish()
pagina <- pagina[1]
last_pagina <- pagina %>%
  str_extract(one_or_more(DGT) %R% SPC %R% ANY_CHAR %R% END) %>%
  str_extract(one_or_more(DGT))

# Generamos un string con las urls de todas las páginas
url_base <- "https://www.resetera.com/threads/no-mans-sky-next-ot-a-korvax-a-gek-and-a-vykeen-walk-into-a-base.57062/"
paginas <- 2:last_pagina
urls_reset <- c(url_base, str_c(url_base, "page-", paginas))


reset_masivo <- read_html(urls_reset)


# Vamos a probar la función para todas las páginas. Llevará un buen tiempo
resetera_tabla <- extract_posts(url_base)

# Nos quedamos con tablas que tengan información temporal
resetera_tabla_new <- resetera_tabla[10:140]

reset_tibble <- bind_rows(resetera_tabla_new)

head(reset_tibble)


## Tokenización con tidytext====
# Hagamos un tokenize de estos wapos
reset_tokens <- reset_tibble %>% unnest_tokens(word, Texto)

# Ahora tendríamos que relacionar las palabras con alguno de los lexicones
reset_nrc <- reset_tokens %>% inner_join(get_sentiments("nrc"), .id = "word")
reset_bing <- reset_tokens %>% inner_join(get_sentiments("bing"), .id = "word")
reset_afinn <- reset_tokens %>% inner_join(get_sentiments("afinn"), .id = "word")

reset_bing$sentiment <- as.factor(reset_bing$sentiment)

# Nos podria interesar dividir Fecha en año, meses, hora y minutos

reset_nrc <- reset_nrc %>% mutate(Hora = hour(Fecha),
                                  Dia = day(Fecha)) %>% 
  select(Fecha, Dia, Hora, sentiment, word)

reset_afinn <- reset_afinn %>% mutate(Hora = hour(Fecha),
                                      Dia = day(Fecha)) %>% 
  select(Fecha, Dia, Hora, word, score)

# Ahora exploraremos las palabras que más se repiten en el hilo estudiado (OT No Mans Sky)
reset_nrc %>% count(word) %>% arrange(desc(n)) %>% top_n(10)
reset_nrc %>% count(sentiment) %>% arrange(desc(n))

reset_nrc %>% count(sentiment, word) %>% arrange(desc(n)) %>% top_n(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_bar(stat = "identity") + coord_flip()

reset_nrc %>% count(word) %>% arrange(desc(n)) %>% top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_bar(stat = "identity", fill = "red") +
  coord_flip() + labs(title = "Palabras más pronunciadas en 'No Mans Sky: NEXT [OT] (lexicon nrc)'",
                      y = "Frecuencia",
                      x = "Palabra") +
  scale_fill_brewer(type = , palette = 2)

reset_nrc %>% count(Hora, sentiment)

reset_nrc %>% count(sentiment) %>% arrange(desc(n)) %>% top_n(10) %>%
  ggplot(aes(x = reorder(sentiment, n), y = n)) + geom_bar(stat = "identity", fill = "blue") +
  coord_flip() + labs(title = "Sentimientos más frecuentes en 'No Mans Sky: NEXT [OT]' (lexicon nrc)",
                      y = "Frecuencia",
                      x = "Sentimiento")


reset_bing %>% count(sentiment, word) %>% top_n(10)


reset_bing %>% count(word, sentiment) %>%
  arrange(desc(n)) %>%
  top_n(10) %>% ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") + coord_flip()

reset_bing %>% count(sentiment) %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Total palabras negativas y positivas del \n hilo 'No Mans Sky: NEXT [OT]' (lexicon bing)",
       y = "Frecuencia",
       x = "Sentimiento") +
  theme(plot.title = element_text(hjust = 0.5))


# ¿Que tal con el afinn?
# Media según dia
reset_afinn %>% group_by(Dia) %>% summarise(Puntuacion = mean(score)) %>%
  ggplot(aes(x = as.factor(Dia), y = Puntuacion)) + geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = mean(reset_afinn$score), colour = "Media total"), lty = 2, lwd = 1.2, show.legend = TRUE) +
  labs(title = "Media puntuación según dia (máximo 5 mínimo -5) \n 'No Mans Sky: NEXT [OT]'",
       y = "Media",
       x = "Dia de Julio 2018") +
  theme(plot.title = element_text(hjust = 0.5))

# Media según hora
reset_afinn %>% group_by(Hora) %>% summarise(Puntuacion = mean(score)) %>%
  ggplot(aes(x = as.factor(Hora), y = Puntuacion)) + geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = mean(reset_afinn$score), colour = "Media total"), lty = 2, lwd = 1.2, show.legend = TRUE) +
  labs(title = "Media puntuación según Hora (máximo 5 mínimo -5) \n 'No Mans Sky: NEXT [OT]'",
       y = "Media",
       x = "Hora") +
  theme(plot.title = element_text(hjust = 0.5))


# ¿Las palabras más populares según puntuación
reset_afinn %>% group_by(score) %>% count(word) %>% top_n(5) %>%
  mutate(sentiment = ifelse(score < 0, "negative", "positive")) %>% 
  ggplot(aes(x = reorder(word, desc(n)), y = n, fill = sentiment)) + geom_bar(stat = "identity") +
  facet_wrap( ~ score, scales = "free") +
  labs(title = "Palabras más populares según puntuación \n 'No Mans Sky: NEXT [OT]'",
       x = "Palabra",
       y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   vjust = unit(0.75, "mm")))



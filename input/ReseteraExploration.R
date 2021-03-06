
## Installing necessary packages
install.packages(c("pracma", "polite", "tidytext", "wordcloud", "rebus"))

## Importing packages
library(tidyverse)
library(tidytext)
library(wordcloud)
library(rvest)
library(polite)
library(stringr)
library(rebus)
library(lubridate)
library(xts)
library(infer)
library(parallel)
library(parallelly)
library(pracma)


## Loading our own functions
source(file.path("input", "ReseteraFunctions.R"), encoding = "utf-8")

## Importing the datasets
rst_thread_df_raw <- read_csv(file.path("data", "rst_thread_df_raw.csv"))
sentiments <- read_csv(file.path("data", "afinn.csv"))

## Setting the plot theme
rst_palette <- c("#7847B5", "#8952CD", "#FEF9FE", "#9F75DB", "#8B70B4")

theme_set(
  theme(panel.background = element_rect(fill = rst_palette[3]),
        panel.grid.major.x = element_line(colour = rst_palette[5]),
        panel.grid.major.y = element_blank())
)

# First exploration ====

## We got info on thread posts, the date of posting (from year 'till minute), the page
## and the title of the thread.
head(rst_thread_df_raw)

## A total of 32554 posts have been recorded. Dates range between 2018 until 2022.
## The store's launch date was on December 6th in 2018.
summary(rst_thread_df_raw)

## Having another column that only register just years and months could be interesting
rst_thread_df <- rst_thread_df_raw %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         # Also, Page column should be of integer type
         Page = as.integer(Page)
  )

## In the Post column, some posts are completely empty (this is probably due to
## advertising). We should filter them out
rst_thread_df <- rst_thread_df %>% filter(!str_detect(Post, "^$"))

## Because of the searching method that we've used, thread duplicates
## could've been generated

## A total of 703 rows are supposedly duplicated
duplicated.data.frame(rst_thread_df)

## Frequencies of threads and pages, in descending order
rst_thread_df %>%
  count(Thread_title, Page, Date, Post) %>%
  arrange(desc(n)) %>% 
  # Just show posts that have been repeated at least twice
  filter(n > 1) %>% 
  # Which threads are duplicated?
  distinct(Thread_title)

## We remove duplicated with selecting the first element of a combination of
## four different factors

rst_thread_df <- rst_thread_df %>%
  group_by(Thread_title, Page, Date, Post) %>%
  slice(1) %>% 
  ungroup()

## There's no duplicates anymore!
duplicated.data.frame(rst_thread_df)


## Also, some posts have strange words, that are related to the posting of
## images. These phrases start with "{ \"lightbox_close\" and end with 
## \"Toggle sidebar\" }

## Create pattern to remove those strange words
lightbox_pattern <- '\\{ \"lightbox_close\".+\"Toggle sidebar\" \\}'
click_pattern <- 'Click to expand... Click to shrink...'

rst_thread_df <- rst_thread_df %>%
  mutate(Post = Post %>%
           str_remove(lightbox_pattern) %>%
           str_remove_all(click_pattern) %>% 
           # Trim whitespaces from both sides and inside
           str_squish()) %>% 
  ## This leads to new empty posts, so we remove them again
  filter(!str_detect(Post, "^$"))  

# Some threads present NA pages: This is because they only have one page.
## Substitute NA's with the number 1
rst_thread_df[is.na(rst_thread_df$Page),]$Page <- 1



## Tidytext tokenization ====

# Let's tokenize for separate words
rst_tkn_word_df <- rst_thread_df %>%
  unnest_tokens(Word, Post, drop = FALSE)

# rst_tkn_word_df <- rst_thread_df %>%
# unnest_tokens(Word, Post,drop = FALSE)



## Because tokenizing counts articles, prepositions, etc... We've got to remove them
## using a stopword lexicon

# Remove stopwords
rst_tkn_word_df <- rst_tkn_word_df %>%
  anti_join(get_stopwords(), by = c("Word" = "word"))



# Load the sentiments data frame
# sentiments_df lacks important works like "don't like"
sentiments_df <- get_sentiments("afinn") %>%
  bind_rows(data.frame(word = "don't like", value = -2))


# With the AFINN sentiment lexicon, we can classify words by its sentiments
rst_tkn_word_afinn <- rst_tkn_word_df %>%
  inner_join(sentiments_df, by = c("Word" = "word")) %>% 
  # A general classifier for positives and negatives could be useful
  mutate(sentiment = ifelse(value > 0, "positive", "negative"))


## EDA ====

# Most common words overall
rst_tkn_word_df %>% 
  count(Word) %>% 
  arrange(desc(n))

# A wordcloud for most common words
rst_tkn_word_df %>% 
  count(Word) %>% 
  with(wordcloud(Word, n, max.words = 50))

# Most common positive words
rst_tkn_word_afinn %>%
  filter(value > 0) %>% 
  count(Word) %>% 
  arrange(desc(n))

# Most common negative words
rst_tkn_word_afinn %>%
  filter(value < 0) %>% 
  count(Word) %>% 
  arrange(desc(n))

# Wordcloud of positive and negative
rst_tkn_word_afinn %>%
  count(Word, Sentiment, sort = TRUE) %>%
  acast(Word ~ Sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

# Frequency bars for positive and negative sentiments
rst_tkn_word_afinn %>% 
  count(Sentiment, Word) %>% 
  group_by(Sentiment) %>% 
  top_n(10) %>% 
  ggplot(aes(x = fct_reorder(Word, n), y = n)) +
  geom_bar(stat = "identity", fill = rst_palette[4]) +
  facet_wrap(~ Sentiment, scale = "free_y") +
  coord_flip()

# What's the mean sentiment of the entire dataset?
rst_tkn_word_afinn %>% 
  summarise(Mean_sentiment = mean(Value))

# Mean sentiment by groups of month and year
rst_tkn_word_afinn %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_word_sentiment = mean(Value)) %>% 
  mutate(year_month = paste0(Year, "-", Month) %>% ym()) %>% 
  ggplot(aes(x = year_month, y = Mean_word_sentiment)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_date(date_breaks = "6 months") +
  labs(title = "Mean sentiment of words by month and year") +
  theme(axis.text.x = element_text(angle = 0))

# Mean word sentiment by thread

## Top 10 most positive threads by words
rst_tkn_word_afinn %>% 
  group_by(Thread_title) %>% 
  summarise(Mean_sentiment = mean(Value)) %>% 
  arrange(desc(Mean_sentiment))


# Ahora tendríamos que relacionar las palabras con alguno de los lexicones
reset_nrc <- rst_tkn_word_df %>% inner_join(get_sentiments("nrc"), by = c("Word" = "word"))
reset_bing <- rst_tkn_word_df %>% inner_join(get_sentiments("bing"), by = c("Word" = "word"))
reset_afinn <- rst_tkn_word_df %>% inner_join(get_sentiments("afinn"), by = c("Word" = "word"))

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


# ¿Las palabras más populares según puntuación? 
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



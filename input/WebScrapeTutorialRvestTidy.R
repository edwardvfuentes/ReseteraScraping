library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)

##Importacion datos ====
url <- 'http://www.trustpilot.com/review/www.amazon.com'
url_176 <- 'http://www.trustpilot.com/review/www.amazon.com?page=176'
amazon_xml <- read_html(url)
ultima_pag <- read_html(url_176)



get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.pagination-page') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}
get_reviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.review-info__body__text') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()
}
get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.consumer-info__details__name') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}
get_review_dates <- function(html){
  
  status <- html %>% 
    html_nodes('time') %>% 
    # The status information is this time a tag attribute
    html_attrs() %>%             
    # Extract the second element
    map(2) %>%                    
    unlist() 
  
  dates <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>% 
    map(1) %>% 
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%                 
    unlist()
  
  # Combine the status and the date information to filter one via the other
  return_dates <- tibble(status = status, dates = dates) %>%   
    # Only these are actual reviews
    filter(status == 'ndate') %>%              
    # Select and convert to vector
    pull(dates) %>%                            
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00') 
  
  # The lengths still occasionally do not lign up. You then arbitrarily crop the dates to fit
  # This can cause data imperfections, however reviews on one page are generally close in time)
  
  length_reviews <- length(get_reviews(html))
  
  return_reviews <- if (length(return_dates)> length_reviews){
    return_dates[1:length_reviews]
  } else{
    return_dates
  }
  return_reviews
}
get_star_rating <- function(html){
  
  stars <- html %>% html_nodes('.star-rating') %>% html_attrs() %>%
    map(str_match, pattern = capture(DIGIT)) %>% map(1) %>% unlist()
 
  #Las dos primeras puntuaciones no son parte de las puntuaciones de los
  #clientes, sino la puntuación general de la empresa y una opción para el
  #usuario de poner una puntuación.
  stars <- stars[c(-1, -2)]
  return(stars)
   
}

get_data_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  dates <- get_review_dates(html)
  ratings <- get_star_rating(html)
  
  # Combine into a tibble
  combined_data <- tibble(reviewer = reviewer_names,
                          date = dates,
                          rating = ratings,
                          review = reviews) 
  
  # Tag the individual data with the company name
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company, reviewer, date, rating, review)
}
get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html, company_name)
}

scrape_write_table <- function(url, company_name){
  
  # Read first page
  first_page <- read_html(url)
  
  # Extract the number of pages that have to be queried
  latest_page_number <- get_last_page(first_page)
  
  # Generate the target URLs
  list_of_pages <- str_c(url, '?page=', 1:latest_page_number)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    write_tsv(str_c(company_name,'.tsv'))     
}

latest_page_number <- get_last_page(amazon_xml)

list_of_pages <- str_c(url, '?page=', 1:latest_page_number)

html_nodes(amazon_xml, '.consumer-info__details__name') %>% html_text() %>% str_trim()


estado <- amazon_xml %>% 
  html_nodes('time') %>% 
  # The status information is this time a tag attribute
  html_attrs() %>%             
  # Extract the second element
  map(2) %>%                    
  unlist()

fechas <- amazon_xml %>% 
  html_nodes('time') %>% 
  # The status information is this time a tag attribute
  html_attrs() %>%             
  # Extract the second element
  map(1) %>%
  ymd_hms() %>% 
  unlist()

tibble(status = estado, dates = fechas) %>% filter(status == "ndate") %>% pull(dates)

html_nodes(amazon_xml, css = ".star-rating") %>% html_attrs()

paterno <- 'count-' %R% capture(DIGIT)


#Mazo info de amazon
scrape_write_table(url, "Amazon")

##Sección Inferencia ====

library(xts)
library(infer)

fb_url <- 'https://www.trustpilot.com/review/www.facebook.com'

facebook_table <- scrape_write_table(fb_url, "Facebook")

amazon_table <- read_tsv("Amazon.tsv")
facebook_table <- read_tsv("Facebook.tsv")



#Series temporales
amazon_ts <- xts(amazon_table$rating, amazon_table$date)
colnames(amazon_ts) <- "rating"

facebook_ts <- xts(facebook_table$rating, facebook_table$date)
colnames(facebook_ts) <- "rating"

intervalo_abierto <- '2016-01-01/'

amazon_sts <- amazon_ts[intervalo_abierto]
facebook_sts <- facebook_ts[intervalo_abierto]

apply.monthly(amazon_sts, colMeans, na.rm = T)

apply.monthly(amazon_sts, FUN = length)
apply.monthly(facebook_sts, FUN = length)






amazon_sts %>% apply.monthly(mean, na.rm = T) %>% fortify.zoo() %>% pull(Index)


#¿paso de los datos a un dataframe para un ggplot?
amazon_sts_df <- amazon_sts %>% apply.monthly(mean, na.rm = T) %>% fortify.zoo()
amazon_sts_df$Index <- as_date(amazon_sts_df$Index)


facebook_sts_df <- facebook_sts %>% apply.monthly(mean, na.rm = T) %>% fortify.zoo()
facebook_sts_df$Index <- as_date(facebook_sts_df$Index)

amazon_sts_df %>% ggplot(aes(x = Index, y = rating)) +
  geom_line() +
  labs(title = "Rating de Amazon (media mensual), 2016-2018") +
  scale_x_date(date_breaks = "6 months")

facebook_sts_df %>% ggplot(aes(x = Index, y = rating)) +
  geom_line() +
  labs(title = "Rating de facebook (media mensual), 2016-2018") +
  scale_x_date(date_breaks = "6 months")




#Movidote
#Modifiquemos el date de tabla_completa para liarla parda
tabla_completa <- rbind(amazon_table, facebook_table)



tabla_completa <- tabla_completa %>% filter(date >= intervalo_abierto) %>% 
  mutate(weekday = weekdays(date, abbreviate = T),
         hour = hour(date))

tabla_completa$weekday <- factor(tabla_completa$weekday,
                                 levels = c("lu.", "ma.", "mi.", "ju.", "vi.", "sá.", "do."))


tabla_completa %>% count(company, weekday) %>%
  ggplot(aes(x = weekday, y = n)) + geom_bar(stat = "identity") + facet_wrap( ~ company)

tabla_completa %>% count(company, hour) %>%
  ggplot(aes(x = hour, y = n)) + geom_bar(stat = "identity") + facet_wrap( ~ company)


#Test de medias de dias laborales frente a findesemana en Amazon y Facebook

hipotesis_tabla <- tabla_completa %>% 
  mutate(es_finde = ifelse(weekday %in% c("sá.", "do."), 1, 0)) %>% 
  select(company, es_finde, rating)

hipotesis_tabla %>% 
  group_by(company, es_finde) %>% 
  summarise(Media_aprox = mean(rating)) %>% 
  spread(es_finde, Media_aprox) %>% 
  rename(laboral = '0', finde = '1')

labor_rating <- hipotesis_tabla %>%
  filter(company == "Amazon") %>% 
           filter(es_finde == 0) %>% 
  summarise(mean(rating)) %>% pull()

finde_rating <- hipotesis_tabla %>%
  filter(company == "Amazon") %>% 
  filter(es_finde == 1) %>% 
  summarise(mean(rating)) %>% pull()  
  
  
diff_ratings <- labor_rating - finde_rating
  
  
#Ahora permutaciones con el paquete infer

hipotesis_tabla$es_finde <- as.factor(hipotesis_tabla$es_finde)

permutacion_test <- hipotesis_tabla %>%
  filter(company == "Amazon") %>% 
  specify(rating ~ es_finde) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 10000, type = 'permute') %>% 
  calculate(stat = 'diff in means', order = c(0, 1))


permutacion_test %>% summarise(p = mean(abs(stat) >= diff_ratings))
  
  


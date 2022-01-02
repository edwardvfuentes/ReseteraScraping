
library(tidyverse)
library(tidytext)
library(polite)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(xts)
library(infer)


extracto_texto <- readRDS("posts_example.rds")

rm_citas_2 <- function(posts){
  
 # start_quote <- "\n+[\\w+ ]+said:"
  start_quote <- "\\w+ said:"
  end_quote <- "Click to shrink...[\n]+"
  
  has_cita <- which(str_detect(posts, end_quote))
  
  for(i in has_cita){
    citas_rango <- cbind(str_locate_all(posts[i], start_quote)[[1]][, 1] - 1 ,
                         str_locate_all(posts[i], end_quote)[[1]][, 2])
    
    citas_rango_final <- c(0,
                           citas_rango %>% t() %>% as.numeric(),
                           str_length(posts[i]))%>% matrix(byrow = T,
                                                           ncol = 2)
    
    extracto <- posts[i] %>% str_sub(citas_rango_final) %>% str_trim()
    extracto <- extracto[str_length(extracto) > 0]
    
    posts[i] <- extracto %>% str_c(collapse = " ")
    
  }
  
  return(posts)
  
}


rm_citas_2(extracto_texto)
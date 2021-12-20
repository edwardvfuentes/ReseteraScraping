library(tidyverse)
library(tidytext)
library(polite)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(xts)
library(infer)


# We first politely connect to the webpage
rst_session <- bow("https://www.resetera.com/threads/halo-infinite-ot-comeback-evolved.514647/page-324")

# Then we scrape it
rst_xml <- scrape(rst_session, query = list())

# We are interested in extracting text from posts within the forum.
# These messages are contained inside a class called "bbWrapper"

extracto_texto <- rst_xml %>%
  html_elements(".bbWrapper") %>%
  html_text(trim = TRUE)



"bbCodeBlock bbCodeBlock--expandable bbCodeBlock--quote js-expandWatch is-expandable"



rst_xml %>%
  html_elements(".bbWrapper :not(bbCodeBlock) not:(bbCodeBlock--expandable) :not(bbCodeBlock--quote) :not(js-expandWatch) :not(is-expandable) :not(is-expanded)")


(rst_xml %>%
    html_elements(".bbWrapper"))[2] %>%
  html_elements(":not(.bbCodeBlock-expandContent)
                 :not(.bbCodeBlock-content)
                 :not(.bbCodeBlock--expandable)
                 :not(.bbCodeBlock)
                
                ")

ejemplo <- (rst_xml %>%
              html_elements(".bbWrapper"))[22]

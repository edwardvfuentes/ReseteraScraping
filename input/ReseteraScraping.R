
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

## Connecting to Google ====

# We intend to extract resetera urls through Google, at least for 10 different
# search pages

## Base url
ggle_rst_base <- "https://www.google.com/search?q=resetera+epic+games+store+site:www.resetera.com&"

## Then, we create a list for 10 search pages adding a pattern to ggle_rst_base
ggle_rst_urls <- paste0(ggle_rst_base, "start=", seq(0, 90, by = 10), "&")


# Let's save google xmls in a list
ggle_cookies_list <- map(ggle_rst_urls, read_html)


# We need to get past cookies
ggle_xml_list <- ggle_cookies_list %>% 
  map(html_form) %>% 
  map(1) %>% 
  map(html_form_submit) %>% 
  map(read_html)


# Retrieve text that include URLs, then extract those URLs
# with a specific function

rst_thread_urls <- ggle_xml_list %>%
  map(function(x) 
  {
    html_elements(x, "a") %>%
      as.character() %>%
      # Extract only urls
      extract_google_url() %>% 
      # Filter out URLs that are not related to Resetera
      str_subset("^((?!google.com).)*$")
  }
  ) %>% 
  # We unlist this for clarity
  unlist()

# Some thread urls don't begin in the first page of that thread. Therefore, we
# need to rewrite some of the urls
rst_thread_urls <- rst_thread_urls %>%
  str_remove("page-\\d+")

# First, in order to construct adequate loops for reach thread, we need to 
# know the last page for every thread

tic()

rst_thread_last <- rst_thread_urls %>% 
  map(bow) %>% 
  map(scrape) %>% 
  map(ultima_pag) %>% 
  map(as.numeric)

toc()

# If there's only one page, fill those spaces with a number one
rst_thread_last[map_lgl(rst_thread_last, function(x) length(x) == 0)] <- 1
    

# We'll create a list with every page for every thread
rst_thread_urls_full <- rst_thread_urls %>%
  map2(rst_thread_last, function(x, y) paste0(x, "page-", 1:y)) %>% 
  unlist()


# Extracting text messages will be a computationally expensive task, so we are using
# parallel processing for it.

clusterino <- makeClusterPSOCK(3)

clusterExport(clusterino, c('extract_rst_text', 'rm_quotes', 'ultima_pag', 'extract_google_url'))

clusterEvalQ(clusterino, library(tidyverse))
clusterEvalQ(clusterino, library(tidytext))
clusterEvalQ(clusterino, library(rvest))
clusterEvalQ(clusterino, library(lubridate))
clusterEvalQ(clusterino, library(polite))

tic()

rst_thread_extracts <- parLapplyLB(clusterino, rst_thread_urls_full, fun = function(x)
  
  {
    extract_rst_text(scrape(bow(x))) 
  }

)

elapsed_cluster_time <- toc()

stopCluster(clusterino)

# Merge all the elements from the list into one dataframe
rst_thread_df_raw <- bind_rows(rst_thread_extracts)

# Write the dataset into a csv file for future imports
write_csv(rst_thread_df_raw, "data/rst_thread_df_raw.csv")



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
           str_squish) %>% 
  ## This leads to new empty posts, so we remove them again
  filter(!str_detect(Post, "^$"))  

# Some threads present NA pages: This is because they only have one page.
## Substitute NA's with the number 1
rst_thread_df[is.na(rst_thread_df$Page),]$Page <- 1


# Write the dataset into a csv file for future imports
## write_csv(rst_thread_df, "data/rst_thread_df.csv")

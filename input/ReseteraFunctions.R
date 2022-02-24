### Functions for scraping resetera


# For removing quotes inside posts in order to avoid duplicated messages
rm_quotes <- function(posts){
  
  start_quote <- "[\\w+ ]+ said:"
  end_quote <- "Click to shrink\\.\\.\\."
  
  clean_text <- str_remove(posts,
                           paste0(start_quote, "(?:.|\n)+",
                                  end_quote))
  
  return(clean_text)
  
}


# For detecting the last page in a thread, so we can establish an ending
# for the extracting loops


ultima_pag <- function(xml) {
  if (!class(xml)[1] == "xml_document") {
    stop("Error: Object is not from the xml_document class")
  }
  
  last_page <-
    xml %>%
    html_elements(".pageNav-page") %>%
    html_text2() %>%
    last()
  
  return(last_page)
}


# For having a list of every pertinent URL found by a google search
extract_google_url <- function(text) {
  start_pattern <- "(?<=q=)htt[ps]"
  end_pattern <- "(?=&amp)"
  
  extracted_url <- str_extract(text, paste0(start_pattern, "(.+?)", end_pattern))
  
  return(extracted_url)
}


# For having a detailed data frame of every post inside a Resetera thread
# and their date of writing
extract_rst_text <- function(xml) {
  if (!class(xml)[1] == "xml_document") {
    stop("Error: Object is not from the xml_document class")
  }
  
  # The thread title portion
  rst_title <- xml %>% 
    html_elements(".p-title-value") %>%
    html_text2()
  
  # The thread page portion
  rst_page <- xml %>%
    html_elements(".pageNav-page--current") %>%
    html_text() %>%
    first()
  
  # The text portion
  rst_text <- xml %>%
    html_elements(".bbWrapper") %>%
    html_text2() %>%
    ## Remove quotes
    rm_quotes() %>%
    ## Remove \n
    str_replace_all("\n", " ") %>% 
    ## Trim spaces
    str_trim()
  
  # The date portion
  rst_dates <- xml %>%
    html_elements("[data-lb-caption-desc]") %>%
    html_attr("data-lb-caption-desc") %>%
    str_extract("(?<=· ).+") %>%
    mdy_hm()
  
  # All together in one data frame
  rst_frame <- tibble(
    Thread_title = rst_title,
    Page = rst_page,
    Date = rst_dates,
    Post = rst_text
  )
  
  return(rst_frame)
}




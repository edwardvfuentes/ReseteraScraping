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
ultima_pag <- function(url) {
  
  pag_xml <- read_html(url)
  
  pagina <- html_nodes(pag_xml, ".pageNavLinkGroup") %>%
    html_text() %>%
    str_squish()
  
  pagina <- pagina[1]
  
  last_pagina <- pagina %>%
    str_extract(one_or_more(DGT) %R% SPC %R% ANY_CHAR %R% END) %>%
    str_extract(one_or_more(DGT))
  return(last_pagina)
  
}


# For having a list of every pertinent URL found by a google search
extract_google_url <- function(url) {
  start_pattern <- "(?<=q=)htt[ps]"
  end_pattern <- "(?=&amp)"
  
  extracted_url <- str_extract(url, paste0(start_pattern, "(.+?)", end_pattern))
  
  return(extracted_url)
}


# For having a detailed data frame of every post inside a Resetera thread
# and their date of writing
extract_rst_text <- function(read_xml) {
  if (!class(read_xml)[1] == "xml_document") {
    stop("Error: Object is not from the xml_document class")
  }
  
  # The text portion
  rst_text <- read_xml %>%
    html_elements(".bbWrapper") %>%
    html_text2() %>%
    ## Remove quotes
    rm_quotes() %>%
    ## Remove \n
    str_replace_all("\n", " ")
  
  # The date portion
  rst_dates <- read_xml %>%
    html_elements("[data-lb-caption-desc]") %>%
    html_attr("data-lb-caption-desc") %>%
    str_extract("(?<=· ).+") %>%
    mdy_hm()
  
  # All together in one data frame
  rst_frame <- tibble(
    Date = rst_dates,
    Post = rst_text
  )
  
  return(rst_frame)
}


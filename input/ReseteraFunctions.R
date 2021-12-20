# Funciones resetera

rm_citas <- function(posts){
  
  start_quote <- "\n+[\\w+ ]+said:"
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

ultima_pag <- function(url) {
  # La última página siempre va a provenir
  pag_xml <- read_html(url)
  pagina <- html_nodes(pag_xml, ".pageNavLinkGroup") %>% html_text() %>% str_squish()
  pagina <- pagina[1]
  last_pagina <- pagina %>%
    str_extract(one_or_more(DGT) %R% SPC %R% ANY_CHAR %R% END) %>%
    str_extract(one_or_more(DGT))
  return(last_pagina)
  
}

extract_posts <- function(url_base){
  
  # Recopilamos todas las urls del hilo del foro seleccionado
  last_pagina <- ultima_pag(url_base)
  paginas <- 2:last_pagina
  urls_reset <- str_c(url_base, "page-", paginas)
  
  # Debemos extraer un tibble con información textual por cada página
  
  list_tablas <- vector("list", length(urls_reset))
  
  for(i in 2:length(urls_reset)){
    
    url_xml <- read_html(urls_reset[i])
    
    # Extracción mensajes
    msg_content <- url_xml %>%
      html_nodes(".messageText.SelectQuoteContainer.ugc.baseHtml") %>%
      html_text() %>% str_squish() %>% rm_citas()
    
    # Extracción información temporal
    # msg_time <- url_xml %>% html_nodes(".datePermalink") %>%
    #   html_nodes(".DateTime") %>%
    #   html_attrs() %>% map(2) %>% unlist() %>% mdy_hm()
    
    time_info <- url_xml %>%
      html_nodes(".datePermalink") %>%
      html_nodes(".DateTime") %>% html_attrs()
    
    msg_time <- str_c(time_info %>% map(4),
                      time_info %>% map(5), sep = " ") %>% mdy_hm()
    
    # Devolver tibble que aglutine todo
    list_tablas[[i]] <- tibble(Fecha = msg_time, Texto = msg_content)
    
  }
  return(list_tablas)
}





# Load library ------------------------------------------------------------

library(rvest)
library(tidyverse)
library(stringr)


n_pages <- 100
getUrl <- function(x){
  paste0('https://www.avito.ru/moskva/odezhda_obuv_aksessuary/zhenskaya_odezhda?p=', x, '&sgtd=21&q=шуба+норковая')
}
urls <- map_chr(1:n_pages, getUrl)  

coat_urls <- vector('character', 0)
for(i in seq_along(urls)){
  if(i %% 5 == 0){
    cat(i, 'страниц пропарсили', '\n')
  }
  Sys.sleep(2)
  ur <- read_html(urls[i]) %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    discard(!str_detect(., '/moskva/odezhda_obuv_aksessuary/(shu|nor)')) %>% 
    unique()
  coat_urls <- c(coat_urls, ur)
}
urlsParse <- paste0('https://www.avito.ru', coat_urls)

# Great tibble --------------------------------------------------

coat <- tibble(
  price = numeric(0),
  size = numeric(0),
  description = character(0),
  img_url = character(0),
  file_location = character(0),
  id = numeric(0)
)


# Functions ---------------------------------

get_price <- function(x){
  read_html(urlsParse[x]) %>% 
  html_nodes('div') %>% 
  html_nodes('span') %>%
  html_text() %>% 
  discard(!str_detect(., '\\n.+')) %>% 
  last() %>% 
  str_extract_all(., "[0-9]+") %>% 
  map(function(x) paste0(x[1], x[2])) %>% 
  as.numeric()
}

get_size <- function(x){
  size <- read_html(urlsParse[x]) %>% 
    html_nodes('div') %>% 
    html_nodes('span') %>%
    html_text() %>% 
    discard(!str_detect(., 'Размер: [0-9]+')) %>% 
    str_extract(., '[0-9]+') %>% 
    as.numeric()
  if(length(size) == 0){
    size <- 1
  }
  return(size)
}

get_description <- function(x){
  decs <- read_html(urlsParse[x]) %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    discard(str_detect(., '\\n.+')) %>% 
    .[1]
  if(is.na(decs)){
    decs <- read_html(urlsParse[3]) %>% 
      html_nodes('h1') %>% 
      html_text() %>% 
      str_trim(.)
  }
  return(decs)
}

get_imgurl <- function(x){
  read_html(urlsParse[x]) %>%
  html_node('img') %>% 
  html_attr('src') %>% 
  paste0('https:', .)
}

get_fotoname <- function(x) {
    str_c(getwd(), '/foto_coat/', x, ".jpg")
}

downloadImage <- function(get_imgurl, get_fotoname) {
  tryCatch(
    download.file(get_imgurl, get_fotoname, mode = "wb", quiet = TRUE),
    error = function(e) return(1)
  )
}

if(!dir.exists('foto_coat')){
  dir.create('foto_coat')
}

getAll <- function(x) {
  
  Sys.sleep(3)
  
  price <- get_price(x)
  
  size <- get_size(x)
  
  description <- get_description(x)
  
  img_url <- get_imgurl(x)
  
  id_touse <- if (is_empty(coat$id)) {
    1:length(img_url)
  } else {
    (tail(coat$id, 1) + 1):(length(img_url) + tail(coat$id, 1))
  }
  
  coat_df <- tibble(price = price, size = size, 
                    description = description, img_url = img_url) %>% 
    mutate(id = id_touse,
           file_location = map_chr(id, get_fotoname),
           not_downloaded = map2_dbl(img_url, file_location, downloadImage))
    
  coat <<- rbind.data.frame(coat, coat_df)
}

walk(1:length(urlsParse), getAll)

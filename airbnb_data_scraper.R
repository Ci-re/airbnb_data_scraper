setwd("../Documents/airbnb_data_scraper/")
library(rvest)
library(httr)
library(RSelenium)
library(tidyverse)
library(wdman)
library(xml2)
library(magrittr)

CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}
CatchupPause(1)
driver$goBack()
rD <- rsDriver(browser = "firefox", port = 4101L, chromever = "109.0.5414.25")
driver <- rD[["client"]]
root_url <- "https://www.airbnb.com/s/Aruba/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&flexible_trip_lengths%5B%5D=one_week&price_filter_input_type=0&price_filter_num_nights=5&source=structured_search_input_header&search_type=search_query"
driver$navigate(root_url)

top_url <- "https://www.airbnb.com"
data_set <- data.frame()
data_collected_total <- data.frame()

# driver$goBack()
for(i in 8:15){
  print(i)
  
  pagesource_home <- driver$getPageSource()
  aru_session_parsed <- read_html(pagesource_home[[1]])
  
  next_link <- aru_session_parsed %>% 
    html_element("a._1bfat5l") %>% 
    html_attr("href")
  next_link <- paste0(top_url, next_link)
  # driver$navigate(next_link)
  cards <- aru_session_parsed |> 
    html_elements("div.c4mnd7m")
  for(k in 1:length(cards)){
    print(k)
    source_link <- cards[[k]] |> 
      html_element("a") |> 
      html_attr("href")
    
    source_link <- paste0(top_url, source_link)
    
    type <- cards[[k]] |> 
      html_elements("div.t1jojoys") |> 
      html_text() |> 
      stringr::str_replace_all(pattern = "[\n, \t]", replacement = " ") |> 
      str_trim("both")
    
    reviews <- cards[[k]] %>% 
      html_elements("span.r1dxllyb") %>% 
      html_text() %>% 
      stringr::str_replace_all(pattern = "[\n,\t]", replacement = "") |> 
      str_trim("both")
    
    if(identical(reviews, character(0))){
      reviews <- NA
    }else {
      reviews <- reviews
    }
    
    title_parent <- cards[[k]] |> 
      html_elements("span.t6mzqp7") |> 
      html_text()
    
    status <- cards[[k]] |> 
      html_elements("div.t1mwk1n0") |> 
      html_text()
    
    if(identical(status, character(0))){
      status <- NA
    }
    
    # price <- cards[[k]] |> 
    #   html_elements("div._1jo4hgw") |> 
    #   html_text()
    
    # source_link <- paste0(top_url,urls[k])
    driver$navigate(source_link)
    Sys.sleep(3)
    scroll_d <- driver$findElement(using = "css", value = "body")
    scroll_d$sendKeysToElement(list(key = "end"))
    pagesource2 <- driver$getPageSource()[[1]]
    Sys.sleep(2)
    pagesource3 <- read_html(pagesource2)
    Sys.sleep(2)
    
    props <- pagesource3 %>% 
      html_elements("div._tqmy57") %>% 
      html_elements("ol.lgx66tx>li.l7n4lsf>span") %>% 
      html_text() %>% 
      as.list()
    
    price <- pagesource3 |> 
      html_elements("span._14y1gc") |> 
      extract(1) |> 
      html_element("span._1y74zjx") |> 
      html_text() |> 
      str_trim()
    
    if(!identical(price,character(0))){
      if(is.na(price)){
        price <- pagesource3 |> 
          html_elements("span._tyxjp1") |> 
          extract(1) |> 
          html_text() |> 
          str_trim()
      }
    }
    
    while(identical(price, character(0))){
      pagesource2 <- driver$getPageSource()[[1]]
      Sys.sleep(2)
      pagesource3 <- read_html(pagesource2)
      price <- pagesource3 |> 
        html_elements("span._14y1gc") |> 
        extract(1) |> 
        html_element("span._1y74zjx") |> 
        html_text() |> 
        str_trim()
      
      if(!identical(price, character(0))){
        if(is.na(price)){
          price <- pagesource3 |> 
            html_elements("span._tyxjp1") |> 
            extract(1) |> 
            html_text() |> 
            str_trim()
        }
        break;  
      } 
    }
    while(is.null(unlist(props[1]))){
      pagesource2 <- driver$getPageSource()[[1]]
      Sys.sleep(2)
      pagesource3 <- read_html(pagesource2)
      props <- pagesource3 %>% 
        html_elements("div._tqmy57") %>% 
        html_elements("ol.lgx66tx>li.l7n4lsf>span") %>% 
        html_text() %>% 
        as.list()
      
      if(!is.null(unlist(props[1]))){
        break;  
      } 
    }
    
    # price <- pagesource3 |> 
    #   html_elements("span._14y1gc") |> 
    #   extract(1) |> 
    #   html_element("span._1y74zjx") |> 
    #   html_text() |> 
    #   str_trim()
    
    location <- pagesource3 %>% 
      html_nodes("span._9xiloll") %>% 
      html_text()
    
    host <- pagesource3 %>% 
      html_nodes("div._cv5qq4") %>% 
      html_text()
    
    props <- props[!str_detect(string = props, pattern = " . ")] %>% unlist()
    props <- str_split(props, " ")
    var <- c() 
    val <- c() 
    for(n in 1:length(props)){
      var[length(var) + 1] <- props[[n]][2]
      val[length(val) + 1] <- props[[n]][1]
    }
    
    pool_check <- pagesource3 %>% 
      html_nodes("div._1byskwn") %>% 
      html_elements("div") %>% 
      html_text() %>% unique()
    
    pool_check <- pool_check[str_detect(pool_check, "pool|Pool")][1]
    if(is.na(pool_check)){
      pool_check <- "no"
    } else {
      pool_check <- "yes"
    }
    
    data_set <- data.frame(variables = var, values = val)
    
    data_set <- data_set %>% mutate(variables = if_else(variables == "baths", "bath", variables)) %>% 
      mutate(variables = if_else(variables == "guests", "guest", variables)) %>% 
      mutate(variables = if_else(variables == "bedrooms", "bedroom", variables)) %>% 
      mutate(variables = if_else(variables == "beds", "bed", variables)) %>% 
      mutate(variables = if_else(variables == "bathroom", "bath", variables))
    
    data_set <- data_set |> add_row(
      variables = c("property_name", "type", "host", "pool", "location", "reviews", "price","status", "source"),
      values = c(title_parent, type, host, pool_check, location, reviews, price, status, source_link)
    )
    
    data_set <- data_set %>% group_by(variables) %>% 
      pivot_wider(
        id_cols = everything(), names_from = "variables", values_from = "values"
      )
    data_set <- data_set[1,]
    data_collected_total <- bind_rows(data_collected_total, data_set)
    Sys.sleep(2)
  }
  
  driver$navigate(next_link)
  Sys.sleep(5)
}

datr <- data_collected_total

dat <- data_collected_total[!duplicated(data_collected_total$property_name),]

names(dat)
dim(dat)

real_dat2 <- bind_rows(real_dat2, final_data3)
write.csv(x = dat, file = "../Desktop/February/Week2/airbnb_data_week3.csv")

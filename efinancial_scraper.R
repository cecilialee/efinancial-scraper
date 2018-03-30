library(tidyverse)
library(rvest)
library(plyr)

scrape_page <- function(p) {
  
  print(paste("Scraping page", p))
  
  base_url <- "https://www.efinancialcareers.com/search?keywords=Data+Scientist&locationsId=1819730&locationsName=Hong+Kong&page=%s"
  url <- sprintf(base_url, p)
  
  page <- read_html(url)
  
  job_title <- html_nodes(page, ".jobPreview h3") %>% 
    html_text() %>% 
    str_replace("\\r\\n[[:space:]]+", "")
  
  job_description <- html_nodes(page, ".description") %>% 
    html_text() %>% 
    str_replace("\\r\\n[[:space:]]+", "")
  
  salary <- html_nodes(page, ".salary span") %>%
    html_text()
  
  location <- html_nodes(page, ".location span") %>% 
    html_text()
  
  position <- html_nodes(page, ".position span span") %>% 
    html_text()
  
  company <- html_nodes(page, ".company span") %>%
    html_text() %>% 
    str_replace_all("\\r\\n", "") %>% 
    str_trim(side = "both")
  company <- company[!grepl("[0-9]{2}[A-Z]?[0-9]{4}", company)]
  
  date <- html_nodes(page, ".updated span") %>% 
    html_text() %>% 
    str_replace("\\r\\n[[:space:]]+", "")
  
  job_ads <- data.frame(job_title, job_description, salary, location, position, company, date)
  
  return(job_ads)
}

data <- lapply(seq(26), scrape_page)

data <- ldply(data, data.frame)

write_csv(data, "efinancial_031818.csv")

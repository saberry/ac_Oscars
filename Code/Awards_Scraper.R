#CRITICS
#Golden Globes
#Critics' Choice Movie Awards
#Chicago Film Critics Association awards
#Satellite Awards
#National Board of Review awards
#New York Film Critics Circle awards
#Los Angeles Film Critics Circle awards.

#INSIDERS
#Directors Guild of America
#Producers Guild of America
#Screen Actors Guild
#Writers Guild of America
#British Academy of Film and Television Arts (BAFTA)
#American Cinema Editors.

library(tidyverse)
library(rvest)
library(RSelenium)

setwd("C:\\Users\\peter\\Google Drive\\Business Analytics Club\\Oscars Predictions")

award_show_list <- read_csv("award_show_list.csv")

driver <- rsDriver(browser = c("chrome"),chromever ="79.0.3945.36", port = 4444L)
remDr <- driver[["client"]]

awards_all <- data.frame()

for (award_show in 1:nrow(award_show_list)) {
  
  award_show_name <- award_show_list$award_show[award_show]
  award_show_url <- award_show_list$url[award_show]

  remDr$navigate(award_show_url)
  html <- remDr$getPageSource()[[1]]
  html <- read_html(html)
  
  award_years <- html %>%
    html_nodes(".event-history-widget a") %>%
    html_text() %>%
    as.numeric()
  
  award_urls <- html %>%
    html_nodes(".event-history-widget a") %>%
    html_attr("href")
  
  award_url_df <- data.frame(award_years, award_urls)
  
  ref_year <- 1
  row_num <- 1
  
  while (ref_year <= 25 & row_num <= nrow(award_url_df)) {
    
    url <- award_url_df$award_urls[row_num]
    year <- award_url_df$award_years[row_num]
    
    remDr$navigate(url)
    
    html <- remDr$getPageSource()[[1]]
    html <- read_html(html)
    
    number_of_awards <- html %>%
      html_nodes(".event-widgets__award-category") %>%
      length() - 
      html %>%
      html_nodes(".event-widgets__award-name") %>%
      length() +
      1
    
    if (number_of_awards > 1) {
      
      for (award in 1:number_of_awards) {
        
        award_name <- html %>%
          html_nodes(".event-widgets__award-category") %>%
          .[[award]] %>%
          html_nodes(".event-widgets__award-category-name") %>%
          html_text()
        
        nominees <- html %>%
          html_nodes(".event-widgets__award-category") %>%
          .[[award]] %>%
          html_nodes(".event-widgets__primary-nominees") %>%
          html_text()
        
        details <- html %>%
          html_nodes(".event-widgets__award-category") %>%
          .[[award]] %>%
          html_nodes(".event-widgets__secondary-nominees") %>%
          html_text()
        
        temp_df <- data.frame(nominees, details, stringsAsFactors = FALSE) %>%
          mutate(award_show = award_show_name,
                 year = year,
                 award = award_name,
                 winner = ifelse(row_number() == 1, 1, 0),
                 ref_year = ref_year)
        
        awards_all <- rbind(awards_all,
                            temp_df)
        
      }
      
      ref_year <- ref_year + 1
      row_num <- row_num + 1
      
    } else {
      
      row_num <- row_num + 1
      
    }
    
  }
 
}

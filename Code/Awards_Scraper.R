#Listed below are the awards included in 538's model. We'll try to scrape as many as we can from IMDb.

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

#I use the following line to set up a working drive on my Google Drive, but you can use whatever drive you like, or read/write files directly to paths you specify in subsequent lines of code.
setwd("C:\\Users\\peter\\Google Drive\\Business Analytics Club\\Oscars Predictions")

#Read in the list of award shows and their IMDb URLs.
award_show_list <- read_csv("award_show_list.csv")

#Set up the RSelenium driver. This will launch a Chrome web browser in another window.
#If you don't use Chrome, we'll need to adjust the code below to your preferred web browser.
driver <- rsDriver(browser = c("chrome"),chromever ="79.0.3945.36", port = 4445L)
remDr <- driver[["client"]]

#Create an empty data frame where we can store our awards as we scrape them.
awards_all <- data.frame()

#Loop through each award show in the award_show_list dataframe.
for (award_show in 8:nrow(award_show_list)) {
  
  #Create variables with the award show's name and URL.
  award_show_name <- award_show_list$award_show[award_show]
  award_show_url <- award_show_list$url[award_show]

  #Have RSelenium navigate to the award show's URL.
  remDr$navigate(award_show_url)
  #Extract the page's HTML (using RSelenium)
  html <- remDr$getPageSource()[[1]]
  #Use rvest to read in the HTML.
  html <- read_html(html)
  
  #Use rvest to select the award history section and extract the years, then convert them to numbers.
  award_years <- html %>%
    html_nodes(".event-history-widget a") %>%
    html_text() %>%
    as.numeric()
  
  #Use rvest to select the award history section and extract the hyperlink URL (denoted by "href") for each year.
  award_urls <- html %>%
    html_nodes(".event-history-widget a") %>%
    html_attr("href")
  
  #Create a dataframe of each year's URL by combining the years and URLs.
  award_url_df <- data.frame(award_years, award_urls, stringsAsFactors = FALSE)
  
  #Set up a "Reference Year" variable that should help us match years up later.
  ref_year <- 1
  #Set up a "Row Number" variable that will help us move through the next loop.
  row_num <- 1
  
  #Create a while loop that runs as long as the "Reference Year" is less than or equal to 25 (i.e. we're only going back 25 years)
  #OR the "Row Number" reaches the end of the list of URLs (i.e. the last year for which IMDb has a page).
  while (ref_year <= 25 & row_num <= nrow(award_url_df)) {
    
    #Set up variables for the year and the URL for that award and year
    url <- paste0("https://www.imdb.com", award_url_df$award_urls[row_num])
    year <- award_url_df$award_years[row_num]
    
    #Have RSelenium navigate to that URL.
    remDr$navigate(url)
    
    #Extract the HTML using RSelenium
    html <- remDr$getPageSource()[[1]]
    #Use rvest to read the HTML.
    html <- read_html(html)
    
    #Count the number of award categories on the page.
    number_of_awards <- html %>%
      html_nodes(".event-widgets__award-category") %>%
      length()
    
    #If there is more than one award listed on the page, proceed with the following code to scrape the page.
    #(We do this because IMDb has award pages for some years that are just blank. We use this IF statement to skip those.)
    if (number_of_awards > 1) {
      
      #Loop through each award on the page
      for (award in 1:number_of_awards) {
        
        #Use rvest to extract the name of the award ("BEST PICTURE")
        award_name <- html %>%
          html_nodes(".event-widgets__award-category") %>%
          .[[award]] %>%
          html_nodes(".event-widgets__award-category-name") %>%
          html_text()
        
        #Use rvest to extract the name of the nominee (the actor or the film)
        nominees <- html %>%
          html_nodes(".event-widgets__award-category") %>%
          .[[award]] %>%
          html_nodes(".event-widgets__primary-nominees") %>%
          html_text()
        
        #Use rvest to extract the second item listed for the nominee (for actors, the name of the film)
        details <- html %>%
          html_nodes(".event-widgets__award-category") %>%
          .[[award]] %>%
          html_nodes(".event-widgets__secondary-nominees") %>%
          html_text()
        
        #If the "Award Name" variable is blank, then the award is an honorary award, and we're not interested in it.
        #If the "Award Name" variable isn't blank, then do the following steps.
        if (length(award_name) != 0) {
        
          #Create a temporary dataframe that combines the nominees and their details with the award show's name, the award's name, the year, the "Reference Year" variable, and whether or not they won.
          #NOTE: On IMDb, the first person listed is the winner, so the ifelse statement below captures this.
          temp_df <- data.frame(nominees, details, stringsAsFactors = FALSE) %>%
            mutate(award_show = award_show_name,
                   year = year,
                   award = award_name,
                   winner = ifelse(row_number() == 1, 1, 0),
                   ref_year = ref_year)
          
          #Add the temporary dataframe to the container capturing all of the award show data.
          awards_all <- rbind(awards_all,
                              temp_df)
        
        }
        
      }
      
      #Increase the "Reference Year" and "Row Number" variables by 1 and continue the WHILE loop.
      ref_year <- ref_year + 1
      row_num <- row_num + 1
      
    } else {
      
      #Going back to the IF statement above... If there is not more than 1 award on the page, increase the "Row Number" variable by 1 and continue the WHILE loop.
      row_num <- row_num + 1
      
    }
    
  }
 
}

#Write all of the awards you've captured to a CSV.
write_csv(awards_all, "awards_all.csv")

#Create a list of the various award names across award shows. 
#This will allow us to identify which category names ("Best Actor in a Leading Role", "Best Leading Actor", etc.) map to which categories ("Best Actor").
all_award_names <- awards_all %>%
  distinct(award_show, award)

#Write that list to a CSV.
write_csv(all_award_names, "awards_mapping_start.csv")
#OUTSIDE OF R (IN EXCEL) MAP EACH CATEGORY NAME TO ITS ANALOGOUS OSCAR CATEGORY.

#Read in the mapped file and "unite" the columns into one column.
award_mapping <- read_csv("awards_mapping_finish.csv") %>%
  unite("mapped_award", best_picture, best_actor, best_supporting_actor, best_actress, best_supporting_actress, best_director,
        best_original_screenplay, best_adapted_screenplay, best_cinematography, best_production_design, best_score, best_song,
        best_foreign_film, best_animated_film, best_costume_design, best_documentary, best_editing, 
        remove = TRUE, sep = "", na.rm = TRUE)

#Create a list of the Oscar winners prior to this year.
oscar_winners <- awards_all %>%
  filter(ref_year >= 2 & award_show == "Academy Awards" & winner == 1) %>%
  inner_join(award_mapping,
             by = c("award_show", "award")) %>%
  filter(mapped_award != "") %>%
  select(ref_year, mapped_award, oscar_winner = nominees)

#Using 538's methodology, create a score for each award show and award category.
score_creation <- awards_all %>%
  filter(ref_year >= 2 & award_show != "Academy Awards") %>%
  inner_join(award_mapping,
             by = c("award_show", "award")) %>%
  filter(mapped_award != "") %>%
  inner_join(oscar_winners, 
             by = c("ref_year", "mapped_award")) %>%
  select(ref_year, mapped_award, oscar_winner, nominees, award_show, winner) %>%
  mutate(nominees = ifelse(award_show == "Directors Guild" & mapped_award == "Best Director" & str_detect(nominees, ","),
                           str_sub(nominees, end = str_locate(nominees, ",") - 1),
                           nominees),
         picked_winner = ifelse(oscar_winner == nominees & winner == 1,
                                1, 0)) %>%
  group_by(mapped_award, award_show, ref_year) %>%
  summarize(picked_winner = sum(picked_winner, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(mapped_award, award_show) %>%
  summarize(picked_years = sum(picked_winner, na.rm = TRUE),
            total_years = n()) %>%
  ungroup() %>%
  mutate(percent_years = picked_years / total_years,
         squared_percent_years = percent_years ^ 2,
         insider_award = case_when(
           award_show == "BAFTA" ~ 1,
           mapped_award == "Best Picture" & award_show == "Producers Guild" ~ 1,
           mapped_award == "Best Actor" & award_show == "SAG" ~ 1,
           mapped_award == "Best Supporting Actor" & award_show == "SAG" ~ 1,
           mapped_award == "Best Actress" & award_show == "SAG" ~ 1,
           mapped_award == "Best Supporting Actress" & award_show == "SAG" ~ 1,
           mapped_award == "Best Animated Feature Film" & award_show == "Producers Guild" ~ 1,
           mapped_award == "Best Director" & award_show == "Directors Guild" ~ 1,
           mapped_award == "Best Film Editing" & award_show == "American Cinema Editors" ~ 1,
           TRUE ~ 0
         ),
         award_score = ifelse(insider_award == 1,
                        squared_percent_years * 2,
                        squared_percent_years)) %>%
  select(mapped_award, award_show, award_score)

#Calculate the scores for each of this year's nominees.

#REMOVED CODE USED TO CALCULATE FINAL PREDICTIONS SO THAT PARTICIPANTS MUST DO IT THEMSELVES.


##### GOOGLE NEWS MODEL #####


library(tidyverse)
library(rvest)
library(RSelenium)

#Set up the RSelenium driver.
driver <- rsDriver(browser = c("chrome"),chromever ="79.0.3945.36", port = 4444L)
remDr <- driver[["client"]]

#Import the CSV into a dataframe and create a blank column for the search results.
oscars2020 <- read_csv("oscars2020.csv") %>%
  mutate(result_count = NA_character_)

#Loop through each nominee in the dataframe
for (nominee in 1:nrow(oscars2020)) {
  
    #Create the URL using the nominee name and secondary field (if applicable)
    url <- paste0("https://www.google.com/search?&tbm=nws&q=",
                  str_replace(oscars2020$nominee_name[nominee], " ", "+"), "+",
                  ifelse(oscars2020$second_term[nominee] == 1,
                         paste0(str_replace(oscars2020$character_name[nominee], " ", "+"), "+"),
                         ""),
                  "oscars")
    
    #Send RSelenium to the page
    remDr$navigate(url)
    
    #Extract the HTML from the page
    html <- remDr$getPageSource()[[1]]
    html <- read_html(html)

    #Extract the "Results Stats" from the page
    result_count <- html %>%
      html_nodes("#resultStats") %>%
      html_text()
    
    #Add the stats to that row of the dataframe.
    oscars2020$result_count[nominee] <- result_count
  
}

#After you've scraped all the rows, clean up the result.
oscars2020 <- oscars2020 %>%
  mutate(result_count = str_extract(result_count, "(?<=About ).*(?= results)"),
         result_count = str_remove_all(result_count, ","),
         result_count = as.numeric(result_count))

#Take the top results getter for each award.
top_results <- oscars2020 %>%
  group_by(award) %>%
  top_n(result_count, n = 1)


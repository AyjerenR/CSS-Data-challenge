library(dplyr)
library(rstudioapi)
library(utf8)

#setting path
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#lading data
load("complete_tweets_by_politicians.RData") 
load("responses_tweets_bypol.RData")
tweets_by_politicians <- all.pol 
rm(all.pol) 

##getting the (updated) everypolitican data
everypol_bundestag <- read.csv("../Auxiliary datasets/unified_politicians_file_NOV2018.csv", 
                               stringsAsFactors = F, encoding="UTF-8") 

everypol_bundestag$twitter <- utf8_normalize(everypol_bundestag$twitter,
                                             remove_ignorable = T)
everypol_bundestag$twitter <- tolower(everypol_bundestag$twitter)
everypol_bundestag$twitter <- trimws(everypol_bundestag$twitter)

tweets_by_politicians$screen_name <- utf8_normalize(tweets_by_politicians$screen_name,
                                                    remove_ignorable = T)
tweets_by_politicians$screen_name <- tolower(tweets_by_politicians$screen_name)

#merging data
tweets_by_politicians <- tweets_by_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata",  "twitter")], 
            by=c("screen_name"="twitter")) 


tweets_matched <- responses_tweets_bypol %>%
  filter(is_retweet == FALSE) %>% 
  left_join(tweets_by_politicians[, c("screen_name", "text", "group", "gender",
                                      "user_id", "status_id")],
            by = c("reply_to_status_id" = "status_id"))




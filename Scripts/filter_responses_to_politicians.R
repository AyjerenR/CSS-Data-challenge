library(dplyr)
library(rstudioapi)

#setting path
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#lading data
load("complete_tweets_by_politicians.RData") 

setwd("../Data/tweets")
file_list <- list.files(pattern = c(".RData"))

## function for filtering and selecting relevant variables
prlmn.slct <- function(df) {
  out.dta <- df %>%
    filter(reply_to_status_id %in% all.pol$status_id) %>% 
    dplyr::select("text", "name", "screen_name","mentions_screen_name", "lang",
                  "reply_to_user_id", "favorite_count", "user_id", 
                  "retweet_count", "friends_count", "followers_count", 
                  "reply_to_status_id", "is_retweet")
  return(out.dta)
}

## create empty data.frame
responses_tweets_bypol <- data.frame()

## scrape relevant tweets (direct replys from by_politician tweets)
for (i in 1:length(file_list)){
  temp <- prlmn.slct(get(load(file_list[i])))
  responses_tweets_bypol <- rbind(responses_tweets_bypol, temp)
}

save(responses_tweets_bypol, file = "responses_tweets_bypol.RData")

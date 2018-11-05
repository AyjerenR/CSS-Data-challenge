#load library
library(twitteR)
library(ROAuth)
library(readr)
library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# load twitter data
twitter_ID <- read_csv("../Data/twitter_IDs.utf-8.csv", col_names = FALSE)

# load credentials (you have to fill in the credentials from your twitter 
# accounts. If you have not scraped twitter data yet, here is a short intro
# of how to set it up: 
# https://www.r-bloggers.com/setting-up-the-twitter-r-package-for-text-analytics/)
consumer_key <- "Y98MdS04vqXcxScjpCBTf6AFC"
consumer_secret<- "0w7qe2SJlfHqQphYnk0NI0ExLrIyw5JuuXwIogwlzlHWqVVPv7"
access_token <- "1575036906-J6jsJT9RCbyu6hHXt6e1RIkIOzuEoGi2UiWvmqG"
access_secret <- "8uBgefvlnFvr5lk4niH5KS0v9CF0lcuHIMuFh5Nn3luaN"

# set up to authenticate
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# function for retrieving chunks of tweets from ID (I don't know whether there 
# is a similar function implemented into the tweetR package now. I could not 
# find it.)
lookupStatus <- function (ids, ...){
  lapply(ids, twitteR:::check_id)
  
  batches <- split(ids, ceiling(seq_along(ids)/100))
  
  results <- lapply(batches, function(batch) {
    params <- parseIDs(batch)
    statuses <- twitteR:::twInterfaceObj$doAPICall(paste("statuses", "lookup", 
                                                         sep = "/"),
                                                   params = params, ...)
    twitteR:::import_statuses(statuses)
  })
  return(unlist(results))
}

parseIDs <- function(ids){
  id_list <- list()
  if (length(ids) > 0) {
    id_list$id <- paste(ids, collapse = ",")
  }
  return(id_list)
}


# sample twitter ids (don't know whether there is a better option to split the
# sampled data. I tested the sampling and it should works with the seed. The
# different chunks are mutually exclusive (no overlap))
#set.seed(12345)
#id_sample <- sample(as.character(twitter_ID$X1), 2000000, replace = FALSE)
splits=seq(from=1000000,to=length(twitter_ID$X1),1000000)
id_batch1 <- twitter_ID$X1[1:splits[1]]
id_batch2 <- twitter_ID$X1[splits[1]+1:splits[2]]
id_batch3 <- twitter_ID$X1[splits[2]+1:splits[3]]
id_batch4 <- twitter_ID$X1[splits[3]+1:splits[4]]
id_batch5 <- twitter_ID$X1[splits[4]+1:splits[5]]
id_batch6 <- twitter_ID$X1[splits[5]+1:splits[6]]
id_batch7 <- twitter_ID$X1[splits[6]+1:splits[7]]
id_batch8 <- twitter_ID$X1[splits[7]+1:splits[8]]
id_batch9 <- twitter_ID$X1[splits[8]+1:splits[9]]
id_batch10 <- twitter_ID$X1[splits[9]+1:splits[10]]
id_batch11 <- twitter_ID$X1[splits[10]+1:splits[11]]
id_batch12 <- twitter_ID$X1[splits[11]+1:splits[12]]
id_batch13 <- twitter_ID$X1[splits[12]+1:splits[13]]
id_batch14 <- twitter_ID$X1[splits[13]+1:splits[14]]
id_batch15 <- twitter_ID$X1[splits[14]+1:splits[15]]
id_batch16 <- twitter_ID$X1[splits[15]+1:splits[16]]
id_batch17 <- twitter_ID$X1[splits[16]+1:splits[17]]
id_batch18 <- twitter_ID$X1[splits[17]+1:splits[18]]
id_batch19 <- twitter_ID$X1[splits[18]+1:splits[19]]
id_batch20 <- twitter_ID$X1[splits[19]+1:splits[20]]
id_batch21 <- twitter_ID$X1[splits[20]+1:splits[21]]
id_batch22 <- twitter_ID$X1[splits[21]+1:splits[22]]
id_batch23 <- twitter_ID$X1[splits[22]+1:length(twitter_ID$X1)]

#id_armin <- id_sample[1500001:2000000]

# scrape_tweets (here change to your chunk of the sampled data)
tweets1 <- lookupStatus(id_batch1, retryOnRateLimit=100)
tweets2 <- lookupStatus(id_batch2, retryOnRateLimit=100)
tweets3 <- lookupStatus(id_batch3, retryOnRateLimit=100)
tweets4 <- lookupStatus(id_batch4, retryOnRateLimit=100)
tweets5 <- lookupStatus(id_batch5, retryOnRateLimit=100)
tweets6 <- lookupStatus(id_batch6, retryOnRateLimit=100)
tweets7 <- lookupStatus(id_batch7, retryOnRateLimit=100)
tweets8 <- lookupStatus(id_batch8, retryOnRateLimit=100)
tweets9 <- lookupStatus(id_batch9, retryOnRateLimit=100)
tweets10 <- lookupStatus(id_batch10, retryOnRateLimit=100)
tweets11 <- lookupStatus(id_batch11, retryOnRateLimit=100)
tweets12 <- lookupStatus(id_batch12, retryOnRateLimit=100)
tweets13 <- lookupStatus(id_batch13, retryOnRateLimit=100)
tweets14 <- lookupStatus(id_batch14, retryOnRateLimit=100)
tweets15 <- lookupStatus(id_batch15, retryOnRateLimit=100)
tweets16 <- lookupStatus(id_batch16, retryOnRateLimit=100)
tweets17 <- lookupStatus(id_batch17, retryOnRateLimit=100)
tweets18 <- lookupStatus(id_batch18, retryOnRateLimit=100)
tweets19 <- lookupStatus(id_batch19, retryOnRateLimit=100)
tweets20 <- lookupStatus(id_batch20, retryOnRateLimit=100)
tweets21 <- lookupStatus(id_batch21, retryOnRateLimit=100)
tweets22 <- lookupStatus(id_batch22, retryOnRateLimit=100)
tweets23 <- lookupStatus(id_batch23, retryOnRateLimit=100)

# turn tweet list into dataframe 
tweets_df1 <- twListToDF(tweets1)
tweets_df2 <- twListToDF(tweets2)
tweets_df3 <- twListToDF(tweets3)
tweets_df4 <- twListToDF(tweets4)
tweets_df5 <- twListToDF(tweets5)
tweets_df6 <- twListToDF(tweets6)
tweets_df7 <- twListToDF(tweets7)
tweets_df8 <- twListToDF(tweets8)
tweets_df9 <- twListToDF(tweets9)
tweets_df10 <- twListToDF(tweets10)
tweets_df11 <- twListToDF(tweets11)
tweets_df12 <- twListToDF(tweets12)
tweets_df13 <- twListToDF(tweets13)
tweets_df14 <- twListToDF(tweets14)
tweets_df15 <- twListToDF(tweets15)
tweets_df16 <- twListToDF(tweets16)
tweets_df17 <- twListToDF(tweets17)
tweets_df18 <- twListToDF(tweets18)
tweets_df19 <- twListToDF(tweets19)
tweets_df20 <- twListToDF(tweets20)
tweets_df21 <- twListToDF(tweets21)
tweets_df22 <- twListToDF(tweets22)
tweets_df23 <- twListToDF(tweets23)

# save as Rdata (we join the data later on)
save(tweets_df1, file = "tweets_1.RData")
save(tweets_df2, file = "tweets_2.RData")
save(tweets_df3, file = "tweets_3.RData")
save(tweets_df4, file = "tweets_4.RData")
save(tweets_df5, file = "tweets_5.RData")
save(tweets_df6, file = "tweets_6.RData")
save(tweets_df7, file = "tweets_7.RData")
save(tweets_df8, file = "tweets_8.RData")
save(tweets_df9, file = "tweets_9.RData")
save(tweets_df10, file = "tweets_10.RData")
save(tweets_df11, file = "tweets_11.RData")
save(tweets_df12, file = "tweets_12.RData")
save(tweets_df13, file = "tweets_13.RData")
save(tweets_df14, file = "tweets_14.RData")
save(tweets_df15, file = "tweets_15.RData")
save(tweets_df16, file = "tweets_16.RData")
save(tweets_df17, file = "tweets_17.RData")
save(tweets_df18, file = "tweets_18.RData")
save(tweets_df19, file = "tweets_19.RData")
save(tweets_df20, file = "tweets_20.RData")
save(tweets_df21, file = "tweets_21.RData")
save(tweets_df22, file = "tweets_22.RData")
save(tweets_df23, file = "tweets_23.RData")
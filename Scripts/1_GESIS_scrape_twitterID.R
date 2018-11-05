#load library
library(twitteR)
library(ROAuth)
library(readr)
library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# load twitter data
twitter_ID <- read_csv("twitter_IDs.utf-8.csv", col_names = FALSE)

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

#Create Token for rtwet
detach(package:twitteR)
library(rtweet)
create_token(
  app = "css_test",
  consumer_key = "Y98MdS04vqXcxScjpCBTf6AFC",
  consumer_secret = "0w7qe2SJlfHqQphYnk0NI0ExLrIyw5JuuXwIogwlzlHWqVVPv7",
  access_token = "1575036906-J6jsJT9RCbyu6hHXt6e1RIkIOzuEoGi2UiWvmqG",
  access_secret = "8uBgefvlnFvr5lk4niH5KS0v9CF0lcuHIMuFh5Nn3luaN")

#Define splits to scrape 80000 tweets at a time
splits=seq(from=0,to=length(twitter_ID$X1),80000)

#scrape tweets from API in batches of 80000
for(i in 1:(length(splits)-1)){
  #Sleep 15 minutes to not exhaust limits
  Sys.sleep(900)
  #Define a batch of 80000 IDs to scrape
  id_batch <- as.character(twitter_ID$X1[(splits[i]+1):(splits[i+1])])
  #Scrape the 80000 tweets, this will produce a nicely formatted data frame
  tweets_df=lookup_tweets(id_batch, parse = TRUE, token = NULL)
  #design a name
  name=paste("tweetbatch",i,sep="_")
  #Save tweets as RData file. Tis will create 276 RData files
  save(tweets_df, file = paste0("tweets/",name,".RData"))
}

#Same code for the last RData file because I was to lazy to code this directly into the loop above.
i=277
id_batch <- as.character(twitter_ID$X1[(splits[277]+1):nrow(twitter_ID)])
tweets_df=lookup_tweets(id_batch, parse = TRUE, token = NULL)
#  tweets <- lookupStatus(id_batch, retryOnRateLimit=100)
#  tweets_df <- twListToDF(tweets)
name=paste("tweetbatch",i,sep="_")
save(tweets_df, file = paste0("tweets/",name,".RData"))


all.files = paste0("../Data/tweets/",dir("../Data/tweets/"))
everypol_bundestag = 
  read_csv("../Auxiliary datasets/everypolitican_bundestag19.csv", 
           col_names = TRUE) ##getting everypolitican data

#Load each batch seperately, then filter for tweets by politicians and save the new RData frame
for(i in all.files){
  load(file=i)
  tweets_by_politicians=
    tweets_df%>%filter(screen_name%in%everypol_bundestag$twitter)
  name=paste("tweetbatch",as.numeric(gsub("[^\\d]+", "", i, perl=TRUE)),"bypols",sep="_")
save(tweets_by_politicians, file = paste0("../Data/tweets_by_politicians/",name,".RData"))
}

#Combine all Tweet batches with tweets by politicians
all.files.pol = paste0("../Data/tweets_by_politicians//",dir("../Data/tweets_by_politicians//"))
mylist.pol<- lapply(all.files.pol, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

names(mylist.pol) <- all.files
all.pol <- do.call("rbind", mylist.pol)
all.pol$id <- rep(all.files.pol, sapply(mylist.pol, nrow))
#Save tweets by politicans as a single RData file
save(all.pol, file = paste0("complete_tweets_by_politicians.RData"))

############################################################
# Merging Twitter data to Everypolitican data
#    http://docs.everypolitician.org/use_the_data.html
#    Data that we want: Twitter accounts, Gender
#########################################

#load library
#library(twitteR)
#library(readr)
library(rstudioapi)
library(tidyverse)
library(scales)
library(gridExtra)
library(grid)

#setting path
#current_path <- getActiveDocumentContext()$path 
#setwd(dirname(current_path))

#lading data
#load("../Data/complete_tweets.RData") 
everypol_bundestag = 
  read_csv("/Users/arminmertens/Desktop/everypolitican_bundestag19.csv", 
           col_names = TRUE) ##getting everypolitican data

twitterpols <- everypol_bundestag %>%
  filter(!is.na(twitter))

tweets_by_politicians <- all.pol %>% 
  filter(screen_name%in%everypol_bundestag$twitter)

#JEREN: Add mentions!!!!!!!!!!!!!!
tweets_at_politicians <- all.pol %>%
  filter(reply_to_screen_name %in% everypol_bundestag$twitter & 
           !is.na(reply_to_screen_name))

#merging data
tweets_by_politicians <- left_join(tweets_by_politicians, everypol_bundestag[ , 
                             c("name", "gender", "group", "facebook", 
                               "wikidata", "twitter")], 
                             by=c("screen_name"="twitter")) 

tweets_at_politicians <- left_join(tweets_at_politicians, everypol_bundestag[ , 
                                  c("name", "gender", "group", "facebook", 
                                    "wikidata", "twitter")], 
                                  by=c("screen_name"="twitter")) 


## Rename and join political parties
tweets_by_politicians$group <- as.factor(tweets_by_politicians$group) 

tweets_by_politicians$group <- plyr::revalue(tweets_by_politicians$group, 
                                   c("Alliance '90/The Greens" = "Greens",
                                     "Alternative for Germany" = "AfD",
                                     "Christian Democratic Union" = "CDU/CSU",
                                     "Christian Social Union of Bavaria" = 
                                       "CDU/CSU",
                                     "Die Linke" = "Left",
                                     "Free Democratic Party" = "FDP",
                                     "Social Democratic Party of Germany" = 
                                       "SPD"))

## Define party colours
cols <- c("Left" = "#960E66",
          "Greens" = "#00A646",
          "AfD" = "#009FE1",
          "CDU/CSU" = "#2A2B2C",
          "FDP" = "#ffed00",
          "SPD" = "#DF0029")

## Reorder factor levels
tweets_by_politicians$group <- factor(tweets_by_politicians$group, 
                                      levels = c("Left", "Greens", "SPD", 
                                                 "CDU/CSU", "FDP", "AfD"))

###################
# Exploring data
####################

# Filter for all relevant tweets (made by politicians)
#tweets_politicans <- tweets_join %>% 
#  filter(!is.na(name)) 

## number of politicans by gender in general 
gender_bt <- everypol_bundestag %>% 
  filter(!is.na(gender)) %>%
  group_by(gender) %>% 
  summarise(n_bt = n(),perc_bt=(n()*100)/nrow(everypol_bundestag))


## number of tweeting politicians by gender
gender_twitter <- tweets_by_politicians %>% 
  group_by(gender) %>% 
  summarise(n_twitter = n_distinct(name.y),
            perc_bt=(n_distinct(name.y)*100)/
              length(unique(tweets_by_politicians$name.y)))


## number of tweets by gender
gender_tweets <- tweets_by_politicians %>% 
  group_by(gender) %>% 
  summarise(n_tweets = n(),
            perc_bt=(n()*100)/nrow(tweets_by_politicians))

## Plot representation ratio by gender 
gender_representation <- gender_bt %>% 
  left_join(gender_twitter, by = "gender") %>% 
  left_join(gender_tweets, by = "gender") %>% 
  gather("n_bt", "n_twitter", "n_tweets",
         key = "type", value = "n") %>% 
  mutate(type = as.factor(type),
         gender = as.factor(gender)) %>% 
  ggplot(aes(type, n,fill = gender)) + 
  geom_bar(position = position_fill(reverse = TRUE),stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(limits = c("n_bt", "n_twitter", "n_tweets"),
                   labels = c("Bundestag", "Twitter", "Number of tweets")) +
  xlab("") + ylab("") + 
  ggtitle("Representation by Gender") +
  theme_bw()

## Descriptive Plots
## Gender representation by politial party
gender_bt_groups <- everypol_bundestag %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n_bt = n())


## Number of politicans by gender in general 
gender_bt_party <- everypol_bundestag %>% 
  filter(!is.na(gender)) %>%
  group_by(group, gender) %>% 
  summarise(n_bt = n()) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_bt_party <- gender_bt_party %>% 
  left_join(gender_bt_groups, by = "group") %>% 
  mutate(ratio = n_bt.x / n_bt.y,
         gender = as.factor(gender),
         group = as.factor(group))

## plot gender ratios by party 
gender_bt_plot <- gender_bt_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, gender, ratio), 
             n_bt.x,fill = gender)) + 
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  xlab("") + ylab("") + 
  ggtitle("Politicans by gender and political party") +
  coord_flip() +
  theme_bw() +
  NULL

## Tweets
## Gender representation by politial party
gender_tw_groups <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n())

## Number of politicans by gender in general 
gender_tw_party <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_tw_party <- gender_tw_party %>% 
  left_join(gender_tw_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

## Plot number of tweets by party and gender 
gender_tweets_plot <- gender_tw_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, gender, -n.y), n.x,fill = gender)) + 
  geom_bar(stat = "identity") +
  xlab("") + ylab("") + 
  ggtitle("Number of tweets by party and gender") +
  coord_flip() +
  theme_bw() +
  NULL


## Twitter
## Gender representation by politial party
gender_twitter_groups <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n_distinct(name.y))

## number of politicans by gender in general 
gender_twitter_party <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group, gender) %>% 
  summarise(n = n_distinct(name.y)) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_twitter_party <- gender_twitter_party %>% 
  left_join(gender_twitter_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

## Plot number of twitter account by party and gender 
gender_twitter_plot <- gender_twitter_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, gender, -n.y), n.x,fill = gender)) + 
  geom_bar(stat = "identity") +
  xlab("") + ylab("") + 
  ggtitle("Number of twitter users by party and gender") +
  coord_flip() +
  theme_bw() +
  NULL


## Descriptive statistics of reply to politicans' tweets
gender_reply_groups <- tweets_at_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n())

## number of politicans by gender in general 
gender_reply_party <- tweets_at_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_reply_party <- gender_reply_party %>% 
  left_join(gender_reply_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

## Plot number of twitter account by party and gender 
twitter_replys_plot <- gender_reply_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, gender, -n.y), n.x,fill = gender)) + 
  geom_bar(stat = "identity") +
#  scale_y_continuous(labels = percent_format()) +
  xlab("") + ylab("") + 
  ggtitle("Replys to politicans' tweets by party and gender") +
  coord_flip() +
  theme_bw() +
  NULL

## Ratio of favourites to total tweets by gender 
fav_plot <- tweets_by_politicians %>% 
  filter(is_retweet == FALSE) %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            fav = sum(favorite_count),
            ratio = fav / n) %>% 
  ggplot(aes(gender, ratio, fill = gender)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of favourites per original tweet and gender") +
  theme_bw() +
  NULL

## All plots in one frame
grid.arrange(gender_representation, gender_bt_plot, gender_twitter_plot, 
             gender_tweets_plot, twitter_replys_plot, fav_plot, 
             ncol = 2, nrow = 3,
             top=textGrob("First descriptive statistics of GESIS data",
                          gp=gpar(fontsize=20 ))) 

## Alternative plot with party colours
gender_tw_party %>% 
  ggplot(aes(gender, n.x,fill = group)) + 
  geom_bar(stat = "identity", col = "black") +
  facet_grid(group ~ .) +
  scale_fill_manual(values=cols) +
  xlab("") + ylab("") + 
  ggtitle("Number of tweets by party and gender") +
  coord_flip() +
  theme_bw() +
  NULL

  
## Who creates more original content (retweet y/n)
tweets_politicans %>% 
  group_by(gender) %>% 
  filter(isRetweet == FALSE) %>% 
  summarise(retweet = sum(retweetCount))

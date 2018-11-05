############################################################
# Merging Twitter data to Everypolitican data
#    http://docs.everypolitician.org/use_the_data.html
#    Data that we want: Twitter accounts, Gender
# Some first descriptove analyses:
# -female/male ratios: Bundestag, 
# representation in twitter (at least one tweet),
# number tweets coming from female vs. male politicans
# 
#
#
#########################################

#load library
library(twitteR)
library(readr)
library(rstudioapi)
library(dplyr)
library("stminsights")


#setting path
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#lading data
load("../Data/tweets_franzi.RData") ##getting twitter data; SHOULD CHANGED TO MERGED DATA
everypol_bundestag = read_csv("../Auxiliary datasets/everypolitican_bundestag19.csv", col_names = TRUE) ##getting everypolitican data
everypol_bundestag$screenName=everypol_bundestag$twitter

#merging data

tweets_politicans = left_join(tweets_df, everypol_bundestag[ , c("name", "gender", "group", "facebook", "wikidata", "twitter")], by=c("screenName"="twitter")) 

###################
# Exploring data
#
#
####################

###creating a  variable that we need later (counts tweet coming from an politican)

tweets_politicans$numtweet = 0 
tweets_politicans$numtweet[which(!is.na(tweets_politicans$name))] = 1 ##helps me to calculate a link when a politican made that tweet
sum(tweets_politicans$numtweet)


# OVERALL NUMBER OF POLITICANS  BY GENDER
bundestagf=table(everypol_bundestag$gender[everypol_bundestag$gender=="female"]) ##overall number of female politicans in Bundestag
bundestagm=table(everypol_bundestag$gender[everypol_bundestag$gender=="male"]) ##overall number of male politicans in Bundestag
bundestagf=sum(everypol_bundestag$gender%in%"female",na.rm=T)
bundestagm=sum(everypol_bundestag$gender%in%"male",na.rm=T)
bundestagf  #number of female politicans
bundestagm #number of male politicans

bundestagfm_r= bundestagf / bundestagm  #ratio (female politicans/ male politicans)
bundestagfm_r


# OVERALL NUMBER OF TWEETS  BY GENDER
ntweets_polf = sum((tweets_politicans$gender=="female")[!is.na(tweets_politicans$name)]) #overall number of tweets: female politicans
ntweets_polm = sum((tweets_politicans$gender=="male")[!is.na(tweets_politicans$name)]) #overall number of tweets: male politicans
ntweets_polf #number of tweets coming from female politican
ntweets_polm #number of tweets coming from male politican
ntweets_polfm_r= ntweets_polf/ ntweets_polm  #ratio (overall tweets  female politicans/ overall tweets male politicans)
ntweets_polfm_r



# OVERALL NUMBER OF TWEETING POLITICANS BY GENDER
table(tweets_politicans%>%filter(!duplicated(tweets_politicans$name))%>%select(gender),exclude=NULL) 
ntweeting_polf = sum((tweets_politicans$gender=="female")[!is.na(tweets_politicans$name) & !duplicated(tweets_politicans$name)]) ##number of tweeting female politicans in dataset
ntweeting_polm = sum((tweets_politicans$gender=="male")[!is.na(tweets_politicans$name) & !duplicated(tweets_politicans$name)]) ##number of tweeting male politicans in dataset
ntweeting_polf #number of tweeting female politicans in datset
ntweeting_polm #number of tweeting male politicans in datset
ntweeting_polfm_r = ntweeting_polf /ntweeting_polm #ratio (number tweeting female politicans/ number tweeting male politicans)
ntweeting_polfm_r 

#### Structual bias
bundestagfm_r #ratio (female politicans/ male politicans)
ntweets_polfm_r #ratio (overall tweets  female politicans/ overall tweets male politicans)
ntweeting_polfm_r #ratio (number tweeting female politicans/ number tweeting male politicans)

aggregate(tweets_politicans$numtweet~tweets_politicans$gender+tweets_politicans$group,FUN=sum)  ###aggregated overall tweets (every tweet counts, included politicans duplicates)
aggregate(tweets_politicans$numtweet~tweets_politicans$gender,FUN=sum)


### Sum of tweets by politicans ######hier weitermachen
#library(dplyr)
#tweets_politicans$numtweet %>% 
 # group_by(tweets_politicans$gender,na.rm=T) %>% 
  #summarise(Frequency = sum(Frequency))
 ###########           





###################
# Exploring topics
#
#
####################
#### Exploring topics: stminsights: A Shiny Application for Inspecting Structural Topic Models

library(stm)
library(quanteda)

########################
# prepare data
########################
#https://cran.r-project.org/web/packages/stminsights/vignettes/intro.html
#http://www.mjdenny.com/Text_Processing_In_R.html


########################################################################################################################
# IDEEN: Erweiterungen: vielleicht kann man auch über abgeordnetenwatch an den twitter account heran, bzw vlt kann auch zusätzlich mit Tools die Facebookdaten verarbeiten gearbeite  werden (xxx % gefällt das); xxx abonniert
#
#
#
#
rm(list=ls())
library(rstudioapi)
library(tidyverse)
library(quanteda)
library(Zelig)


current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
library(jsonlite)
library(XML)
library(RCurl)
library(stringr)
winners <- fromJSON("https://www.abgeordnetenwatch.de/api/parliaments.json", flatten=TRUE)
data.parl=winners$parliaments

bt17_json=data.parl$`datasets.deputies.by-name`[data.parl$name=="Bundestag"]
bt17 <- fromJSON(bt17_json, flatten=TRUE)
bt17_profiles=bt17$profiles

data_questions=data.frame(id=NA,date=NA,category=NA,url=NA,answers=NA,name=NA)
pb <- txtProgressBar(min = 0, max = nrow(bt17_profiles), style = 3)
for(i in 205:nrow(bt17_profiles)){
  name=bt17_profiles$meta.username[i]
  profile <- fromJSON(paste0("https://www.abgeordnetenwatch.de/api/parliament/bundestag/profile/",name,"/profile.json"), flatten=TRUE)
  questions=profile$profile$questions
  if(length(questions)>0){
  questions$name=name
  data_questions=rbind(data_questions,questions)
  }
  Sys.sleep(1)
  setTxtProgressBar(pb, i)
}
data_questions=data_questions[-1,]

save(file="../Data/question_data.RData",data_questions,bt17_profiles)

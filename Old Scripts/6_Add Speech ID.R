##1: Creating the 'Reader in Column' value. Counting the number of speeches for each reading (1st, 2nd, 3rd) and assigning that value. 
##2: Creating 'Speech ID' value. Counting the number of speeches overall on that bill and assigning that value.
##3: Creating both of the prior columns in the individual legislature dataframes. 

rm(list=ls())
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015/")
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015") #SOP
setwd("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/") #Jens
load("Individual legislature dataframes/16leg_metadata_df.Rdata")
load("Individual legislature dataframes/17leg_metadata_df.Rdata")
load("Individual legislature dataframes/18leg_metadata_df.Rdata")
load("Individual legislature dataframes/16leg_metadata_speakers_complete_df_encoded.Rdata")
load("Individual legislature dataframes/17leg_metadata_speakers_complete_df_encoded.Rdata")
load("Individual legislature dataframes/18leg_metadata_speakers_complete_df_encoded.Rdata")
metadata_df=rbind(metadata_df16,metadata_df17,metadata_df18)
metadata_speakers_df=rbind(metadata_speakers_complete_df16,metadata_speakers_complete_df17,metadata_speakers_complete_df18)
table(metadata_speakers_df$Legislative_period)
i=1
##1
#Creating 'Reader in Column' value
for (i in 1:nrow(metadata_df)) {
  bill<-metadata_df[i, "Drucksachennummer"]
  
  speeches1<-which(metadata_speakers_df$Drucksachennummer==bill & metadata_speakers_df$Reading==1 & metadata_speakers_df$'Type of speech'=="Rede"& !is.na(metadata_speakers_df$'Type of speech'))
  if (length(speeches1)>0) {
    for (j in 1:length(speeches1)) {
      metadata_speakers_df[speeches1[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0") #padding the number to make it 3 digits
    }
  }
  speeches2<-which(metadata_speakers_df$Drucksachennummer==bill & metadata_speakers_df$Reading==2 & metadata_speakers_df$'Type of speech'=="Rede"& !is.na(metadata_speakers_df$'Type of speech'))
  if (length(speeches2)>0) {
    for (j in 1:length(speeches2)) {
      metadata_speakers_df[speeches2[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches3<-which(metadata_speakers_df$Drucksachennummer==bill & metadata_speakers_df$Reading==3 & metadata_speakers_df$'Type of speech'=="Rede"& !is.na(metadata_speakers_df$'Type of speech'))
  if (length(speeches3)>0) {
    for (j in 1:length(speeches3)) {
      metadata_speakers_df[speeches3[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

##2
#Creating 'Speech ID' value
for (i in 1:nrow(metadata_df)) {
  bill<-metadata_df[i, "Drucksachennummer"]
  
  speeches4<-which(metadata_speakers_df$Drucksachennummer==bill & metadata_speakers_df$'Type of speech'=="Rede"& !is.na(metadata_speakers_df$'Type of speech'))
  if (length(speeches4)>0) {
    for (j in 1:length(speeches4)) {
      metadata_speakers_df[speeches4[j], "Speech_ID"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

##3
#Creating 'Reader in Column' and 'Speech_ID' columns in individual legislatures
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015/Individual legislature dataframes")
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015/Individual legislature dataframes") #SOP
#load("18leg_metadata_speakers_complete_df_encoded.Rdata")
#load("18leg_metadata_df.Rdata")
i=1
#Creating 'Reader in Column' value
for (i in 1:nrow(metadata_df18)) {
  bill<-metadata_df18[i, "Drucksachennummer"]
  
  speeches1<-which(metadata_speakers_complete_df18$Drucksachennummer==bill & metadata_speakers_complete_df18$Reading==1 & metadata_speakers_complete_df18$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df18$'Type of speech'))
  if (length(speeches1)>0) {
    for (j in 1:length(speeches1)) {
      metadata_speakers_complete_df18[speeches1[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches2<-which(metadata_speakers_complete_df18$Drucksachennummer==bill & metadata_speakers_complete_df18$Reading==2 & metadata_speakers_complete_df18$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df18$'Type of speech'))
  if (length(speeches2)>0) {
    for (j in 1:length(speeches2)) {
      metadata_speakers_complete_df18[speeches2[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches3<-which(metadata_speakers_complete_df18$Drucksachennummer==bill & metadata_speakers_complete_df18$Reading==3 & metadata_speakers_complete_df18$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df18$'Type of speech'))
  if (length(speeches3)>0) {
    for (j in 1:length(speeches3)) {
      metadata_speakers_complete_df18[speeches3[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

#Creating 'Speech ID' value
for (i in 1:nrow(metadata_df18)) {
  bill<-metadata_df18[i, "Drucksachennummer"]
  
  speeches4<-which(metadata_speakers_complete_df18$Drucksachennummer==bill & metadata_speakers_complete_df18$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df18$'Type of speech'))
  if (length(speeches4)>0) {
    for (j in 1:length(speeches4)) {
      metadata_speakers_complete_df18[speeches4[j], "Speech_ID"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

for (i in 1:nrow(metadata_df17)) {
  bill<-metadata_df17[i, "Drucksachennummer"]
  
  speeches1<-which(metadata_speakers_complete_df17$Drucksachennummer==bill & metadata_speakers_complete_df17$Reading==1 & metadata_speakers_complete_df17$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df17$'Type of speech'))
  if (length(speeches1)>0) {
    for (j in 1:length(speeches1)) {
      metadata_speakers_complete_df17[speeches1[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches2<-which(metadata_speakers_complete_df17$Drucksachennummer==bill & metadata_speakers_complete_df17$Reading==2 & metadata_speakers_complete_df17$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df17$'Type of speech'))
  if (length(speeches2)>0) {
    for (j in 1:length(speeches2)) {
      metadata_speakers_complete_df17[speeches2[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches3<-which(metadata_speakers_complete_df17$Drucksachennummer==bill & metadata_speakers_complete_df17$Reading==3 & metadata_speakers_complete_df17$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df17$'Type of speech'))
  if (length(speeches3)>0) {
    for (j in 1:length(speeches3)) {
      metadata_speakers_complete_df17[speeches3[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

#Creating 'Speech ID' value
for (i in 1:nrow(metadata_df17)) {
  bill<-metadata_df17[i, "Drucksachennummer"]
  
  speeches4<-which(metadata_speakers_complete_df17$Drucksachennummer==bill & metadata_speakers_complete_df17$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df17$'Type of speech'))
  if (length(speeches4)>0) {
    for (j in 1:length(speeches4)) {
      metadata_speakers_complete_df17[speeches4[j], "Speech_ID"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

for (i in 1:nrow(metadata_df16)) {
  bill<-metadata_df16[i, "Drucksachennummer"]
  
  speeches1<-which(metadata_speakers_complete_df16$Drucksachennummer==bill & metadata_speakers_complete_df16$Reading==1 & metadata_speakers_complete_df16$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df16$'Type of speech'))
  if (length(speeches1)>0) {
    for (j in 1:length(speeches1)) {
      metadata_speakers_complete_df16[speeches1[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches2<-which(metadata_speakers_complete_df16$Drucksachennummer==bill & metadata_speakers_complete_df16$Reading==2 & metadata_speakers_complete_df16$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df16$'Type of speech'))
  if (length(speeches2)>0) {
    for (j in 1:length(speeches2)) {
      metadata_speakers_complete_df16[speeches2[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
  speeches3<-which(metadata_speakers_complete_df16$Drucksachennummer==bill & metadata_speakers_complete_df16$Reading==3 & metadata_speakers_complete_df16$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df16$'Type of speech'))
  if (length(speeches3)>0) {
    for (j in 1:length(speeches3)) {
      metadata_speakers_complete_df16[speeches3[j], "Speaker_in_reading"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}

#Creating 'Speech ID' value
for (i in 1:nrow(metadata_df16)) {
  bill<-metadata_df16[i, "Drucksachennummer"]
  
  speeches4<-which(metadata_speakers_complete_df16$Drucksachennummer==bill & metadata_speakers_complete_df16$'Type of speech'=="Rede"& !is.na(metadata_speakers_complete_df16$'Type of speech'))
  if (length(speeches4)>0) {
    for (j in 1:length(speeches4)) {
      metadata_speakers_complete_df16[speeches4[j], "Speech_ID"]<-str_pad(j, width=3, side="left", pad="0")
    }
  }
}
dir()
save(metadata_speakers_complete_df16,file="Individual legislature dataframes/16leg_metadata_speakers_complete_df_encoded.Rdata")
save(metadata_speakers_complete_df17,file="Individual legislature dataframes/17leg_metadata_speakers_complete_df_encoded.Rdata")
save(metadata_speakers_complete_df18,file="Individual legislature dataframes/18leg_metadata_speakers_complete_df_encoded.Rdata")

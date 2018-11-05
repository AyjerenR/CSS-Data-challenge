##THIS SCRIPT MUST BE RUN ON A MAC.
##1. Using stringr on the PC webscraping script results in corrupted encoding of the special characters. Therefore, since stringr is necessary to extract the EU impulse value from the 'Inhalt' column, it must be done after the PC extraction on a Mac.
##2. Similarly, the speaker names should be trimmed after extraction on the PC, since trimming them in the PC script would corrupt the special characters.

rm(list=ls())
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015")
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015") #SOP
setwd("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/") #Jens
load("Individual legislature dataframes/16leg_metadata_df.Rdata")
load("Individual legislature dataframes/17leg_metadata_df.Rdata")
load("Individual legislature dataframes/18leg_metadata_df.Rdata")
load("Individual legislature dataframes/16leg_metadata_speakers_complete_df_encoded.Rdata")
load("Individual legislature dataframes/17leg_metadata_speakers_complete_df_encoded.Rdata")
load("Individual legislature dataframes/18leg_metadata_speakers_complete_df_encoded.Rdata")
library(stringr)


##1
#For dataframes without speakers, Europaische impulse must be extracted from the Inhalt column
for (i in 1:nrow(metadata_df16)) {
  EU_impulse<-str_extract(metadata_df16[i, "Inhalt"], "Europäische Impulse\\:.+")
  EU_impulse<-str_extract(EU_impulse, "\\:.+")
  EU_impulse_value<-str_extract(EU_impulse, "[[:alnum:]].+")
  metadata_df16[i, "EU_impulse_relationship"]<-EU_impulse_value
}
save(metadata_df16, file="Individual legislature dataframes/16leg_metadata_df.Rdata")

for (i in 1:nrow(metadata_df17)) {
  EU_impulse<-str_extract(metadata_df17[i, "Inhalt"], "Europäische Impulse\\:.+")
  EU_impulse<-str_extract(EU_impulse, "\\:.+")
  EU_impulse_value<-str_extract(EU_impulse, "[[:alnum:]].+")
  metadata_df17[i, "EU_impulse_relationship"]<-EU_impulse_value
}
save(metadata_df17, file="Individual legislature dataframes/17leg_metadata_df.Rdata")

for (i in 1:nrow(metadata_df18)) {
  EU_impulse<-str_extract(metadata_df18[i, "Inhalt"], "Europäische Impulse\\:.+")
  EU_impulse<-str_extract(EU_impulse, "\\:.+")
  EU_impulse_value<-str_extract(EU_impulse, "[[:alnum:]].+")
  metadata_df18[i, "EU_impulse_relationship"]<-EU_impulse_value
}
save(metadata_df18, file="Individual legislature dataframes/18leg_metadata_df.Rdata")


##2
#For speaker dataframes, Europaische impulse must be extracted from the Inhalt column AND the speaker names and parties should be trimmed
for (i in 1:nrow(metadata_speakers_complete_df16)) {
  EU_impulse<-str_extract(metadata_speakers_complete_df16[i, "Inhalt"], "Europäische Impulse\\:.+")
  EU_impulse<-str_extract(EU_impulse, "\\:.+")
  EU_impulse_value<-str_extract(EU_impulse, "[[:alnum:]].+")
  metadata_speakers_complete_df16[i, "EU_impulse_relationship"]<-EU_impulse_value
  
  metadata_speakers_complete_df16[i, "Speaker name"]<-str_trim(metadata_speakers_complete_df16[i, "Speaker name"])
  metadata_speakers_complete_df16[i, "Speaker party"]<-str_trim(metadata_speakers_complete_df16[i, "Speaker party"])
}
save(metadata_speakers_complete_df16, file="Individual legislature dataframes/16leg_metadata_speakers_complete_df_encoded.Rdata")

for (i in 1:nrow(metadata_speakers_complete_df17)) {
  EU_impulse<-str_extract(metadata_speakers_complete_df17[i, "Inhalt"], "Europäische Impulse\\:.+")
  EU_impulse<-str_extract(EU_impulse, "\\:.+")
  EU_impulse_value<-str_extract(EU_impulse, "[[:alnum:]].+")
  metadata_speakers_complete_df17[i, "EU_impulse_relationship"]<-EU_impulse_value
  
  metadata_speakers_complete_df17[i, "Speaker name"]<-str_trim(metadata_speakers_complete_df17[i, "Speaker name"])
  metadata_speakers_complete_df17[i, "Speaker party"]<-str_trim(metadata_speakers_complete_df17[i, "Speaker party"])
}
save(metadata_speakers_complete_df17, file="Individual legislature dataframes/17leg_metadata_speakers_complete_df_encoded.Rdata")

for (i in 1:nrow(metadata_speakers_complete_df18)) {
  EU_impulse<-str_extract(metadata_speakers_complete_df18[i, "Inhalt"], "Europäische Impulse\\:.+")
  EU_impulse<-str_extract(EU_impulse, "\\:.+")
  EU_impulse_value<-str_extract(EU_impulse, "[[:alnum:]].+")
  metadata_speakers_complete_df18[i, "EU_impulse_relationship"]<-EU_impulse_value
  
  metadata_speakers_complete_df18[i, "Speaker name"]<-str_trim(metadata_speakers_complete_df18[i, "Speaker name"])
  metadata_speakers_complete_df18[i, "Speaker party"]<-str_trim(metadata_speakers_complete_df18[i, "Speaker party"])
}
save(metadata_speakers_complete_df18, file="Individual legislature dataframes/18leg_metadata_speakers_complete_df_encoded.Rdata")

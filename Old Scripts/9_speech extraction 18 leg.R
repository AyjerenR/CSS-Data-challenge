#THIS SCRIPT MUST BE RUN ON A MAC. The special character encodings only show up on a Mac and they comprise part of this script.
##1. Looping a script to extract speeches from each reading (1st, 2nd, 3rd -- which must be changed manually). Budget speeches will not be extracted with this loop.
##2. Looping a script to extract budget speeches from each reading (1st, 2nd, 3rd -- which must be changed manually). 

rm(list=ls())
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015/Individual legislature dataframes")
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015") #SOP
setwd("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/") #Jens
#setwd("~/Dropbox/Research/Sentiment Analysis of Parliamentary Speeches/Germany Parliamentary Debates 1949-2015/germanybundestagforjens/R scripts and data/Metadata 2005-2015/") # Oli
load("Individual legislature dataframes/18leg_metadata_speakers_complete_df_encoded.Rdata")
load("Individual legislature dataframes/18leg_metadata_df.Rdata")
table(metadata_df18$Drucksachennummer%in%metadata_speakers_complete_df18$Drucksachennummer)
table(metadata_speakers_complete_df18$Drucksachennummer%in%metadata_df18$Drucksachennummer)

# correct bill speaker in 18/3007
metadata_speakers_complete_df18$`Speaker name`[metadata_speakers_complete_df18$Drucksachennummer=="18/3007" & metadata_speakers_complete_df18$"Speaker name"=="Alexander  Ulrich" & !is.na(metadata_speakers_complete_df18$"Speaker name")]<-"Volker Ullrich"
metadata_speakers_complete_df18$`Speaker party`[metadata_speakers_complete_df18$Drucksachennummer=="18/3007" & metadata_speakers_complete_df18$"Speaker name"=="Alexander  Ulrich" & !is.na(metadata_speakers_complete_df18$"Speaker name")]<-"CDU/CSU"
setwd("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/18th legislature txts/") #Jens
#setwd("~/Dropbox/Research/Sentiment Analysis of Parliamentary Speeches/Germany Parliamentary Debates 1949-2015/germanybundestagforjens/R scripts and data/18th legislature txts/") # Oli
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/18th legislature txts")
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/18th legislature txts") #SOP
library(stringr)

##1. 
which(metadata_df18$Drucksachennummer=="18/11547")
metadata_speakers_complete_df18[metadata_speakers_complete_df18$Drucksachennummer=="18/12355",]
#loop for regular (non-budget) debates
complicated<-c() #initializing a vector to store the complicated (budget) debates to be dealt with later
#nth reading
i=65
#metadata_df18=metadata_df18[-which(metadata_df18$Plenary_protocol1=="18/225"&metadata_df18$Drucksachennummer=="18/9521"&!is.na(metadata_df18$Drucksachennummer)),]
for (i in 1:nrow(metadata_df18)) {
  bill<-metadata_df18[i, "Drucksachennummer"]
  plpr3<-metadata_df18[i, "Plenary_protocol3"] #both 'plpr3' and 'Plenary_protocol3' need to be changed to 'plpr1'/'plpr2' and 'Plenary_protocol1'/'Plenary_protocol2' etc. when changing the reading
  speakers.df <- subset(metadata_speakers_complete_df18, Drucksachennummer==bill & Reading==3 & `Type of speech`=="Rede") #subsetting the dataframe to the (if any) speakers on this bill and this reading -- 'Reading==n' must be changed for each reading
  if (nrow(speakers.df)>0) { #if there any speakes for this reading
    speaker_names<-str_trim(str_replace_all(speakers.df[, "Speaker name"], "[[:blank:]]{2}", " ")) #removing extra spaces in the speakernames
    speaker_names<-str_replace_all(speaker_names, "Wolfgang Ne[[:alpha:]]kovi[[:alpha:]]", "Wolfgang Ne[[:alpha:]]kovi[[:alpha:]]") #at various times, the speaker name extracted from the website does not exactly correspond to the plenary
    speaker_names<-str_replace_all(speaker_names, "Sevim Da[[:alpha:]]delen", "Sevim Da.delen")
    speaker_names<-str_replace_all(speaker_names, "Dr. h. c. Jürgen Koppelin", "Jürgen Koppelin")
    speaker_names<-str_replace_all(speaker_names, "Sabine Zimmermann Zwickau", "Sabine Zimmermann")
    speaker_names<-str_replace_all(speaker_names, "Dr. h.c. Gernot Erler", "Gernot Erler")
    speaker_names<-str_replace_all(speaker_names, "Doroth[[:alpha:]]e Menzner", "Doroth[[:alpha:]]e Menzner")
    speaker_names<-str_replace_all(speaker_names, "Prof.", "")
    speaker_names<-str_replace_all(speaker_names, "Marianne Schieder Schwandorf", "Marianne Schieder")
    speaker_names<-str_replace_all(speaker_names, "Volkmar Uwe Vogel Kleinsaara", "Volkmar Uwe Vogel")
    speaker_names<-str_replace_all(speaker_names, "Marcus Weinberg Hamburg", "Marcus Weinberg")
    speaker_names<-str_replace_all(speaker_names, "Michael Georg Link", "Michael Link")
    speaker_names<-str_replace_all(speaker_names, "Armin Schuster Weil am Rhein", "Armin Schuster")
    speaker_names<-str_replace_all(speaker_names, "Aydan Özoguz", "Aydan Özo.uz")
    speaker_names<-str_replace_all(speaker_names, "Nadine Schön St. Wendel", "Nadine Schön")
    speaker_names<-str_replace_all(speaker_names, "Dagmar G.  Wöhrl", "Dagmar Wöhrl")
    speaker_names<-str_replace_all(speaker_names, "Sabine Weiss Wesel I", "Sabine  Weiss")
    if (bill=="18/3007" & "Alexander Ulrich"%in%speaker_names) 
      speaker_names<-str_replace_all(speaker_names, "Alexander Ulrich", "Volker Ullrich")
    if (str_detect(metadata_df18[i, "Plenary_protocol3"], ";")) {  #if it has multiple plenary sessions for each reading, remove and save for complicated (budget) loop -- 'Plenary_protocol3' must be changed for each reading
      complicated<-c(complicated, i)
    } else { #proceed as normal
      text.name<-paste0(unlist(str_extract_all(plpr3,"[[:digit:]]")), collapse="") #'plpr3' must be changed for each reading
      if (nchar(text.name)==4) {text.name<-paste0(str_sub(text.name, 1,2), "0", str_sub(text.name, 3,4))} #correcting textname to match nomenclature of plenary textfiles
      if (nchar(text.name)==3) {text.name<-paste0(str_sub(text.name, 1,2), "00", str_sub(text.name, 3))}
      if (text.name>18181) { #only have plenaries up to the 181st session
       
        fulltext<-readLines(paste0(text.name, ".txt"))
        debate <- fulltext[grep("Beginn:",fulltext,value=F):length(fulltext)]
        debate<-debate[grep(bill,debate,value=F)[1]:length(debate)]
        debate<-(str_replace_all(debate, "[(][[:upper:]][)]", "")) #remove [A][B]..etc which show up as paragraph markers
        
        combined<-paste(debate,collapse="Olivia") #collapse all the lines of the debate into one so that grep can run across lines, as interjections are usually spread across lines. (arbitrary choice of name)
        #want to remove the brackets that go (CAPITALlowercase...anything (must include a space) until end of bracket. -- these are the interjections
        combined2<-(str_replace_all(combined, "\\([[:upper:]][[:lower:]][^)]*[[:space:]].+?\\)", "")) 
        #removing interjections which I know don't include a space
        combined2<-(str_replace_all(combined2, "\\(Beifall\\)", ""))
        combined2<-(str_replace_all(combined2, "\\(Heiterkeit\\)", ""))
        debate.clean<-unlist(strsplit(combined2, "Olivia")) #now split the debate back again
        debate.clean<-debate.clean[debate.clean!=""] #removing empty lines
        debate.clean<-debate.clean[debate.clean!=" "]
        
        for (j in 1:length(speaker_names)) {
          if ((j==1)&(j!=length(speaker_names))) { #if the speaker is the first one and not simultaneously the last one (e.g. not only one speaker)
            pattern<-c(unlist(str_split(speaker_names[j], " |\\.")), ":") #creating a pattern from the speaker name and ':'
            ix <- Reduce(`&`, lapply(pattern, grepl, debate.clean))  # # searching for each element of the pattern and reducing search results to the results that match all elements 
            start.marker<-which(ix==TRUE)[1] #start marker is the first search result that matches all elements
            debate.clean<-debate.clean[start.marker:length(debate.clean)] #subsetting the debate to begin when the speaker begins speaking
            pattern2<-c(unlist(str_split(speaker_names[j+1], " |\\.")), ":") #creating a second pattern from the subsequent  speaker name and ':'
            ix2 <- Reduce(`&`, lapply(pattern2, grepl, debate.clean))  
            end.marker<-which(ix2==TRUE)[1]
          }
          if ((j>1)&(j!=length(speaker_names))) { #if the speaker is not the first one and also not the last one
            start.marker<-end.marker #the start of the second speaker is actually the same value that marked the end of the previous debate
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            pattern2<-c(unlist(str_split(speaker_names[j+1], " |\\.")), ":")
            ix2 <- Reduce(`&`, lapply(pattern2, grepl, debate.clean))  # This does the trick 
            end.marker<-which(ix2==TRUE)[1]
          }
          if ((j==length(speaker_names))&(j!=1)) { #if the speaker is the last one and not simultaneously the last one (e.g. not only one speaker)
            start.marker<-end.marker
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            end.marker<-grep("Ich schließe.+?die.+?Aussprache|Ich schließe die erste Beratung|komme.+?Punkt.+?[[:digit:]].+?Tagesordnung|ich komme.+?nunmehr.+?Tagesordnung|Wir kommen zu Einzelplan|Schluß der Sitzung|schließe die Sitzung|schließe ich die Aussprache|Ich schließe.+?Beratung|ich die Aussprache schließen|ich schließe die Aussprache|schließe ich die Aussprache|Die Aussprache ist geschlossen|Tagesordnungspunkt\\s[[:digit:]]|Tagesordnungspunkt\\sV|Tagesordnungspunkte\\s[[:digit:]]|Tagesordnungspunkten\\s[[:digit:]]|Die Sitzung ist geschlossen|Schluß unserer Tagesordnung|Tagesordnung aufgeführten Ausschüsse zu überweisen|Ich kann also die Aussprache schließen.|Weitere Wortmeldungen liegen nicht vor|ohne Debatte|Ich rufe Punkt [[:digit:]]{1,2}|Ich rufe nunmehr Punkt [[:digit:]]{1,2} der Tagesordnung|Ich rufe.+?Zusatzpunkte|Ich rufe die Punkte [[:digit:]]|Ich rufe.+?Punkt [[:digit:]]|Das Wort wird.+?nicht|Ich rufe.+?Zusatzpunkt [[:digit:]]|Ich berufe nunmehr|Ich.+?rufe.+?Punkt.+?[[:digit:]].+?Tagesordnung|Wir kommen zu Punkt [[:digit:]]|Ende der Rednerliste",debate.clean,value=F)[1]
          }
          if ((j==length(speaker_names))&(j==1)) { #if the speaker is simulatenously the first and last speaker (e.g. only one speaker)
            pattern<-c(unlist(str_split(speaker_names[j], " |\\.")), ":")
            ix <- Reduce(`&`, lapply(pattern, grepl, debate.clean))   
            start.marker<-which(ix==TRUE)[1]
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            end.marker<-grep("Ich schließe.+?die.+?Aussprache|Ich schließe die erste Beratung|komme.+?Punkt.+?[[:digit:]].+?Tagesordnung|ich komme.+?nunmehr.+?Tagesordnung|Wir kommen zu Einzelplan|Schluß der Sitzung|schließe die Sitzung|schließe ich die Aussprache|Ich schließe.+?Beratung|ich die Aussprache schließen|ich schließe die Aussprache|schließe ich die Aussprache|Die Aussprache ist geschlossen|Tagesordnungspunkt\\s[[:digit:]]|Tagesordnungspunkt\\sV|Tagesordnungspunkte\\s[[:digit:]]|Tagesordnungspunkten\\s[[:digit:]]|Die Sitzung ist geschlossen|Schluß unserer Tagesordnung|Tagesordnung aufgeführten Ausschüsse zu überweisen|Ich kann also die Aussprache schließen.|Weitere Wortmeldungen liegen nicht vor|ohne Debatte|Ich rufe Punkt [[:digit:]]{1,2}|Ich rufe nunmehr Punkt [[:digit:]]{1,2} der Tagesordnung|Ich rufe.+?Zusatzpunkte|Ich rufe die Punkte [[:digit:]]|Ich rufe.+?Punkt [[:digit:]]|Das Wort wird.+?nicht|Ich rufe.+?Zusatzpunkt [[:digit:]]|Ich berufe nunmehr|Ich.+?rufe.+?Punkt.+?[[:digit:]].+?Tagesordnung|Wir kommen zu Punkt [[:digit:]]|Ende der Rednerliste",debate.clean,value=F)[1]
          }
          
          speech<-debate.clean[1:(end.marker-1)]
          speech<-str_trim(speech)
          speakername<-str_extract(speech[1], ".+?:") #taking 'speakername' as exactly what comes before the colon - will be useful for questions
          
          speaker_name<-str_trim(str_replace_all(speakers.df[j, "Speaker name"], "[[:blank:]]{2}", " ")) #nicely formatting the speaker_name for the textfile name
          speaker_name<-gsub(" ", "_", speaker_name)
          speaker_name<-gsub("\\(","",speaker_name)
          speaker_name<-gsub("\\)","",speaker_name)
          speaker_name<-gsub("\\)","",speaker_name)
          speaker_name<-gsub("\\.","",speaker_name)
          
          party<-str_trim(str_replace_all(speakers.df[j, "Speaker party"], "[[:blank:]]{2}", " ")) #nicely formatting the party for the textfile name
          party <- gsub("\\(","",party)
          party<-gsub(" ", "_", party)
          party <- gsub("\\)","",party)
          party <- gsub("\\:","",party)
          party <- gsub("\\/","",party)
          
          spkr_reading<-speakers.df[j, "Speaker_in_reading"]
          speech_id<-speakers.df[j, "Speech_ID"]
          
          filename <- paste0(gsub("\\/", "_", bill),"__", speech_id, "__", "3rd_reading", "__", speaker_name, "__", party, "__", spkr_reading,".txt") #'3rd_reading' needs to be manually changed for each reading
          
          questions_text<-grep("^[[:upper:]].+?[(].+?[)][:]",speech, value=T) #storing the text of when anyone speaks
          questions_positions<-grep("^[[:upper:]].+?\\(.+?\\):",speech,value=F) #storing the location of when anyone speaks
          questions<-questions_positions[-(grep(speakername, questions_text, fixed=T, value=F))] #removing those that are the speaker itself
          answers<-grep(speakername,speech,value=F, fixed=T) #locations of where speaker speaks
          
          if(length(questions)>0) { #removing the text between questions and answers (if it exists)
            for (m in 1:(length(questions))) {
              if (length(which(answers>questions[m]))>0) {
                speech[(questions[m]-2):(answers[which(answers>questions[m])[1]]-1)] <-c("")
              }
            }
            if ((questions[length(questions)])>(answers[length(answers)])) {
              speech[(questions[length(questions)]):length(speech)]<-c("")
            }
          }
          
          president.mentions<-grep("(^|^[[:blank:]])(Präsident|Präsidentin|Vizepräsidentin|Vizepräsident).+?:", speech)
          
          if(length(president.mentions)>0) { #removing the text between a presidential interjection and when the speaker resumes (if any)
            for (m in 1:(length(president.mentions))) {
              if (length(which(answers>president.mentions[m]))>0) {
                speech[(president.mentions[m]):(answers[which(answers>president.mentions[m])[1]]-1)] <-c("")
              }
            }
            if ((president.mentions[length(president.mentions)])>(answers[length(answers)])) { #removing the text after the president introduces the next speaker (at the end of the speech)
              speech[(president.mentions[length(president.mentions)]):length(speech)]<-c("")
            }
            for (p in 1:(length(president.mentions))) {
              speech[president.mentions[p]]<-c("")
            }
          }
          
          
          writeLines(speech, con=paste0("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/Extracted Speeches_new", "/",filename))
        #  writeLines(speech, con=paste0("~/Dropbox/Research/Sentiment Analysis of Parliamentary Speeches/Germany Parliamentary Debates 1949-2015/germanybundestagforjens/R scripts and data/Metadata 2005-2015/Extracted Speeches_new", "/",filename))
          
            speakerelement<-NULL
          speaker<-NULL
          filename<-NULL
          name<-NULL
          party<-NULL
          speakername<-NULL
          comma<-NULL
        }
      }
    }
  }
}
#complicated for first reading:190, 356, 509, 565
#complicated for second reading: 137 190 356 509 565

##2.
#budget debates - first and second reading
complicated<-c(137, 190, 356, 509, 565)
for (i in 1:length(complicated)) {
  i<-complicated[i]
  bill<-metadata_df18[i, "Drucksachennummer"]
  plpr3<-unlist(str_split(metadata_df18[i, "Plenary_protocol3"], ";"))
  for (q in 1:length(plpr3)) { #for each session in each reading
    speakers.df <- subset(metadata_speakers_complete_df18, Drucksachennummer==bill & Reading==3 & `Type of speech`=="Rede" & Comments==paste0("Reading 3: Session:",plpr3[q])) #extracting the information of the speakers in this specific session
    if (nrow(speakers.df)>0) {
      speaker_names<-str_trim(str_replace_all(speakers.df[, "Speaker name"], "[[:blank:]]{2}", " "))
      peaker_names<-str_replace_all(speaker_names, "Wolfgang Ne[[:alpha:]]kovi[[:alpha:]]", "Wolfgang Ne[[:alpha:]]kovi[[:alpha:]]")
      speaker_names<-str_replace_all(speaker_names, "Sevim Da[[:alpha:]]delen", "Sevim Da.delen")
      speaker_names<-str_replace_all(speaker_names, "Dr. h. c. Jürgen Koppelin", "Jürgen Koppelin")
      speaker_names<-str_replace_all(speaker_names, "Sabine Zimmermann Zwickau", "Sabine Zimmermann")
      speaker_names<-str_replace_all(speaker_names, "Dr. h.c. Gernot Erler", "Gernot Erler")
      speaker_names<-str_replace_all(speaker_names, "Doroth[[:alpha:]]e Menzner", "Doroth[[:alpha:]]e Menzner")
      speaker_names<-str_replace_all(speaker_names, "Prof.", "")
      speaker_names<-str_replace_all(speaker_names, "Marianne Schieder Schwandorf", "Marianne Schieder")
      speaker_names<-str_replace_all(speaker_names, "Volkmar Uwe Vogel Kleinsaara", "Volkmar Uwe Vogel")
      speaker_names<-str_replace_all(speaker_names, "Marcus Weinberg Hamburg", "Marcus Weinberg")
      speaker_names<-str_replace_all(speaker_names, "Michael Georg Link", "Michael Link")
      speaker_names<-str_replace_all(speaker_names, "Armin Schuster Weil am Rhein", "Armin Schuster")
      speaker_names<-str_replace_all(speaker_names, "Aydan Özoguz", "Aydan Özo.uz")
      speaker_names<-str_replace_all(speaker_names, "Nadine Schön St. Wendel", "Nadine Schön")
      speaker_names<-str_replace_all(speaker_names, "Dagmar G.  Wöhrl", "Dagmar Wöhrl")
      speaker_names<-str_replace_all(speaker_names, "Sabine Weiss Wesel I", "Sabine  Weiss")
      
      if (bill=="18/9200" & "Alexander Ulrich"%in%speaker_names) 
        speaker_names<-str_replace_all(speaker_names, "Alexander Ulrich", "Volker Ullrich")
      
      text.name<-paste0(unlist(str_extract_all(plpr3[q],"[[:digit:]]")), collapse="")
      if (nchar(text.name)==4) {text.name<-paste0(str_sub(text.name, 1,2), "0", str_sub(text.name, 3,4))}
      if (nchar(text.name)==3) {text.name<-paste0(str_sub(text.name, 1,2), "00", str_sub(text.name, 3))}
      if (text.name==18029 & bill=="18/700") {speaker_names[speaker_names=="Michael Brand Fulda"]<-"Michael Brand"}
      if (text.name>18181) {
        fulltext<-readLines(paste0(text.name, ".txt"))
        debate <- fulltext[grep("Beginn:|Beginn [[:digit:]]([[:digit:]]|[[:space:]]|\\.)+Uhr",fulltext,value=F):length(fulltext)]
        debate<-debate[grep(bill,debate,value=F)[1]:length(debate)]
        debate<-(str_replace_all(debate, "[(][[:upper:]][)]", ""))
        
        combined<-paste(debate,collapse="Olivia")
        #want to remove the brackets that go (CAPITALlowercase...anything (including a space) until end of bracket.
        combined2<-(str_replace_all(combined, "\\([[:upper:]][[:lower:]][^)]*[[:space:]].+?\\)", ""))
        
        #how to put it back together now? need some way of returning it
        combined2<-(str_replace_all(combined2, "\\(Beifall\\)", ""))
        combined2<-(str_replace_all(combined2, "\\(Heiterkeit\\)", ""))
        debate.clean<-unlist(strsplit(combined2, "Olivia"))
        debate.clean<-debate.clean[debate.clean!=""]
        debate.clean<-debate.clean[debate.clean!=" "]
        
        for (j in 1:length(speaker_names)) {
          if ((j==1)&(j!=length(speaker_names))) {
            pattern<-c(unlist(str_split(speaker_names[j], " |\\.")), ":")
            #pattern<-c(unlist(str_split(speaker_names[j], " ")), unlist(str_split(speaker_parties[j], " |\\/")), ":")
            ix <- Reduce(`&`, lapply(pattern, grepl, debate.clean))  # This does the trick 
            start.marker<-which(ix==TRUE)[1]
            #start.marker<-agrep(speaker_names[j], debate.clean, max.distance=0.21, value=F) #finding instances of speaker name in the text
            #start.marker<-start.marker[grep(":", debate.clean[start.marker])] [1] #making sure to choose one with a colon, and the first one
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            pattern2<-c(unlist(str_split(speaker_names[j+1], " |\\.")), ":") 
            #pattern2<-c(unlist(str_split(speaker_names[j+1], " ")), unlist(str_split(speaker_parties[j+1], " |\\/")), ":") 
            ix2 <- Reduce(`&`, lapply(pattern2, grepl, debate.clean))  # This does the trick 
            end.marker<-which(ix2==TRUE)[1]
            #end.marker<-agrep(speaker_names[j+1], debate.clean, max.distance=0.06, value=F) #finding instances of speaker name in the text
            #end.marker<-end.marker[grep(":", debate.clean[end.marker])] [1] #making sure to choose one with a colon, and the first one
          }
          if ((j>1)&(j!=length(speaker_names))) {
            start.marker<-end.marker
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            #pattern2<-c(unlist(str_split(speaker_names[j+1], " ")), unlist(str_split(speaker_parties[j+1], " |\\/")), ":")
            pattern2<-c(unlist(str_split(speaker_names[j+1], " |\\.")), ":")
            ix2 <- Reduce(`&`, lapply(pattern2, grepl, debate.clean))  # This does the trick 
            end.marker<-which(ix2==TRUE)[1]
            #end.marker<-agrep(speaker_names[j+1], debate.clean, max.distance=0.21, value=F) #finding instances of speaker name in the text
            #end.marker<-end.marker[grep(":", debate.clean[end.marker])] [1] #making sure to choose one with a colon, and the first one
          }
          if ((j==length(speaker_names))&(j!=1)) {
            start.marker<-end.marker
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            end.marker<-grep("Ich schließe.+?die.+?Aussprache|Ich schließe die erste Beratung|komme.+?Punkt.+?[[:digit:]].+?Tagesordnung|ich komme.+?nunmehr.+?Tagesordnung|Wir kommen zu Einzelplan|Schluß der Sitzung|schließe die Sitzung|schließe ich die Aussprache|Ich schließe.+?Beratung|ich die Aussprache schließen|ich schließe die Aussprache|schließe ich die Aussprache|Die Aussprache ist geschlossen|Tagesordnungspunkt\\s[[:digit:]]|Tagesordnungspunkt\\sV|Tagesordnungspunkte\\s[[:digit:]]|Tagesordnungspunkten\\s[[:digit:]]|Die Sitzung ist geschlossen|Schluß unserer Tagesordnung|Tagesordnung aufgeführten Ausschüsse zu überweisen|Ich kann also die Aussprache schließen.|Weitere Wortmeldungen liegen nicht vor|ohne Debatte|Ich rufe Punkt [[:digit:]]{1,2}|Ich rufe nunmehr Punkt [[:digit:]]{1,2} der Tagesordnung|Ich rufe.+?Zusatzpunkte|Ich rufe die Punkte [[:digit:]]|Ich rufe.+?Punkt [[:digit:]]|Das Wort wird.+?nicht|Ich rufe.+?Zusatzpunkt [[:digit:]]|Ich berufe nunmehr|Ich.+?rufe.+?Punkt.+?[[:digit:]].+?Tagesordnung|Wir kommen zu Punkt [[:digit:]]|Ende der Rednerliste|Ich berufe die nächste Sitzung des Deutschen",debate.clean,value=F)[1]
          }
          if ((j==length(speaker_names))&(j==1)) {
            #pattern<-c(unlist(str_split(speaker_names[j], " ")), unlist(str_split(speaker_parties[j], " |\\/")), ":")
            pattern<-c(unlist(str_split(speaker_names[j], " |\\.")), ":")
            ix <- Reduce(`&`, lapply(pattern, grepl, debate.clean))  # This does the trick 
            start.marker<-which(ix==TRUE)[1]
            #start.marker<-agrep(speaker_names[j], debate.clean, max.distance=0.06, value=F) #finding instances of speaker name in the text
            #start.marker<-start.marker[grep(":", debate.clean[start.marker])] [1] #making sure to choose one with a colon, and the first one
            debate.clean<-debate.clean[start.marker:length(debate.clean)]
            end.marker<-grep("Ich schließe.+?die.+?Aussprache|Ich schließe die erste Beratung|komme.+?Punkt.+?[[:digit:]].+?Tagesordnung|ich komme.+?nunmehr.+?Tagesordnung|Wir kommen zu Einzelplan|Schluß der Sitzung|schließe die Sitzung|schließe ich die Aussprache|Ich schließe.+?Beratung|ich die Aussprache schließen|ich schließe die Aussprache|schließe ich die Aussprache|Die Aussprache ist geschlossen|Tagesordnungspunkt\\s[[:digit:]]|Tagesordnungspunkt\\sV|Tagesordnungspunkte\\s[[:digit:]]|Tagesordnungspunkten\\s[[:digit:]]|Die Sitzung ist geschlossen|Schluß unserer Tagesordnung|Tagesordnung aufgeführten Ausschüsse zu überweisen|Ich kann also die Aussprache schließen.|Weitere Wortmeldungen liegen nicht vor|ohne Debatte|Ich rufe Punkt [[:digit:]]{1,2}|Ich rufe nunmehr Punkt [[:digit:]]{1,2} der Tagesordnung|Ich rufe.+?Zusatzpunkte|Ich rufe die Punkte [[:digit:]]|Ich rufe.+?Punkt [[:digit:]]|Das Wort wird.+?nicht|Ich rufe.+?Zusatzpunkt [[:digit:]]|Ich berufe nunmehr|Ich.+?rufe.+?Punkt.+?[[:digit:]].+?Tagesordnung|Wir kommen zu Punkt [[:digit:]]|Ende der Rednerliste|Ich berufe die nächste Sitzung des Deutschen",debate.clean,value=F)[1]
          }
          
          speech<-debate.clean[1:(end.marker-1)]
          speech<-str_trim(speech)
          speakername<-str_extract(speech[1], ".+?:")
          
          speaker_name<-str_trim(str_replace_all(speakers.df[j, "Speaker name"], "[[:blank:]]{2}", " "))
          speaker_name<-gsub(" ", "_", speaker_name)
          speaker_name<-gsub("\\(","",speaker_name)
          speaker_name<-gsub("\\)","",speaker_name)
          speaker_name<-gsub("\\)","",speaker_name)
          speaker_name<-gsub("\\.","",speaker_name)
          
          party<-str_trim(str_replace_all(speakers.df[j, "Speaker party"], "[[:blank:]]{2}", " "))
          party <- gsub("\\(","",party)
          party<-gsub(" ", "_", party)
          party <- gsub("\\)","",party)
          party <- gsub("\\:","",party)
          party <- gsub("\\/","",party)
          
          spkr_reading<-speakers.df[j, "Speaker_in_reading"]
          speech_id<-speakers.df[j, "Speech_ID"]
          
          filename <- paste0(gsub("\\/", "_", bill),"__", speech_id, "__", "3rd_reading", "__", speaker_name, "__", party, "__", spkr_reading,".txt")
          
          questions_text<-grep("^[[:upper:]].+?[(].+?[)][:]",speech, value=T) #storing the text of when anyone speaks
          questions_positions<-grep("^[[:upper:]].+?\\(.+?\\):",speech,value=F) #storing the location of when anyone speaks
          questions<-questions_positions[-(grep(speakername, questions_text, fixed=T, value=F))] #removing those that are the speaker itself
          answers<-grep(speakername,speech,value=F, fixed=T) #locations of where speaker speaks
          
          if(length(questions)>0) {
            for (k in 1:(length(questions))) {
              if (length(which(answers>questions[k]))>0) {
                speech[(questions[k]-2):(answers[which(answers>questions[k])[1]]-1)] <-c("")
              }
            }
          }
          
          president.mentions<-grep("(^|^[[:blank:]])(Präsident|Präsidentin|Vizepräsidentin|Vizepräsident).+?:", speech)
          
          if(length(president.mentions)>0) {
            for (m in 1:(length(president.mentions))) {
              if (length(which(answers>president.mentions[m]))>0) {
                speech[(president.mentions[m]):(answers[which(answers>president.mentions[m])[1]]-1)] <-c("")
              }
            }
            if ((president.mentions[length(president.mentions)])>(answers[length(answers)])) {
              speech[(president.mentions[length(president.mentions)]):length(speech)]<-c("")
            }
            for (p in 1:(length(president.mentions))) {
              speech[president.mentions[p]]<-c("")
            }
          }
          writeLines(speech, con=paste0("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/Extracted Speeches_new", "/",filename))
          #writeLines(speech, con=paste0("~/Dropbox/Research/Sentiment Analysis of Parliamentary Speeches/Germany Parliamentary Debates 1949-2015/germanybundestagforjens/R scripts and data/Metadata 2005-2015/Extracted Speeches_new", "/",filename))
          speakerelement<-NULL
          speaker<-NULL
          filename<-NULL
          name<-NULL
          party<-NULL
          speakername<-NULL
          comma<-NULL
        }
      }
    }
  }
}














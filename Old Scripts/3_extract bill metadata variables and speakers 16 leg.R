##THIS SCRIPT MUST BE RUN ON A PC.
##The encoding settings for htmlParse and xmlParseDoc MUST be left as is. The special characters will not be formatted properly when viewed on a PC, but on a Mac they show up perfectly.
##This script works for the 16th and 17th legislature, although some variables need to be manually changed when switching legislatures.
##1: Looping a script to extract the bill and speaker variables for the non-budget bills. Budget bills will be saved and dealt with later
##2: Looping a script to extract the bill and speaker variables for the budget bills.


rm(list=ls())
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015")
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015") #SOP
setwd("~/../Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/") #Jens
load("stable_urls_16.Rdata") #loading vector of stable URLs - must be changed when switching legislatures.

library(XML)
library(RCurl)
library(stringr)
info<-debugGatherer() #not sure this (and below) is necessary..
handle<-getCurlHandle(cookiejar ="",cookiefile = "",followlocation=T, autoreferer=T, debugfunc=info$update, verbose=T, httpheader=list(from="olivia.podmore@gmail.com", 'user-agent'=str_c(R.version$ersion.string,", ", R.version$platform)))

##initializing empty dataframe for metadata
metadata_speakers_df16<-data.frame(matrix(ncol = 34))
names(metadata_speakers_df16)<-c("Legislative_period", "Static_URL", "Sachgebiete", "Title", 
                                 "Inhalt","EU_impulse_relationship", "Initiator",  "Aktueller_Stand", 
                                 "Gesta_number", "Zustimmungspflicht_proclamation", "Zustimmungspflicht_bill",
                                 "Drucksachennummer", "Date_of_initiative",  "Proclamation_date", 
                                 "Date_of_entry_into_force", "Date_of_BT_debate1", "Date_of_BT_debate2", 
                                 "Date_of_BT_debate3", "Plenary_protocol1", "Plenary_protocol2", "Plenary_protocol3", 
                                 "Comments", "Reading", "Speaker name", "Speaker type", "Speaker party", 
                                 "Type of speech","Beschluss","Beschlussseite","Beschlusstenor",
                                 "Beschlussdokument","Abstimmungsart","Mehrheit","Abstimmungsergebnis")
endrow<-0

##1.
#starting loop 
complicated<-c() #initializing a vector to store the bills with multiple plenary sessions for each debate (e.g. budget debates) 
for (i in 1:length(stable_urls)) { #'stable_urls17' must be changed when switching legislatures
  ##going to url
  url<-stable_urls[i] #storing first url
  parsed_file <- htmlParse(url) #parsing url - encoding must not be specified here.
  root<-xmlRoot(parsed_file) #storing the root node of the parsed url
  
  ##capturing xml output
  xml_text<-capture.output(root[[2]][[2]][[6]][[2]][[2]][[7]][[2]][[1]][[6]]) #storing the xml output (value of this node)
  xml_text_cut<-xml_text[-c(1:3, length(xml_text))]#removing first line of the output so that xml is read as xml, not a comment
  xml_text_combined<-paste(xml_text_cut, collapse="\n") #collapsing the xml text so that it reflects xml layout
  parsed_xml<-xmlParseDoc(xml_text_combined, asText=T, baseURL=url) #(re)parsing the xml content - encoding must not be specified here.
  
  ##bill variables
  #legislative period
  wahl<-xpathSApply(parsed_xml, path="//WAHLPERIODE", xmlValue)
  
  #Sachgebiete
  sach<-xpathSApply(parsed_xml, path="//SACHGEBIET", xmlValue)
  sach_collapsed<-paste(sach, collapse=";") #multiple subjects so they must be collapsed into one row
  
  #Title
  title<-xpathSApply(parsed_xml, path="//TITEL", xmlValue)
  
  #Inhalt
  inhalt<-xpathSApply(parsed_xml, path="//ABSTRAKT", xmlValue)
  
  #Initiator
  initiative<-xpathSApply(parsed_xml, path="//INITIATIVE", xmlValue)
  initiative_collapsed<-paste(initiative, collapse=";") #multiple authors, so they must be collapsed into one row
  
  #Aktueller Stand
  akt<-xpathSApply(parsed_xml, path="//AKTUELLER_STAND", xmlValue)
  
  #Gesta number
  gesta<-xpathSApply(parsed_xml, path="//GESTA_ORDNUNGSNUMMER", xmlValue)
  
  ##Zustimmungspflicht-proclamation and Zustimmungspflicht-bill
  #zust proclamation
  zust_proc<-xpathSApply(parsed_xml, path="//ZUSTIMMUNGSBEDUERFTIGKEIT[contains(text(), 'VerkÃ¼ndung')]", xmlValue)
  
  #zust bill
  zust_bill<-xpathSApply(parsed_xml, path="//ZUSTIMMUNGSBEDUERFTIGKEIT[contains(text(), 'Gesetzentwurf')]", xmlValue)
  
  ##Drucksachennummer
  drs_num<-xpathSApply(parsed_xml, path="//DRS_TYP[contains(text(), 'Gesetzentwurf')]/parent::WICHTIGE_DRUCKSACHE/DRS_HERAUSGEBER[contains(text(), 'BT')]/following-sibling::DRS_NUMMER", xmlValue)
  
  ##Date of initiative
  init_date<-xpathSApply(parsed_xml, path="//VORGANGSABLAUF/VORGANGSPOSITION[position()=1]/FUNDSTELLE", xmlValue)
  init_date_value<-str_sub(init_date, 1,10) #the date is the first 10 characters of the row
  
  ##Proclamation date
  ver_dates<-xpathSApply(parsed_xml, path="//VERKUENDUNG[contains(text(), 'Gesetz')]", xmlValue) #go to the Verkuendung node that contains 'Gesetz' to find the relevant date
  
  ##Date of entry into force
  inkraft<-xpathSApply(parsed_xml, path="//INKRAFTTRETEN", xmlValue)
  
  ##Plenary protocols and debate dates
  plpr1<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '1. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  plpr2<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '2. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  plpr3<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '3. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  if ((length(plpr1)>1)|(length(plpr2)>1)|(length(plpr3)>1)) { #if there are multiple first readings or second readings or third readings etc. it will be dealt with separately
   complicated<-c(complicated, i)
  } else { #otherwise, continue as usual
    myfunction<-function(x) { #creating a function to store the xmlvalues of the speakers of multiple nodes
      values<-xpathSApply(parsed_xml, x, xmlValue)
      qualities<-xpathSApply(parsed_xml, x, xmlName)
      data.frame(matrix(values,1,length(qualities), dimnames=list(c(""), c(qualities))), stringsAsFactors=F)
    }
    
    ##checking whether there are plenary debates (and hence speakers to extract)
    if (!(is.null(plpr1)&is.null(plpr2)&is.null(plpr3))) {  #if all th
      #checking whether there is a 1st plenary debate
      if (!is.null(plpr1)) {
        #first reading
        query1.2<-sprintf(paste0("//URHEBER[text()='1. Beratung']/following-sibling::FUNDSTELLE[contains(text(),", "'", plpr1, "'", ")]/ancestor::VORGANGSPOSITION/BESCHLUSS[position()=%s]/child::*"),1) #creating a query to extract the children of all the speaker nodes
        if(!is.null(xpathSApply(parsed_xml, query1.2, xmlValue))){
          beschluss1<-lapply(query1.2, myfunction)
        }
        number_speakers1<-length(xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER"), plpr1)))   #counting the number of speakers by printing all the nodes
        if (number_speakers1>0) { #if anyone spoke in the 1st debate
          query1<-sprintf(paste0("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[contains(text(),", "'", plpr1, "'", ")]/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER[position()=%s]/child::*"), 1:number_speakers1) #creating a query to extract the children of all the speaker nodes
          speaker_info1<-lapply(query1, myfunction)
          
          for (j in 1:number_speakers1) { #for each of the speakers
            metadata_speakers_df16[j+endrow,"Speaker name"]<-paste(speaker_info1[[j]][1,"PERSON_TITEL"], speaker_info1[[j]][1,"VORNAME"], speaker_info1[[j]][1,"NAMENSZUSATZ"], speaker_info1[[j]][1,"NACHNAME"], speaker_info1[[j]][1,"WAHLKREISZUSATZ"]) #extracting, pasting, and storing to the dataframe all the relevant qualities of a speaker name (title, first name, middle name, etc)
            metadata_speakers_df16[j+endrow, "Speaker type"]<-speaker_info1[[j]][1, "FUNKTION"]
            if (str_detect(speaker_info1[[j]][1, "FUNKTION"], "Bundeskanzl.|BundestagsprÃ¤s.|Bundestagspräs.")) {  #if the speaker is Angela Merkel or Norbert Lammert then the FRAKTION node is missing
              metadata_speakers_df16[j+endrow, "Speaker party"]<-"CDU/CSU"
            } else { #if it's not Merkel or Lammert, continue as normal..unless it's the 'Beauftr. d. Bundesregierung'
              if (str_detect(speaker_info1[[j]][1, "FUNKTION"], "Beauftr. d. Bundesregierung")) {
                metadata_speakers_df16[j+endrow, "Speaker party"]<-NA
              } else { #continue as normal
                metadata_speakers_df16[j+endrow, "Speaker party"]<-paste(speaker_info1[[j]][1, "FRAKTION"], speaker_info1[[j]][1, "RESSORT"], speaker_info1[[j]][1, "FUNKTIONSZUSATZ"])
              }
            }
            metadata_speakers_df16[j+endrow, "Type of speech"]<-speaker_info1[[j]][1, "AKTIVITAETSART"]
          if(!is.null(xpathSApply(parsed_xml, query1.2, xmlValue))){
            if("BESCHLUSSSEITE"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[j+endrow, "Beschlussseite"]<-beschluss1[[1]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[j+endrow, "Beschlusstenor"]<-beschluss1[[1]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[j+endrow, "Beschlussdokument"]<-beschluss1[[1]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[j+endrow, "Abstimmungsart"]<-beschluss1[[1]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[j+endrow, "Mehrheit"]<-beschluss1[[1]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[j+endrow, "Abstimmungsergebnis"]<-beschluss1[[1]]$ABSTIMMUNG_BEMERKUNG
            }
          }
          }
          metadata_speakers_df16[(1+endrow):(endrow + number_speakers1), "Reading"]<-1 #inputting 1 for the reading for all of the rows just initialized by the speakers
          endrow<-nrow(metadata_speakers_df16)
        } else { #if there are no speakers in the 1st debate, make note of it 
          metadata_speakers_df16[endrow+1, "Comments"] <-"No speakers in 1st debate"
          metadata_speakers_df16[endrow+1, "Reading"]<-1
          if(!is.null(xpathSApply(parsed_xml, query1.2, xmlValue))){
            if("BESCHLUSSSEITE"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlussseite"]<-beschluss1[[1]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlusstenor"]<-beschluss1[[1]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlussdokument"]<-beschluss1[[1]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[1+endrow, "Abstimmungsart"]<-beschluss1[[1]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[1+endrow, "Mehrheit"]<-beschluss1[[1]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss1[[1]])){
              metadata_speakers_df16[1+endrow, "Abstimmungsergebnis"]<-beschluss1[[1]]$ABSTIMMUNG_BEMERKUNG
            }
          }
          endrow<-nrow(metadata_speakers_df16)
          number_speakers1<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
        } 
      } else { #if there was not a 1st plenary debate, make note of it and input the relevant variables
        metadata_speakers_df16[endrow+1, "Comments"]<-"No 1st debate"
        metadata_speakers_df16[endrow+1, "Date_of_BT_debate1"]<-"No 1st debate"
        metadata_speakers_df16[endrow+1, "Reading"]<-1
        endrow<-nrow(metadata_speakers_df16)
        number_speakers1<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
      }
      
      if (!is.null(plpr2)) {
        #second reading
        number_speakers2<-length(xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER"), plpr2)))
        query2.2<-sprintf(paste0("//URHEBER[text()='2. Beratung']/following-sibling::FUNDSTELLE[contains(text(),", "'", plpr2, "'", ")]/ancestor::VORGANGSPOSITION/BESCHLUSS[position()=%s]/child::*"),1) #creating a query to extract the children of all the speaker nodes
        if(!is.null(xpathSApply(parsed_xml, query2.2, xmlValue))){
          beschluss2<-lapply(query2.2, myfunction)
        }
        if (number_speakers2>0) {
          query2<-sprintf(paste0("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[contains(text(),", "'", plpr2, "'", ")]/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER[position()=%s]/child::*"), 1:number_speakers2)
          
          speaker_info2<-lapply(query2, myfunction)
          
          for (k in 1:number_speakers2) {
            #metadata_speakers_df16[k+endrow,"Speaker name"]<-str_trim(paste(speaker_info2[[k]][1,"PERSON_TITEL"], speaker_info2[[k]][1,"VORNAME"], speaker_info2[[k]][1,"NAMENSZUSATZ"], speaker_info2[[k]][1,"NACHNAME"], speaker_info2[[k]][1,"WAHLKREISZUSATZ"])) #next idea is to subset within the list and then just paste into the dataframe
            metadata_speakers_df16[k+endrow,"Speaker name"]<-paste(speaker_info2[[k]][1,"PERSON_TITEL"], speaker_info2[[k]][1,"VORNAME"], speaker_info2[[k]][1,"NAMENSZUSATZ"], speaker_info2[[k]][1,"NACHNAME"], speaker_info2[[k]][1,"WAHLKREISZUSATZ"]) #next idea is to subset within the list and then just paste into the dataframe
            metadata_speakers_df16[k+endrow, "Speaker type"]<-speaker_info2[[k]][1, "FUNKTION"]
            if (str_detect(speaker_info2[[k]][1, "FUNKTION"], "Bundeskanzl.|BundestagsprÃ¤s.")) {  #if the speaker is Angela Merkel or Norbert Lammert then the FRAKTION node is missing
              metadata_speakers_df16[k+endrow, "Speaker party"]<-"CDU/CSU"
            } else { #if it's not Merkel or Lammert, continue as normal..unless it's the 'Beauftr. d. Bundesregierung'
              if (str_detect(speaker_info2[[k]][1, "FUNKTION"], "Beauftr. d. Bundesregierung")) {
                metadata_speakers_df16[k+endrow, "Speaker party"]<-NA
              } else { #continue as normal
                #metadata_speakers_df16[k+endrow, "Speaker party"]<-str_trim(paste(speaker_info2[[k]][1, "FRAKTION"], speaker_info2[[k]][1, "RESSORT"], speaker_info2[[k]][1, "FUNKTIONSZUSATZ"]))
                metadata_speakers_df16[k+endrow, "Speaker party"]<-paste(speaker_info2[[k]][1, "FRAKTION"], speaker_info2[[k]][1, "RESSORT"], speaker_info2[[k]][1, "FUNKTIONSZUSATZ"])
            
              }
            }
            metadata_speakers_df16[k+endrow, "Type of speech"]<-speaker_info2[[k]][1, "AKTIVITAETSART"]
            if(!is.null(xpathSApply(parsed_xml, query2.2, xmlValue))){
              if("BESCHLUSSSEITE"%in%names(beschluss2[[1]])){
                metadata_speakers_df16[k+endrow, "Beschlussseite"]<-beschluss2[[1]]$BESCHLUSSSEITE
              }
              if("BESCHLUSSTENOR"%in%names(beschluss2[[1]])){
                metadata_speakers_df16[k+endrow, "Beschlusstenor"]<-beschluss2[[1]]$BESCHLUSSTENOR
              }
              if("BEZUGSDOKUMENT"%in%names(beschluss2[[1]])){
                metadata_speakers_df16[k+endrow, "Beschlussdokument"]<-beschluss2[[1]]$BEZUGSDOKUMENT
              }
              if("ABSTIMMUNGSART"%in%names(beschluss2[[1]])){
                metadata_speakers_df16[k+endrow, "Abstimmungsart"]<-beschluss2[[1]]$ABSTIMMUNGSART
              }
              if("MEHRHEIT"%in%names(beschluss2[[1]])){
                metadata_speakers_df16[k+endrow, "Mehrheit"]<-beschluss2[[1]]$MEHRHEIT
              }
              if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss2[[1]])){
                metadata_speakers_df16[k+endrow, "Abstimmungsergebnis"]<-beschluss2[[1]]$ABSTIMMUNG_BEMERKUNG
              }
            }
          }
          metadata_speakers_df16[(1+endrow):(endrow + number_speakers2), "Reading"]<-2
          endrow<-nrow(metadata_speakers_df16)
        } else { #if there are no speakers in the 2nd debate, make note of it 
          metadata_speakers_df16[endrow+1, "Comments"]<-"No speakers in 2nd debate"
          metadata_speakers_df16[endrow+1, "Reading"]<-2
          if(!is.null(xpathSApply(parsed_xml, query2.2, xmlValue))){
            if("BESCHLUSSSEITE"%in%names(beschluss2[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlussseite"]<-beschluss2[[1]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss2[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlusstenor"]<-beschluss2[[1]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss2[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlussdokument"]<-beschluss2[[1]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss2[[1]])){
              metadata_speakers_df16[1+endrow, "Abstimmungsart"]<-beschluss2[[1]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss2[[1]])){
              metadata_speakers_df16[1+endrow, "Mehrheit"]<-beschluss2[[1]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss2[[1]])){
              metadata_speakers_df16[1+endrow, "Abstimmungsergebnis"]<-beschluss2[[1]]$ABSTIMMUNG_BEMERKUNG
            }
          }
          endrow<-nrow(metadata_speakers_df16)
          number_speakers2<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
        }
      } else { #if there is not a 2nd plenary debate, make a note of it
        metadata_speakers_df16[endrow+1, "Comments"]<-"No 2nd debate"
        metadata_speakers_df16[endrow+1, "Date_of_BT_debate2"]<-"No 2nd debate"
        metadata_speakers_df16[endrow+1, "Reading"]<-2
        endrow<-nrow(metadata_speakers_df16)
        number_speakers2<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
      }
      
      if (!is.null(plpr3)) {
        #third reading
        number_speakers3<-length(xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER"), plpr3)))
        query3.2<-sprintf(paste0("//URHEBER[text()='3. Beratung']/following-sibling::FUNDSTELLE[contains(text(),", "'", plpr3, "'", ")]/ancestor::VORGANGSPOSITION/BESCHLUSS[position()=%s]/child::*"),1) #creating a query to extract the children of all the speaker nodes
        if(!is.null(xpathSApply(parsed_xml, query3.2, xmlValue))){
          beschluss3<-lapply(query3.2, myfunction)
        }
        if (number_speakers3>0){
          query3<-sprintf(paste0("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[contains(text(),", "'", plpr3, "'", ")]/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER[position()=%s]/child::*"), 1:number_speakers3)
          
          speaker_info3<-lapply(query3, myfunction)
          
          for (m in 1:number_speakers3) {
            #metadata_speakers_df16[m+endrow, "Speaker name"]<-str_trim(paste(speaker_info3[[m]][1,"PERSON_TITEL"], speaker_info3[[m]][1,"VORNAME"], speaker_info3[[m]][1,"NAMENSZUSATZ"], speaker_info3[[m]][1,"NACHNAME"], speaker_info3[[m]][1,"WAHLKREISZUSATZ"])) #next idea is to subset within the list and then just paste into the dataframe
            metadata_speakers_df16[m+endrow,"Speaker name"]<-paste(speaker_info3[[m]][1,"PERSON_TITEL"], speaker_info3[[m]][1,"VORNAME"], speaker_info3[[m]][1,"NAMENSZUSATZ"], speaker_info3[[m]][1,"NACHNAME"], speaker_info3[[m]][1,"WAHLKREISZUSATZ"]) #next idea is to subset within the list and then just paste into the dataframe
            metadata_speakers_df16[m+endrow, "Speaker type"]<-speaker_info3[[m]][1, "FUNKTION"]
            if (str_detect(speaker_info3[[m]][1, "FUNKTION"], "Bundeskanzl.|BundestagsprÃ¤s.")) {  #if the speaker is Angela Merkel or Norbert Lammert then the FRAKTION node is missing
              metadata_speakers_df16[m+endrow, "Speaker party"]<-"CDU/CSU"
            } else { #if it's not Merkel or Lammert, continue as normal..unless it's the 'Beauftr. d. Bundesregierung'
              if (str_detect(speaker_info3[[m]][1, "FUNKTION"], "Beauftr. d. Bundesregierung")) {
                metadata_speakers_df16[m+endrow, "Speaker party"]<-NA
              } else { #continue as normal
                #metadata_speakers_df16[m+endrow, "Speaker party"]<-str_trim(paste(speaker_info3[[m]][1, "FRAKTION"], speaker_info3[[m]][1, "RESSORT"], speaker_info3[[m]][1, "FUNKTIONSZUSATZ"]))
                metadata_speakers_df16[m+endrow, "Speaker party"]<-paste(speaker_info3[[m]][1, "FRAKTION"], speaker_info3[[m]][1, "RESSORT"], speaker_info3[[m]][1, "FUNKTIONSZUSATZ"])
                
              }
            }
            metadata_speakers_df16[m+endrow, "Type of speech"]<-speaker_info3[[m]][1, "AKTIVITAETSART"]
            if(!is.null(xpathSApply(parsed_xml, query3.2, xmlValue))){
              if("BESCHLUSSSEITE"%in%names(beschluss3[[1]])){
                metadata_speakers_df16[m+endrow, "Beschlussseite"]<-beschluss3[[1]]$BESCHLUSSSEITE
              }
              if("BESCHLUSSTENOR"%in%names(beschluss3[[1]])){
                metadata_speakers_df16[m+endrow, "Beschlusstenor"]<-beschluss3[[1]]$BESCHLUSSTENOR
              }
              if("BEZUGSDOKUMENT"%in%names(beschluss3[[1]])){
                metadata_speakers_df16[m+endrow, "Beschlussdokument"]<-beschluss3[[1]]$BEZUGSDOKUMENT
              }
              if("ABSTIMMUNGSART"%in%names(beschluss3[[1]])){
                metadata_speakers_df16[m+endrow, "Abstimmungsart"]<-beschluss3[[1]]$ABSTIMMUNGSART
              }
              if("MEHRHEIT"%in%names(beschluss3[[1]])){
                metadata_speakers_df16[m+endrow, "Mehrheit"]<-beschluss3[[1]]$MEHRHEIT
              }
              if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss3[[1]])){
                metadata_speakers_df16[m+endrow, "Abstimmungsergebnis"]<-beschluss3[[1]]$ABSTIMMUNG_BEMERKUNG
              }
            }
          }
          metadata_speakers_df16[(1+endrow):(endrow + number_speakers3), "Reading"]<-3
          endrow<-nrow(metadata_speakers_df16)
        } else { #if there are no speakers in the 3rd debate, make note of it 
          metadata_speakers_df16[endrow+1, "Comments"]<-"No speakers in 3rd debate"
          metadata_speakers_df16[endrow+1, "Reading"]<-3
          if(!is.null(xpathSApply(parsed_xml, query3.2, xmlValue))){
            if("BESCHLUSSSEITE"%in%names(beschluss3[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlussseite"]<-beschluss3[[1]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss3[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlusstenor"]<-beschluss3[[1]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss3[[1]])){
              metadata_speakers_df16[1+endrow, "Beschlussdokument"]<-beschluss3[[1]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss3[[1]])){
              metadata_speakers_df16[1+endrow, "Abstimmungsart"]<-beschluss3[[1]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss3[[1]])){
              metadata_speakers_df16[1+endrow, "Mehrheit"]<-beschluss3[[1]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss3[[1]])){
              metadata_speakers_df16[1+endrow, "Abstimmungsergebnis"]<-beschluss3[[1]]$ABSTIMMUNG_BEMERKUNG
            }
          }
          endrow<-nrow(metadata_speakers_df16)
          number_speakers3<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
        }
      } else { #if there is not a 3rd plenary debate, make a note of it
        metadata_speakers_df16[endrow+1, "Comments"]<-"No 3rd debate"
        metadata_speakers_df16[endrow+1, "Date_of_BT_debate3"]<-"No 3rd debate"
        metadata_speakers_df16[endrow+1, "Reading"]<-3
        endrow<-nrow(metadata_speakers_df16)
        number_speakers3<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
      } 
      start<-endrow-(number_speakers1+number_speakers2+number_speakers3)+1
      #Inserting bill variables into the rows initialized by speakers (or lack thereof)
      metadata_speakers_df16[(start:endrow),"Legislative_period"]<-wahl
      metadata_speakers_df16[(start:endrow), "Static_URL"]<-url
      metadata_speakers_df16[(start:endrow),"Sachgebiete"]<-sach_collapsed
      metadata_speakers_df16[(start:endrow),"Title"]<-title
      metadata_speakers_df16[(start:endrow),"Inhalt"]<-inhalt
      metadata_speakers_df16[(start:endrow),"Initiator"]<-initiative_collapsed
      metadata_speakers_df16[(start:endrow),"Gesta_number"]<-gesta
      metadata_speakers_df16[(start:endrow),"Aktueller_Stand"]<-akt
      if (length(zust_proc)>0){
        if (str_detect(zust_proc, "Ja")) {metadata_speakers_df16[(start:endrow),"Zustimmungspflicht_proclamation"]<-"yes"}
        if (str_detect(zust_proc, "Nein")) {metadata_speakers_df16[(start:endrow),"Zustimmungspflicht_proclamation"]<-"no"}
      }
      if (length(zust_bill)>0) {
        if (str_detect(zust_bill, "Ja")) {metadata_speakers_df16[(start:endrow),"Zustimmungspflicht_bill"]<-"yes"}
        if (str_detect(zust_bill, "Nein")) {metadata_speakers_df16[(start:endrow),"Zustimmungspflicht_bill"]<-"no"}
      }
      if (length(drs_num)>0) { #if it exists, extract it
        metadata_speakers_df16[(start:endrow),"Drucksachennummer"]<-drs_num  
      }
      metadata_speakers_df16[(start:endrow),"Date_of_initiative"]<-init_date_value
      if (length(ver_dates)>0){
        ver_date_value<-str_extract(ver_dates, "[[:digit:]]([[:digit:]]|\\.){9}") #extracting the first date in this row
        metadata_speakers_df16[(start:endrow),"Proclamation_date"]<-ver_date_value
      }
      if (length(inkraft)>0){
        inkraft<-inkraft[1] #selecting the first entry
        inkraft_value<-str_sub(inkraft, 1,10) #the date is the first 10 characters of the row
        metadata_speakers_df16[(start:endrow),"Date_of_entry_into_force"]<-inkraft_value
      }
      #Plenary protocol and reading
      if (!is.null(plpr1)) { #if the protocol is not null, there will be a readind date associated 
        metadata_speakers_df16[(start:endrow), "Plenary_protocol1"]<-plpr1
        debate1<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr1), xmlValue)
        debate1_date<-substr(debate1, 1,10) #extracting the date (1st 10 characters) from that line
        metadata_speakers_df16[(start:endrow), "Date_of_BT_debate1"]<-debate1_date
      }
      if (!is.null(plpr2)) {
        metadata_speakers_df16[(start:endrow), "Plenary_protocol2"]<-plpr2
        debate2<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr2), xmlValue)
        debate2_date<-substr(debate2, 1,10) #extracting the date (1st 10 characters) from that line
        metadata_speakers_df16[(start:endrow), "Date_of_BT_debate2"]<-debate2_date
      }
      if (!is.null(plpr3)) {
        metadata_speakers_df16[(start:endrow), "Plenary_protocol3"]<-plpr3
        debate3<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr3), xmlValue)
        debate3_date<-substr(debate3, 1,10) #extracting the date (1st 10 characters) from that line
        metadata_speakers_df16[(start:endrow), "Date_of_BT_debate3"]<-debate3_date
      }
      endrow<-nrow(metadata_speakers_df16)
    } else {
      #filling in bill variables if there was no debate in the BT
      metadata_speakers_df16[endrow+1,"Legislative_period"]<-wahl
      metadata_speakers_df16[endrow+1, "Static_URL"]<-url
      metadata_speakers_df16[endrow+1,"Sachgebiete"]<-sach_collapsed
      metadata_speakers_df16[endrow+1,"Title"]<-title
      metadata_speakers_df16[endrow+1,"Inhalt"]<-inhalt
      metadata_speakers_df16[endrow+1,"Initiator"]<-initiative_collapsed
      metadata_speakers_df16[endrow+1,"Aktueller_Stand"]<-akt
      metadata_speakers_df16[endrow+1,"Gesta_number"]<-gesta
      if (length(zust_proc)>0){
        if (str_detect(zust_proc, "Ja")) {metadata_speakers_df16[endrow+1,"Zustimmungspflicht_proclamation"]<-"yes"}
        if (str_detect(zust_proc, "Nein")) {metadata_speakers_df16[endrow+1,"Zustimmungspflicht_proclamation"]<-"no"}
      }
      if (length(zust_bill)>0) {
        if (str_detect(zust_bill, "Ja")) {metadata_speakers_df16[endrow+1,"Zustimmungspflicht_bill"]<-"yes"}
        if (str_detect(zust_bill, "Nein")) {metadata_speakers_df16[endrow+1,"Zustimmungspflicht_bill"]<-"no"}
      }
      if (length(drs_num)>0) { #if it exists, extract it
        metadata_speakers_df16[endrow+1,"Drucksachennummer"]<-drs_num  
      }
      metadata_speakers_df16[endrow+1,"Date_of_initiative"]<-init_date_value
      if (length(ver_dates)>0){
        ver_date_value<-str_extract(ver_dates, "[[:digit:]]([[:digit:]]|\\.){9}") #extracting the first date in this row
        metadata_speakers_df16[endrow+1,"Proclamation_date"]<-ver_date_value
      }
      if (length(inkraft)>0){
        inkraft<-inkraft[1] #selecting the first entry
        inkraft_value<-str_sub(inkraft, 1,10) #the date is the first 10 characters of the row
        metadata_speakers_df16[endrow+1,"Date_of_entry_into_force"]<-inkraft_value
      }
      metadata_speakers_df16[endrow+1, "Comments"]<-"No debates"
      metadata_speakers_df16[endrow+1, "Date_of_BT_debate1"]<-"No 1st debate"
      metadata_speakers_df16[endrow+1, "Date_of_BT_debate2"]<-"No 2nd debate"
      metadata_speakers_df16[endrow+1, "Date_of_BT_debate3"]<-"No 3rd debate"
    }
    endrow<-nrow(metadata_speakers_df16)
  }
  print(i/length(stable_urls))
}

save(metadata_speakers_df16, file="16leg_metadata_speakers_df_encoded.Rdata")
save(complicated, file="complicated.Rdata")

myfunction<-function(x) {  #reloading the function if the workspace has been cleared
  values<-xpathSApply(parsed_xml, x, xmlValue)
  qualities<-xpathSApply(parsed_xml, x, xmlName)
  data.frame(matrix(values,1,length(qualities), dimnames=list(c(""), c(qualities))), stringsAsFactors=F) ##did it!!!!
}

load("complicated.Rdata")

##2. 
#initializing empty dataframe for  complicated (budget) bills
metadata_speakers_comp_df16<-data.frame(matrix(ncol = 34))
names(metadata_speakers_comp_df16)<-c("Legislative_period", "Static_URL", "Sachgebiete", "Title", 
                                      "Inhalt","EU_impulse_relationship", "Initiator", "Aktueller_Stand", 
                                      "Gesta_number", "Zustimmungspflicht_proclamation", "Zustimmungspflicht_bill", 
                                      "Drucksachennummer", "Date_of_initiative",  "Proclamation_date", 
                                      "Date_of_entry_into_force", "Date_of_BT_debate1", "Date_of_BT_debate2", 
                                      "Date_of_BT_debate3", "Plenary_protocol1", "Plenary_protocol2", 
                                      "Plenary_protocol3", "Comments", "Reading", "Speaker name", "Speaker type", 
                                      "Speaker party", "Type of speech","Beschluss","Beschlussseite","Beschlusstenor",
                                      "Beschlussdokument","Abstimmungsart","Mehrheit","Abstimmungsergebnis")

endrow<-0

for (n in 1:length(complicated)) {
  ##going to url
  url<-stable_urls[complicated[n]] #storing the url of the relevant row
  parsed_file <- htmlParse(url) #parsing url
  root<-xmlRoot(parsed_file) #storing the root node of the parsed url
  
  ##capturing xml output
  xml_text<-capture.output(root[[2]][[2]][[6]][[2]][[2]][[7]][[2]][[1]][[6]]) #storing the xml output (value of this node)
  xml_text_cut<-xml_text[-c(1:3, length(xml_text))]#removing first line of the output so that xml is read as xml, not a comment
  xml_text_combined<-paste(xml_text_cut, collapse="\n") #collapsing the xml text so that it reflects xml layout
  parsed_xml<-xmlParseDoc(xml_text_combined, asText=T, baseURL=url) #(re)parsing the xml content
  
  ##bill variables
  #legislative period
  wahl<-xpathSApply(parsed_xml, path="//WAHLPERIODE", xmlValue)
  
  #Sachgebiete
  sach<-xpathSApply(parsed_xml, path="//SACHGEBIET", xmlValue)
  sach_collapsed<-paste(sach, collapse=";") #multiple subjects so they must be collapsed into one row
  
  #Title
  title<-xpathSApply(parsed_xml, path="//TITEL", xmlValue)
  
  #Inhalt
  inhalt<-xpathSApply(parsed_xml, path="//ABSTRAKT", xmlValue)
  
  #Initiator
  initiative<-xpathSApply(parsed_xml, path="//INITIATIVE", xmlValue)
  initiative_collapsed<-paste(initiative, collapse=";") #multiple authors, so they must be collapsed into one row
  
  #Aktueller Stand
  akt<-xpathSApply(parsed_xml, path="//AKTUELLER_STAND", xmlValue)
  
  #Gesta number
  gesta<-xpathSApply(parsed_xml, path="//GESTA_ORDNUNGSNUMMER", xmlValue)
  
  ##Zustimmungspflicht-proclamation and Zustimmungspflicht-bill
  #zust proclamation
  zust_proc<-xpathSApply(parsed_xml, path="//ZUSTIMMUNGSBEDUERFTIGKEIT[contains(text(), 'VerkÃ¼ndung')]", xmlValue)
  
  #zust bill
  zust_bill<-xpathSApply(parsed_xml, path="//ZUSTIMMUNGSBEDUERFTIGKEIT[contains(text(), 'Gesetzentwurf')]", xmlValue)
  
  ##Drucksachennummer
  drs_num<-xpathSApply(parsed_xml, path="//DRS_TYP[contains(text(), 'Gesetzentwurf')]/parent::WICHTIGE_DRUCKSACHE/DRS_HERAUSGEBER[contains(text(), 'BT')]/following-sibling::DRS_NUMMER", xmlValue)
  
  ##Date of initiative
  init_date<-xpathSApply(parsed_xml, path="//VORGANGSABLAUF/VORGANGSPOSITION[position()=1]/FUNDSTELLE", xmlValue)
  init_date_value<-str_sub(init_date, 1,10) #the date is the first 10 characters of the row
  
  ##Proclamation date
  ver_dates<-xpathSApply(parsed_xml, path="//VERKUENDUNG[contains(text(), 'Gesetz')]", xmlValue) #go to the Verkuendung node that contains 'Gesetz' to find the relevant date
  
  ##Date of entry into force
  inkraft<-xpathSApply(parsed_xml, path="//INKRAFTTRETEN", xmlValue)
  
  ##Plenary protocols and debate dates
  plpr1<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '1. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  plpr1<-unique(plpr1) #because there are so many reading sessions, the plenary protocol number is often repeated
  
  plpr2<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '2. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  plpr2<-unique(plpr2)
  
  plpr3<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '3. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  plpr3<-unique(plpr3)
  
  myfunction2<-function(x) { #creating some functions to help sort out the number of children nodes
    children<-xpathSApply(parsed_xml, x)
    number_children<-length(children)
  }
  
  myfunction3<-function(x) {
    1:x
  }
  
  #first debate(s)
  total_speakers1<-0  #because there are multiple sessions of each reading, must keep track of the overall number of speakers
  if (!is.null(plpr1)) {
    for (p in 1:length(plpr1)) { #for each session of first reading
        meeting_ids<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr1[p]), xmlValue) #extracting the section of the session that this specific reading was dedicated to
        meeting_ids<-sort(meeting_ids) #sorting the sessions alphabetically/numerically so the first speaker is always extracted first
        #checking the number of children (length) for all of the sessions
        query1a<-sprintf("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER", meeting_ids)
        number_children_list<-lapply(query1a, myfunction2)  
        #selecting only those ones with children greater than zero
        positive_children<-which(number_children_list>0) #shows which sessions had children (speakers) 
        query1.2a<-sprintf("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/BESCHLUSS",meeting_ids) #creating a query to extract the children of all the speaker nodes
        if(!is.null(xpathSApply(parsed_xml, query1.2a, xmlValue))){
          beschluss1<-lapply(query1.2a, myfunction)
        }
        if (length(positive_children)>0) { #if there were any children (speakers)
          positive_children_list<-number_children_list[positive_children] #subsetting the list of children (speakers) to only include those with positive numbers of children (speakers)
          #creating vectors of their lengths to extract by position
          full_children<-lapply(positive_children_list, myfunction3)
          #now creating a new extraction query from each of these vectors
          positive_meeting_ids<-meeting_ids[positive_children] #subsetting the the meeting_ids to include only those with children (speakers)
          myfunction4<-function(x) {
            sprintf("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER[position()=%s]/child::*", positive_meeting_ids[x], full_children[[x]])
          }
          query1b<-unlist(lapply(1:length(positive_meeting_ids),myfunction4))
          speaker_info1<-lapply(query1b, myfunction) #creating a mini dataframe of the extracted speaker info
          for (q in 1:length(query1b)) {
            metadata_speakers_comp_df16[q+endrow,"Speaker name"]<-paste(speaker_info1[[q]][1,"PERSON_TITEL"], speaker_info1[[q]][1,"VORNAME"], speaker_info1[[q]][1,"NAMENSZUSATZ"], speaker_info1[[q]][1,"NACHNAME"], speaker_info1[[q]][1,"WAHLKREISZUSATZ"]) #next idea is to subset within the list and then just paste into the dataframe
            metadata_speakers_comp_df16[q+endrow, "Speaker type"]<-speaker_info1[[q]][1, "FUNKTION"]
            if (str_detect(speaker_info1[[q]][1, "FUNKTION"], "Bundeskanzl.|BundestagsprÃ¤s.")) {  #if the speaker is Angela Merkel or Norbert Lammert then the FRAKTION node is missing
              metadata_speakers_comp_df16[q+endrow, "Speaker party"]<-"CDU/CSU"
            } else { #if it's not Merkel or Lammert, continue as normal
              metadata_speakers_comp_df16[q+endrow, "Speaker party"]<-paste(speaker_info1[[q]][1, "FRAKTION"], speaker_info1[[q]][1, "RESSORT"], speaker_info1[[q]][1, "FUNKTIONSZUSATZ"])
            }
            metadata_speakers_comp_df16[q+endrow, "Type of speech"]<-speaker_info1[[q]][1, "AKTIVITAETSART"]
            if(!is.null(xpathSApply(parsed_xml, query1.2a, xmlValue))){
              if("BESCHLUSS"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Beschluss"]<-beschluss1[[length(beschluss1)]]$BESCHLUSS
              }
              if("BESCHLUSSSEITE"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Beschlussseite"]<-beschluss1[[length(beschluss1)]]$BESCHLUSSSEITE
              }
              if("BESCHLUSSTENOR"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Beschlusstenor"]<-beschluss1[[length(beschluss1)]]$BESCHLUSSTENOR
              }
              if("BEZUGSDOKUMENT"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Beschlussdokument"]<-beschluss1[[length(beschluss1)]]$BEZUGSDOKUMENT
              }
              if("ABSTIMMUNGSART"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Abstimmungsart"]<-beschluss1[[length(beschluss1)]]$ABSTIMMUNGSART
              }
              if("MEHRHEIT"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Mehrheit"]<-beschluss1[[length(beschluss1)]]$MEHRHEIT
              }
              if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss1[[length(beschluss1)]])){
                metadata_speakers_comp_df16[q+endrow, "Abstimmungsergebnis"]<-beschluss1[[length(beschluss1)]]$ABSTIMMUNG_BEMERKUNG
              }
            }
          }
          metadata_speakers_comp_df16[(1+endrow):(endrow + length(query1b)), "Reading"]<-1 #inputting the relevant information into all the rows initialized by speakers
          metadata_speakers_comp_df16[(1+endrow):(endrow + length(query1b)), "Comments"] <-paste0("Reading 1: Session:", plpr1[p])
          endrow<-nrow(metadata_speakers_comp_df16)
          total_speakers1<-total_speakers1+length(query1b)
        } else { #if there were no speakers in this session, make a note of it
          metadata_speakers_comp_df16[endrow+1, "Comments"] <-paste0("No speakers in Reading 1: Session:", plpr1[p])
          metadata_speakers_comp_df16[endrow+1, "Reading"]<-1
          if(!is.null(xpathSApply(parsed_xml, query1.2a, xmlValue))){
            if("BESCHLUSS"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Beschluss"]<-beschluss1[[length(beschluss1)]]$BESCHLUSS
            }
            if("BESCHLUSSSEITE"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Beschlussseite"]<-beschluss1[[length(beschluss1)]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Beschlusstenor"]<-beschluss1[[length(beschluss1)]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Beschlussdokument"]<-beschluss1[[length(beschluss1)]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Abstimmungsart"]<-beschluss1[[length(beschluss1)]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Mehrheit"]<-beschluss1[[length(beschluss1)]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss1[[length(beschluss1)]])){
              metadata_speakers_comp_df16[1+endrow, "Abstimmungsergebnis"]<-beschluss1[[length(beschluss1)]]$ABSTIMMUNG_BEMERKUNG
            }
          }
          endrow<-nrow(metadata_speakers_comp_df16)
          total_speakers1<-total_speakers1+1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
        }
      } 
  } else { #if there was not a plenary debate, make note of it
    metadata_speakers_comp_df16[endrow+1, "Comments"]<-"No 1st debate"
    metadata_speakers_comp_df16[endrow+1, "Date_of_BT_debate1"]<-"No 1st debate"
    metadata_speakers_comp_df16[endrow+1, "Reading"]<-1
    endrow<-nrow(metadata_speakers_comp_df16)
    total_speakers1<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
  }
  
  #second plenary debate(s)
  total_speakers2<-0
  if (!is.null(plpr2)) {
    for (r in 1:length(plpr2)) { #for each session of first reading
      meeting_ids2<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr2[r]), xmlValue) 
      meeting_ids2<-sort(meeting_ids2)
      #goal now is to check the lengths of children for all of these, and get only the ones who have children
      query2a<-sprintf("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER", meeting_ids2)
      number_children_list2<-lapply(query2a, myfunction2)  
      #now select only those ones with children greater than zero
      positive_children2<-which(number_children_list2>0) #shows which ones are greater than zero --- and write something that happens if none 
      query2.2a<-sprintf("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/BESCHLUSS",meeting_ids) #creating a query to extract the children of all the speaker nodes
      if(!is.null(xpathSApply(parsed_xml, query2.2a, xmlValue))){
        beschluss2<-lapply(query2.2a, myfunction)
      }
      if (length(positive_children2)>0) {
        positive_children_list2<-number_children_list2[positive_children2] #subsetting the list of children to only include those with positive numbers of children
        #now create vectors of their lengths
        full_children2<-lapply(positive_children_list2, myfunction3)
        #now merge each of these into new queries, taking into account which actually have kids
        positive_meeting_ids2<-meeting_ids2[positive_children2]
        myfunction5<-function(x) {
          sprintf("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER[position()=%s]/child::*", positive_meeting_ids2[x], full_children2[[x]])
        }
        query2b<-unlist(lapply(1:length(positive_meeting_ids2),myfunction5))
        speaker_info2<-lapply(query2b, myfunction)
        for (s in 1:length(query2b)) {
          metadata_speakers_comp_df16[s+endrow,"Speaker name"]<-paste(speaker_info2[[s]][1,"PERSON_TITEL"], speaker_info2[[s]][1,"VORNAME"], speaker_info2[[s]][1,"NAMENSZUSATZ"], speaker_info2[[s]][1,"NACHNAME"], speaker_info2[[s]][1,"WAHLKREISZUSATZ"]) #next idea is to subset within the list and then just paste into the dataframe
          metadata_speakers_comp_df16[s+endrow, "Speaker type"]<-speaker_info2[[s]][1, "FUNKTION"]
          if (str_detect(speaker_info2[[s]][1, "FUNKTION"], "Bundeskanzl.|BundestagsprÃ¤s.")) {  #if the speaker is Angela Merkel or Norbert Lammert then the FRAKTION node is missing
            metadata_speakers_comp_df16[s+endrow, "Speaker party"]<-"CDU/CSU"
          } else { #if it's not Merkel or Lammert, continue as normal
            metadata_speakers_comp_df16[s+endrow, "Speaker party"]<-paste(speaker_info2[[s]][1, "FRAKTION"], speaker_info2[[s]][1, "RESSORT"], speaker_info2[[s]][1, "FUNKTIONSZUSATZ"])
          }
          metadata_speakers_comp_df16[s+endrow, "Type of speech"]<-speaker_info2[[s]][1, "AKTIVITAETSART"]
          if(!is.null(xpathSApply(parsed_xml, query2.2a, xmlValue))){
            if("BESCHLUSS"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Beschluss"]<-beschluss2[[length(beschluss2)]]$BESCHLUSS
            }
            if("BESCHLUSSSEITE"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Beschlussseite"]<-beschluss2[[length(beschluss2)]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Beschlusstenor"]<-beschluss2[[length(beschluss2)]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Beschlussdokument"]<-beschluss2[[length(beschluss2)]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Abstimmungsart"]<-beschluss2[[length(beschluss2)]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Mehrheit"]<-beschluss2[[length(beschluss2)]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss2[[length(beschluss2)]])){
              metadata_speakers_comp_df16[s+endrow, "Abstimmungsergebnis"]<-beschluss2[[length(beschluss2)]]$ABSTIMMUNG_BEMERKUNG
            }
          }
        }
        metadata_speakers_comp_df16[(1+endrow):(endrow + length(query2b)), "Reading"]<-2
        metadata_speakers_comp_df16[(1+endrow):(endrow + length(query2b)), "Comments"] <-paste0("Reading 2: Session:", plpr2[r])
        endrow<-nrow(metadata_speakers_comp_df16)
        total_speakers2<-total_speakers2+length(query2b)
      } else {
        metadata_speakers_comp_df16[endrow+1, "Comments"] <-paste0("No speakers in Reading 2: Session:", plpr2[r])
        metadata_speakers_comp_df16[endrow+1, "Reading"]<-2
        if(!is.null(xpathSApply(parsed_xml, query2.2a, xmlValue))){
          if("BESCHLUSS"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschluss"]<-beschluss2[[length(beschluss2)]]$BESCHLUSS
          }
          if("BESCHLUSSSEITE"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschlussseite"]<-beschluss2[[length(beschluss2)]]$BESCHLUSSSEITE
          }
          if("BESCHLUSSTENOR"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschlusstenor"]<-beschluss2[[length(beschluss2)]]$BESCHLUSSTENOR
          }
          if("BEZUGSDOKUMENT"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschlussdokument"]<-beschluss2[[length(beschluss2)]]$BEZUGSDOKUMENT
          }
          if("ABSTIMMUNGSART"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Abstimmungsart"]<-beschluss2[[length(beschluss2)]]$ABSTIMMUNGSART
          }
          if("MEHRHEIT"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Mehrheit"]<-beschluss2[[length(beschluss2)]]$MEHRHEIT
          }
          if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss2[[length(beschluss2)]])){
            metadata_speakers_comp_df16[1+endrow, "Abstimmungsergebnis"]<-beschluss2[[length(beschluss2)]]$ABSTIMMUNG_BEMERKUNG
          }
        }
        endrow<-nrow(metadata_speakers_comp_df16)
        total_speakers2<-total_speakers2+1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
      }
    } 
  } else { #if there was not a plenary debate, make note of it
    metadata_speakers_comp_df16[endrow+1, "Comments"]<-"No 2nd debate"
    metadata_speakers_comp_df16[endrow+1, "Date_of_BT_debate2"]<-"No 2nd debate"
    metadata_speakers_comp_df16[endrow+1, "Reading"]<-2
    endrow<-nrow(metadata_speakers_comp_df16)
    total_speakers2<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
  }
  
  #third debate(s)
  total_speakers3<-0
  if (!is.null(plpr3)) {
    for (t in 1:length(plpr3)) { #for each session of first reading
      meeting_ids3<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr3[t]), xmlValue) 
      meeting_ids3<-sort(meeting_ids3)
      #goal now is to check the lengths of children for all of these, and get only the ones who have children
      query3a<-sprintf("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER", meeting_ids3)
      number_children_list3<-lapply(query3a, myfunction2)  
      #now select only those ones with children greater than zero
      positive_children3<-which(number_children_list3>0) #shows which ones are greater than zero --- and write something that happens if none 
      query3.2a<-sprintf("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/BESCHLUSS",meeting_ids) #creating a query to extract the children of all the speaker nodes
      if(!is.null(xpathSApply(parsed_xml, query3.2a, xmlValue))){
        beschluss3<-lapply(query3.2a, myfunction)
      }
      if (length(positive_children3)>0) {
        positive_children_list3<-number_children_list3[positive_children3] #subsetting the list of children to only include those with positive numbers of children
        #now create vectors of their lengths
        full_children3<-lapply(positive_children_list3, myfunction3)
        #now merge each of these into new queries, taking into account which actually have kids
        positive_meeting_ids3<-meeting_ids3[positive_children3]
        myfunction6<-function(x) {
          sprintf("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[text()='%s']/ancestor::VORGANGSPOSITION/PERSOENLICHER_URHEBER[position()=%s]/child::*", positive_meeting_ids3[x], full_children3[[x]])
        }
        query3b<-unlist(lapply(1:length(positive_meeting_ids3),myfunction6))
        speaker_info3<-lapply(query3b, myfunction)
        for (u in 1:length(query3b)) {
          metadata_speakers_comp_df16[u+endrow,"Speaker name"]<-paste(speaker_info3[[u]][1,"PERSON_TITEL"], speaker_info3[[u]][1,"VORNAME"], speaker_info3[[u]][1,"NAMENSZUSATZ"], speaker_info3[[u]][1,"NACHNAME"], speaker_info3[[u]][1,"WAHLKREISZUSATZ"]) #next idea is to subset within the list and then just paste into the dataframe
          metadata_speakers_comp_df16[u+endrow, "Speaker type"]<-speaker_info3[[u]][1, "FUNKTION"]
          if (str_detect(speaker_info3[[u]][1, "FUNKTION"], "Bundeskanzl.|BundestagsprÃ¤s.")) {  #if the speaker is Angela Merkel or Norbert Lammert then the FRAKTION node is missing
            metadata_speakers_comp_df16[u+endrow, "Speaker party"]<-"CDU/CSU"
          } else { #if it's not Merkel or Lammert, continue as normal
            metadata_speakers_comp_df16[u+endrow, "Speaker party"]<-paste(speaker_info3[[u]][1, "FRAKTION"], speaker_info3[[u]][1, "RESSORT"], speaker_info3[[u]][1, "FUNKTIONSZUSATZ"])
          }
          metadata_speakers_comp_df16[u+endrow, "Type of speech"]<-speaker_info3[[u]][1, "AKTIVITAETSART"]
          if(!is.null(xpathSApply(parsed_xml, query3.2a, xmlValue))){
            if("BESCHLUSS"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Beschluss"]<-beschluss3[[length(beschluss3)]]$BESCHLUSS
            }
            if("BESCHLUSSSEITE"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Beschlussseite"]<-beschluss3[[length(beschluss3)]]$BESCHLUSSSEITE
            }
            if("BESCHLUSSTENOR"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Beschlusstenor"]<-beschluss3[[length(beschluss3)]]$BESCHLUSSTENOR
            }
            if("BEZUGSDOKUMENT"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Beschlussdokument"]<-beschluss3[[length(beschluss3)]]$BEZUGSDOKUMENT
            }
            if("ABSTIMMUNGSART"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Abstimmungsart"]<-beschluss3[[length(beschluss3)]]$ABSTIMMUNGSART
            }
            if("MEHRHEIT"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Mehrheit"]<-beschluss3[[length(beschluss3)]]$MEHRHEIT
            }
            if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss3[[length(beschluss3)]])){
              metadata_speakers_comp_df16[u+endrow, "Abstimmungsergebnis"]<-beschluss3[[length(beschluss3)]]$ABSTIMMUNG_BEMERKUNG
            }
          }
        }
        metadata_speakers_comp_df16[(1+endrow):(endrow + length(query3b)), "Reading"]<-3
        metadata_speakers_comp_df16[(1+endrow):(endrow + length(query3b)), "Comments"] <-paste0("Reading 3: Session:", plpr3[t])
        endrow<-nrow(metadata_speakers_comp_df16)
        total_speakers3<-total_speakers3+length(query3b)
      } else {
        metadata_speakers_comp_df16[endrow+1, "Comments"] <-paste0("No speakers in Reading 3: Session:", plpr3[t])
        metadata_speakers_comp_df16[endrow+1, "Reading"]<-3
        if(!is.null(xpathSApply(parsed_xml, query3.2a, xmlValue))){
          if("BESCHLUSS"%in%names(beschluss1[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschluss"]<-beschluss3[[length(beschluss3)]]$BESCHLUSS
          }
          if("BESCHLUSSSEITE"%in%names(beschluss1[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschlussseite"]<-beschluss3[[length(beschluss3)]]$BESCHLUSSSEITE
          }
          if("BESCHLUSSTENOR"%in%names(beschluss1[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschlusstenor"]<-beschluss3[[length(beschluss3)]]$BESCHLUSSTENOR
          }
          if("BEZUGSDOKUMENT"%in%names(beschluss1[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Beschlussdokument"]<-beschluss3[[length(beschluss3)]]$BEZUGSDOKUMENT
          }
          if("ABSTIMMUNGSART"%in%names(beschluss3[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Abstimmungsart"]<-beschluss3[[length(beschluss3)]]$ABSTIMMUNGSART
          }
          if("MEHRHEIT"%in%names(beschluss3[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Mehrheit"]<-beschluss3[[length(beschluss3)]]$MEHRHEIT
          }
          if("ABSTIMMUNG_BEMERKUNG"%in%names(beschluss3[[length(beschluss3)]])){
            metadata_speakers_comp_df16[1+endrow, "Abstimmungsergebnis"]<-beschluss3[[length(beschluss3)]]$ABSTIMMUNG_BEMERKUNG
          }
        }
        endrow<-nrow(metadata_speakers_comp_df16)
        total_speakers3<-total_speakers3+1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
      }
    } 
  } else { #if there was not a plenary debate, make note of it
    metadata_speakers_comp_df16[endrow+1, "Comments"]<-"No 3rd debate"
    metadata_speakers_comp_df16[endrow+1, "Date_of_BT_debate3"]<-"No 3rd debate"
    metadata_speakers_comp_df16[endrow+1, "Reading"]<-3
    endrow<-nrow(metadata_speakers_comp_df16)
    total_speakers3<-1 #storing the number 1 so I know there is 1 row that needs to be filled with bill information
  }
  
  start<-endrow-(total_speakers1+total_speakers2+total_speakers3)+1 #counting the total number of speakers (or lack thereof) to know how many rows need bill info inputted
  #Inserting bill variables into the rows initialized by speakers (or lack thereof)
  metadata_speakers_comp_df16[(start:endrow),"Legislative_period"]<-wahl
  metadata_speakers_comp_df16[(start:endrow), "Static_URL"]<-url
  metadata_speakers_comp_df16[(start:endrow),"Sachgebiete"]<-sach_collapsed
  metadata_speakers_comp_df16[(start:endrow),"Title"]<-title
  metadata_speakers_comp_df16[(start:endrow),"Inhalt"]<-inhalt
  metadata_speakers_comp_df16[(start:endrow),"Initiator"]<-initiative_collapsed
  metadata_speakers_comp_df16[(start:endrow),"Gesta_number"]<-gesta
  metadata_speakers_comp_df16[(start:endrow),"Aktueller_Stand"]<-akt
  if (length(zust_proc)>0){
    if (str_detect(zust_proc, "Ja")) {metadata_speakers_comp_df16[(start:endrow),"Zustimmungspflicht_proclamation"]<-"yes"}
    if (str_detect(zust_proc, "Nein")) {metadata_speakers_comp_df16[(start:endrow),"Zustimmungspflicht_proclamation"]<-"no"}
  }
  if (length(zust_bill)>0) {
    if (str_detect(zust_bill, "Ja")) {metadata_speakers_comp_df16[(start:endrow),"Zustimmungspflicht_bill"]<-"yes"}
    if (str_detect(zust_bill, "Nein")) {metadata_speakers_comp_df16[(start:endrow),"Zustimmungspflicht_bill"]<-"no"}
  }
  if (length(drs_num)>0) { #if it exists, extract it
    metadata_speakers_comp_df16[(start:endrow),"Drucksachennummer"]<-drs_num  
  }
  metadata_speakers_comp_df16[(start:endrow),"Date_of_initiative"]<-init_date_value
  if (length(ver_dates)>0){
    ver_date_value<-str_extract(ver_dates, "[[:digit:]]([[:digit:]]|\\.){9}") #extracting the first date in this row
    metadata_speakers_comp_df16[(start:endrow),"Proclamation_date"]<-ver_date_value
  }
  if (length(inkraft)>0){
    inkraft<-inkraft[1] #selecting the first entry
    inkraft_value<-str_sub(inkraft, 1,10) #the date is the first 10 characters of the row
    metadata_speakers_comp_df16[(start:endrow),"Date_of_entry_into_force"]<-inkraft_value
  }
  #Plenary protocol and reading
  if (!is.null(plpr1)) {
    metadata_speakers_comp_df16[(start:endrow), "Plenary_protocol1"]<-paste(plpr1, collapse=";")
    debate1<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr1), xmlValue)
    debate1_date<-substr(debate1, 1,10) #extracting the date (1st 10 characters) from that line
    debate1_date<-unique(debate1_date)
    metadata_speakers_comp_df16[(start:endrow), "Date_of_BT_debate1"]<-paste(debate1_date, collapse=";")
  }
  if (!is.null(plpr2)) {
    metadata_speakers_comp_df16[(start:endrow), "Plenary_protocol2"]<-paste(plpr2, collapse=";")
    debate2<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr2), xmlValue)
    debate2_date<-substr(debate2, 1,10) #extracting the date (1st 10 characters) from that line
    debate2_date<-unique(debate2_date)
    metadata_speakers_comp_df16[(start:endrow), "Date_of_BT_debate2"]<-paste(debate2_date, collapse=";")
  }
  if (!is.null(plpr3)) {
    metadata_speakers_comp_df16[(start:endrow), "Plenary_protocol3"]<-paste(plpr3, collapse=";")
    debate3<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr3), xmlValue)
    debate3_date<-substr(debate3, 1,10) #extracting the date (1st 10 characters) from that line
    debate3_date<-unique(debate3_date)
    metadata_speakers_comp_df16[(start:endrow), "Date_of_BT_debate3"]<-paste(debate3_date, collapse=";")
  }
  endrow<-nrow(metadata_speakers_comp_df16)
}
for(i in nrow(metadata_speakers_comp_df16):1){
  if(is.na(metadata_speakers_comp_df16$Beschluss[i])){
    metadata_speakers_comp_df16$Beschluss[i]=metadata_speakers_comp_df16$Beschluss[i+1]
  }
}
metadata_speakers_comp_df16$Beschluss[is.na(metadata_speakers_comp_df16$`Speaker name`)]=NA

save(metadata_speakers_comp_df16, file="16leg_metadata_speakers_comp_df.Rdata")  

#load("16leg_metadata_speakers_comp_df.Rdata")
load("16leg_metadata_speakers_df_encoded.Rdata")

metadata_speakers_complete_df16<-rbind(metadata_speakers_df16, metadata_speakers_comp_df16) #combining the budget extraction with the regular extraction

save(metadata_speakers_complete_df16, file="16leg_metadata_speakers_complete_df_encoded.Rdata")


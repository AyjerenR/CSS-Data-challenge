##THIS SCRIPT MUST BE RUN ON A PC.
##The encoding settings for htmlParse and xmlParseDoc MUST be left as is. The special characters will not be formatted properly when viewed on a PC, but on a Mac they show up perfectly.
##1. Loop to extract bill variables (excluding speaker data). 

rm(list=ls())
#setwd ("C:/Users/Alice/Dropbox/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015") #Olivia
#setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015") #SOP
#setwd("~/Dropbox (Proksch RA Research)/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015")
setwd("~/Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/") #Jens
load("stable_urls_18.Rdata") #loading vector of stable URLs from nth leg

library(XML)
library(RCurl)
library(stringr)
info<-debugGatherer() #not sure this (and below) is necessary..
handle<-getCurlHandle(cookiejar ="",cookiefile = "",followlocation=T, autoreferer=T, debugfunc=info$update, verbose=T, httpheader=list(from="olivia.podmore@gmail.com", 'user-agent'=str_c(R.version$ersion.string,", ", R.version$platform)))

##initializing empty dataframe for metadata
metadata_df18<-data.frame(matrix(nrow = length(stable_urls18), ncol = 22))
names(metadata_df18)<-c("Legislative_period", "Static_URL", "Sachgebiete", "Title", "Inhalt","EU_impulse_relationship", "Initiator", "Aktueller_Stand", "Gesta_number", "Zustimmungspflicht_proclamation", "Zustimmungspflicht_bill", "Drucksachennummer", "Date_of_initiative",  "Proclamation_date", "Date_of_entry_into_force", "Date_of_BT_debate1", "Date_of_BT_debate2", "Date_of_BT_debate3", "Plenary_protocol1", "Plenary_protocol2", "Plenary_protocol3", "Comments")

##1.
##initializing loop
for (i in 1:length(stable_urls18)) {
  ##going to url
  url<-stable_urls18[i] #for 16th leg
  parsed_file <- htmlParse(url) #parsing url
  root<-xmlRoot(parsed_file) #storing the root node of the parsed url
  
  ##capturing xml output
  xml_text<-capture.output(root[[2]][[2]][[6]][[2]][[2]][[7]][[2]][[1]][[6]]) #storing the xml output (value of this node)
  xml_text_cut<-xml_text[-c(1:3, length(xml_text))]#removing first line of the output so that xml is read as xml, not a comment
  xml_text_combined<-paste(xml_text_cut, collapse="\n") #collapsing the xml text so that it reflects xml layout
  parsed_xml<-xmlParseDoc(xml_text_combined, asText=T, baseURL=url) #(re)parsing the xml content
  
  ##extracting relevant variables
  #legislative period
  wahl<-xpathSApply(parsed_xml, path="//WAHLPERIODE", xmlValue)
  metadata_df18[i,"Legislative_period"]<-wahl
  
  #static url
  metadata_df18[i, "Static_URL"]<-url
  
  #Sachgebiete
  sach<-xpathSApply(parsed_xml, path="//SACHGEBIET", xmlValue)
  sach_collapsed<-paste(sach, collapse=";") #multiple subjects so they must be collapsed into one row
  metadata_df18[i,"Sachgebiete"]<-sach_collapsed
  
  #Title
  title<-xpathSApply(parsed_xml, path="//TITEL", xmlValue)
  metadata_df18[i,"Title"]<-title
  
  #Inhalt
  inhalt<-xpathSApply(parsed_xml, path="//ABSTRAKT", xmlValue)
  metadata_df18[i,"Inhalt"]<-inhalt
  
  #Initiator
  initiative<-xpathSApply(parsed_xml, path="//INITIATIVE", xmlValue)
  initiative_collapsed<-paste(initiative, collapse=";") #multiple authors, so they must be collapsed into one row
  metadata_df18[i,"Initiator"]<-initiative_collapsed
  
  #Aktueller Stand
  akt<-xpathSApply(parsed_xml, path="//AKTUELLER_STAND", xmlValue)
  metadata_df18[i,"Aktueller_Stand"]<-akt
  
  #Gesta number
  gesta<-xpathSApply(parsed_xml, path="//GESTA_ORDNUNGSNUMMER", xmlValue)
  metadata_df18[i,"Gesta_number"]<-gesta
  
  ##Zustimmungspflicht-proclamation and Zustimmungspflicht-bill
  #zust proclamation
  zust_proc<-xpathSApply(parsed_xml, path="//ZUSTIMMUNGSBEDUERFTIGKEIT[contains(text(), 'VerkÃ¼ndung')]", xmlValue)
  if (length(zust_proc)>0){
    if (str_detect(zust_proc, "Ja")) {metadata_df18[i,"Zustimmungspflicht_proclamation"]<-"yes"}
    if (str_detect(zust_proc, "Nein")) {metadata_df18[i,"Zustimmungspflicht_proclamation"]<-"no"}
  }
  
  #zust bill
  zust_bill<-xpathSApply(parsed_xml, path="//ZUSTIMMUNGSBEDUERFTIGKEIT[contains(text(), 'Gesetzentwurf')]", xmlValue)
  if (length(zust_bill)>0) {
    if (str_detect(zust_bill, "Ja")) {metadata_df18[i,"Zustimmungspflicht_bill"]<-"yes"}
    if (str_detect(zust_bill, "Nein")) {metadata_df18[i,"Zustimmungspflicht_bill"]<-"no"}
  }
  
  ##Drucksachennummer
  drs_num<-xpathSApply(parsed_xml, path="//DRS_TYP[contains(text(), 'Gesetzentwurf')]/parent::WICHTIGE_DRUCKSACHE/DRS_HERAUSGEBER[contains(text(), 'BT')]/following-sibling::DRS_NUMMER", xmlValue) #find the drucksache of type 'Gesetzentwurf', move to its parent node, move to the child node that corresponds to BT, move to its sibling node and extract the drucksachennummer
  if (length(drs_num)>0) { #if it exists, extract it
    metadata_df18[i,"Drucksachennummer"]<-drs_num  
  }
  
  ##Date of initiative
  init_date<-xpathSApply(parsed_xml, path="//VORGANGSABLAUF/VORGANGSPOSITION[position()=1]/FUNDSTELLE", xmlValue)
  init_date_value<-str_sub(init_date, 1,10) #the date is the first 10 characters of the row
  metadata_df18[i,"Date_of_initiative"]<-init_date_value
  
  ##Proclamation date
  ver_dates<-xpathSApply(parsed_xml, path="//VERKUENDUNG[contains(text(), 'Gesetz')]", xmlValue) #go to the Verkuendung node that contains 'Gesetz' to find the relevant date
  if (length(ver_dates)>0){ #if it exists
    ver_date_value<-str_extract(ver_dates, "[[:digit:]]([[:digit:]]|\\.){9}") #extracting the first date in this row
    metadata_df18[i,"Proclamation_date"]<-ver_date_value
  }
  
  ##Date of entry into force
  inkraft<-xpathSApply(parsed_xml, path="//INKRAFTTRETEN", xmlValue)
  if (length(inkraft)>0){
    inkraft<-inkraft[1] #selecting the first entry
    inkraft_value<-str_sub(inkraft, 1,10) #the date is the first 10 characters of the row
    metadata_df18[i,"Date_of_entry_into_force"]<-inkraft_value
  }
  
  ##Plenary protocols and debate dates
  plpr1<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '1. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  if (!is.null(plpr1)) { #if there is a first plenary session, extract its date. if not, write no debate. 
    plpr1<-unique(plpr1)
    metadata_df18[i, "Plenary_protocol1"]<-paste(plpr1, collapse=";")
    debate1<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '1. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr1), xmlValue)
    debate1_date<-substr(debate1, 1,10) #extracting the date (1st 10 characters) from that line
    debate1_date<-unique(debate1_date)
    metadata_df18[i, "Date_of_BT_debate1"]<-paste(debate1_date, collapse=";")
  } else {metadata_df18[i, "Date_of_BT_debate1"]<-"No 1st debate"}
  plpr2<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '2. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  if (!is.null(plpr2)) {
    plpr2<-unique(plpr2)
    metadata_df18[i, "Plenary_protocol2"]<-paste(plpr2, collapse=";")
    debate2<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '2. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr2), xmlValue)
    debate2_date<-substr(debate2, 1,10) #extracting the date (1st 10 characters) from that line
    debate2_date<-unique(debate2_date)
    metadata_df18[i, "Date_of_BT_debate2"]<-paste(debate2_date, collapse=";")
  } else {metadata_df18[i, "Date_of_BT_debate2"]<-"No 2nd debate"}
  plpr3<-xpathSApply(parsed_xml, path="//PLPR_KLARTEXT[contains(text(), '3. Beratung')]/following-sibling::PLPR_NUMMER", xmlValue)
  if (!is.null(plpr3)) { 
    plpr3<-unique(plpr3)
    metadata_df18[i, "Plenary_protocol3"]<-paste(plpr3, collapse=";")
    debate3<-xpathSApply(parsed_xml, sprintf(("//URHEBER[contains(text(), '3. Beratung')]/following-sibling::FUNDSTELLE[contains(text(), '%s')]"), plpr3), xmlValue)
    debate3_date<-substr(debate3, 1,10) #extracting the date (1st 10 characters) from that line
    debate3_date<-unique(debate3_date)
    metadata_df18[i, "Date_of_BT_debate3"]<-paste(debate3_date, collapse=";")
  } else {metadata_df18[i, "Date_of_BT_debate3"]<-"No 3rd debate"}
  if (is.null(plpr1)&is.null(plpr2)&is.null(plpr3)) {metadata_df18[i, "Comments"]<-"No debates"} #if there are no plenary sessions at all, add a comment to that effect
}



save(metadata_df18, file="18leg_metadata_df_new.Rdata")

metadata_df18$Sachgebiete

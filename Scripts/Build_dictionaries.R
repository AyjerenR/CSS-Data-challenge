rm(list=ls())
library(rstudioapi)
library(tidyverse)
library(quanteda)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

load("../Data/18leg_metadata_df_new.Rdata") #loading vector of stable URLs from nth leg

mdb=read.csv(file="../Data/mdb_18_metadata.csv")

all_Sachgebiete=do.call(rbind,str_split(metadata_df18$Sachgebiete,";"))
library(splitstackshape); 
Sachgebiete_long <- cSplit(data.frame(metadata_df18$Sachgebiete), 'metadata_df18.Sachgebiete', sep=";")%>%gather()%>%filter(!is.na(value))%>%filter(!duplicated(value))
Sachgebiete_long[order(Sachgebiete_long$value),]

all_texts=data.frame(area=NA,text=NA)


########
#Arbeit und Soziales
########
metadata_work=metadata_df18%>%filter(grepl(paste(c("Arbeit und Beschäftigung","Soziale Sicherung"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_work$text=paste(metadata_work$Title,metadata_work$Inhalt)
text_work=paste(metadata_work$text, collapse=" ")
all_texts[1,1]="work"
all_texts[1,2]=text_work

########
#Auswärtiger Ausschuss
########
metadata_foreign=metadata_df18%>%filter(grepl(paste(c("Außenpolitik"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_foreign$text=paste(metadata_foreign$Title,metadata_foreign$Inhalt)
text_foreign=paste(metadata_foreign$text, collapse=" ")
all_texts[2,1]="foreign"
all_texts[2,2]=text_foreign

########
#Bildung
########
metadata_education=metadata_df18%>%filter(grepl(paste(c("Bildung und Erziehung"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_education$text=paste(metadata_education$Title,metadata_education$Inhalt)
text_education=paste(metadata_education$text, collapse=" ")
all_texts[3,1]="education"
all_texts[3,2]=text_education

########
#Digitale Agenda
########
metadata_digital=metadata_df18%>%filter(grepl(paste(c("Medien"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_digital$text=paste(metadata_digital$Title,metadata_digital$Inhalt)
text_digital=paste(metadata_digital$text, collapse=" ")
all_texts[4,1]="digital"
all_texts[4,2]=text_digital

########
#Ernährung und Landwirtschaft
########
metadata_agriculture=metadata_df18%>%filter(grepl(paste(c("Landwirtschaft"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_agriculture$text=paste(metadata_agriculture$Title,metadata_agriculture$Inhalt)
text_agriculture=paste(metadata_agriculture$text, collapse=" ")
all_texts[5,1]="agriculture"
all_texts[5,2]=text_agriculture

########
#Europäische Union
########
metadata_eu=metadata_df18%>%filter(grepl(paste(c("Europäische Union"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_eu$text=paste(metadata_eu$Title,metadata_eu$Inhalt)
text_eu=paste(metadata_eu$text, collapse=" ")
all_texts[6,1]="eu"
all_texts[6,2]=text_eu

########
#Familie, Senioren, Frauen etc
########
metadata_family=metadata_df18%>%filter(grepl(paste(c("Gesellschaftspolitik"), collapse="|"),metadata_df18$Sachgebiete))%>%
  select(Title,Inhalt)
metadata_family$text=paste(metadata_family$Title,metadata_family$Inhalt)
text_family=paste(metadata_family$text, collapse=" ")
all_texts[7,1]="family"
all_texts[7,2]=text_family




metadata_corpus=corpus(all_texts)

metadata_dfm=dfm(metadata_corpus,remove = stopwords("german"), remove_punct = TRUE,remove_numbers = TRUE)
topfeatures(metadata_dfm,50)
metadata_matrix=as.matrix(metadata_dfm)
metadata_colsums=colSums(metadata_dfm)


metadata_matrix_colprop=metadata_matrix
for(i in 1:ncol(metadata_matrix_colprop)){
  metadata_matrix_colprop[,i]=metadata_matrix[,i]/metadata_colsums[i]
}

exclusive_family_words=colnames(metadata_matrix_colprop)[as.numeric(which(metadata_matrix_colprop[7,]==1))]
summary(metadata_matrix_colprop[7,])
as.numeric(which(metadata_matrix_colprop[7,]==1))

colnames(metadata_matrix_colprop)

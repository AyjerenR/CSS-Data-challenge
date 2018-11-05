rm(list=ls())
library(rstudioapi)
library(tidyverse)
library(quanteda)
library(Zelig)


current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
load(file="../Data/question_data.RData")

data_questions_joined=left_join(data_questions,bt17_profiles,by=c("name"="meta.username"))

####################
#extract Ausschussbezeichnungen
committees=c()
for(i in 1:nrow(data_questions_joined)){
  committees=c(committees,data_questions_joined$committees[i][[1]]$name)
}
committees=unique(committees)
committees

#Ausschuss fuer Inneres und Heimat
data_questions_joined$innenausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Inneres",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$innenausschuss[i]="Yes"
  }
}
bt17_profiles$innenausschuss="No"
for(i in 1:nrow(bt17_profiles)){
  if(sum(grepl("Inneres",bt17_profiles$committees[i][[1]]$name))>0){
    bt17_profiles$innenausschuss[i]="Yes"
  }
}

#Auswärtiger Ausschuss
data_questions_joined$aussenausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Auswärtiger",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$aussenausschuss[i]="Yes"
  }
}
bt17_profiles$aussenausschuss="No"
for(i in 1:nrow(bt17_profiles)){
  if(sum(grepl("Auswärtiger",bt17_profiles$committees[i][[1]]$name))>0){
    bt17_profiles$aussenausschuss[i]="Yes"
  }
}

#Ausschuss fuer Verkehr und digitale Infrastruktur
data_questions_joined$verkehrsausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Verkehr",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$verkehrsausschuss[i]="Yes"
  }
}

#Ausschuss für Ernährung und Landwirtschaft
data_questions_joined$landwirtschaftssausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Landwirtschaft",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$landwirtschaftssausschuss[i]="Yes"
  }
}

#Ausschuss für Umwelt, Naturschutz und nukleare Sicherheit
data_questions_joined$umweltsausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Umwelt",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$umweltsausschuss[i]="Yes"
  }
}

#Haushaltsausschuss
data_questions_joined$haushaltsausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Umwelt",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$umweltsausschuss[i]="Yes"
  }
}

#Familienausschuss
data_questions_joined$familienausschuss="No"
for(i in 1:nrow(data_questions_joined)){
  if(sum(grepl("Frauen",data_questions_joined$committees[i][[1]]$name))>0){
    data_questions_joined$familienausschuss[i]="Yes"
  }
}
bt17_profiles$familienausschuss="No"
for(i in 1:nrow(bt17_profiles)){
  if(sum(grepl("Frauen",bt17_profiles$committees[i][[1]]$name))>0){
    bt17_profiles$familienausschuss[i]="Yes"
  }
}

########################################
# Number of Questions
bt17_profiles$number_questions=0
for(i in 1:nrow(bt17_profiles)){
  bt17_profiles$number_questions[i]=length(data_questions_joined$url[data_questions_joined$name==bt17_profiles$meta.username[i]])
}

table(data_questions_joined$category)

# Fragen bzgl Inneres
data_questions_joined$innenthema=0
data_questions_joined$innenthema[data_questions_joined$category%in%c("Inneres und Justiz","Integration","Sicherheit","Verwaltung und Föderalismus")]=1
bt17_profiles$innenfragen=0
bt17_profiles$innenfragen_share=0
for(i in 1:nrow(bt17_profiles)){
  bt17_profiles$innenfragen[i]=sum(data_questions_joined$innenthema[data_questions_joined$name==bt17_profiles$meta.username[i]])
  bt17_profiles$innenfragen_share[i]=(bt17_profiles$innenfragen[i]/bt17_profiles$number_questions[i])*100
}
bt17_profiles$innenfragen_binary=ifelse(bt17_profiles$innenfragen>0,1,0)

# Fragen bzgl Außenpolitik
data_questions_joined$aussenthema=0
data_questions_joined$aussenthema[data_questions_joined$category%in%c("Internationales")]=1
bt17_profiles$aussenfragen=0
bt17_profiles$aussenfragen_share=0
for(i in 1:nrow(bt17_profiles)){
  bt17_profiles$aussenfragen[i]=sum(data_questions_joined$aussenthema[data_questions_joined$name==bt17_profiles$meta.username[i]])
  bt17_profiles$aussenfragen_share[i]=(bt17_profiles$aussenfragen[i]/bt17_profiles$number_questions[i])*100
}
bt17_profiles$aussenfragen_binary=ifelse(bt17_profiles$aussenfragen>0,1,0)

# Fragen bzgl Familie
data_questions_joined$familienthema=0
data_questions_joined$familienthema[data_questions_joined$category%in%c("Familie","Frauen","Kinder und Jugend","Senioren")]=1
bt17_profiles$familienfragen=0
bt17_profiles$familienfragen_share=0
for(i in 1:nrow(bt17_profiles)){
  bt17_profiles$familienfragen[i]=sum(data_questions_joined$familienthema[data_questions_joined$name==bt17_profiles$meta.username[i]])
  bt17_profiles$familienfragen_share[i]=(bt17_profiles$familienfragen[i]/bt17_profiles$number_questions[i])*100
}
bt17_profiles$familienfragen_binary=ifelse(bt17_profiles$familienfragen>0,1,0)

bt17_profiles_for_models=bt17_profiles%>%filter(number_questions>0)

#Models Innen
mod.internal=glm(innenthema ~ personal.gender+innenausschuss,family=binomial(link='logit'),data=data_questions_joined)
summary(mod.internal)

mod.internal.inter=glm(innenthema ~ personal.gender*innenausschuss,family=binomial(link='logit'),data=data_questions_joined)
summary(mod.internal.inter)

mod.internal.indiv=lm(innenfragen_share ~ personal.gender+innenausschuss,data=bt17_profiles_for_models)
summary(mod.internal.indiv)

mod.internal.inter.indiv=glm(innenfragen_share ~ personal.gender*innenausschuss,data=bt17_profiles_for_models)
summary(mod.internal.inter.indiv)

mod.internal.indiv.binary=glm(innenfragen_binary ~ personal.gender+innenausschuss,family=binomial(link='logit'),data=bt17_profiles_for_models)
summary(mod.internal.indiv.binary)

mod.internal.inter.indiv.binary=glm(innenfragen_binary ~ personal.gender*innenausschuss,family=binomial(link='logit'),data=bt17_profiles_for_models)
summary(mod.internal.inter.indiv.binary)

#Models Aussen

mod.external.inter=glm(aussenthema ~ personal.gender*aussenausschuss,family=binomial(link='logit'),data=data_questions_joined)
summary(mod.external.inter)

mod.external.inter.indiv.binary=glm(aussenfragen_binary ~ personal.gender*aussenausschuss,family=binomial(link='logit'),data=bt17_profiles_for_models)
summary(mod.external.inter.indiv.binary)


#Models Familien
mod.fam=glm(familienthema ~ personal.gender+familienausschuss,family=binomial(link='logit'),data=data_questions_joined)
summary(mod.fam)

mod.fam.inter=glm(familienthema ~ personal.gender*familienausschuss,family=binomial(link='logit'),data=data_questions_joined)
summary(mod.fam.inter)

mod.fam.indiv=lm(familienfragen_share ~ personal.gender+familienausschuss,data=bt17_profiles_for_models)
summary(mod.fam.indiv)

mod.fam.inter.indiv=glm(familienfragen_share ~ personal.gender*familienausschuss,data=bt17_profiles_for_models)
summary(mod.fam.inter.indiv)

mod.fam.indiv.binary=glm(familienfragen_binary ~ personal.gender+familienausschuss,family=binomial(link='logit'),data=bt17_profiles_for_models)
summary(mod.fam.indiv.binary)

mod.fam.inter.indiv.binary=glm(familienfragen_binary ~ personal.gender*familienausschuss,family=binomial(link='logit'),data=bt17_profiles_for_models)
summary(mod.fam.inter.indiv.binary)


data_frame_results=data_frame(Gender=rep(NA,12),Ausschuss=rep(NA,12),est=rep(NA,12),se=rep(NA,12),Topic=rep(NA,12))

preddata <- with(bt17_profiles_for_models, data.frame(personal.gender = c("male","male","female","female"), innenausschuss = c("No","Yes","No","Yes")))
preds <- predict(mod.internal.inter.indiv.binary, newdata = preddata, type = "response", se.fit = TRUE)
data_frame_results$Gender[1:4]=c("male","male","female","female")
data_frame_results$Ausschuss[1:4]=c("Not Innenausschuss","Innenausschuss","Not Innenausschuss","Innenausschuss")
data_frame_results$est[1:4]=preds$fit
data_frame_results$se[1:4]=preds$se.fit
data_frame_results$Topic[1:4]="Innenpolitik"
  
preddata <- with(bt17_profiles_for_models, data.frame(personal.gender = c("male","male","female","female"), familienausschuss = c("No","Yes","No","Yes")))
preds <- predict(mod.fam.inter.indiv.binary, newdata = preddata, type = "response", se.fit = TRUE)
data_frame_results$Gender[5:8]=c("male","male","female","female")
data_frame_results$Ausschuss[5:8]=c("Not Familienausschuss","Familienausschuss","Not Familienausschuss","Familienausschuss")
data_frame_results$est[5:8]=preds$fit
data_frame_results$se[5:8]=preds$se.fit
data_frame_results$Topic[5:8]="Familienpolitik"

preddata <- with(bt17_profiles_for_models, data.frame(personal.gender = c("male","male","female","female"), aussenausschuss = c("No","Yes","No","Yes")))
preds <- predict(mod.external.inter.indiv.binary, newdata = preddata, type = "response", se.fit = TRUE)
data_frame_results$Gender[9:12]=c("male","male","female","female")
data_frame_results$Ausschuss[9:12]=c("Not Aussenausschuss","Aussenausschuss","Not Aussenausschuss","Aussenausschuss")
data_frame_results$est[9:12]=preds$fit
data_frame_results$se[9:12]=preds$se.fit
data_frame_results$Topic[9:12]="Aussenpolitik"



data_frame_results$lower=data_frame_results$est-1.96*data_frame_results$se
data_frame_results$upper=data_frame_results$est+1.96*data_frame_results$se
data_frame_results$labels=paste(data_frame_results$Gender,data_frame_results$Ausschuss)

p.senti.labuk <- ggplot(data=data_frame_results) +
  theme_bw() +ggtitle("")+
  geom_pointrange(
    aes(x=reorder(labels,est), y=est, ymin=lower, ymax=upper),
    lwd = 1/2, position = position_dodge(width = 1/2),
    shape = 21, fill = "black") +
  coord_flip() +
  xlab("") +
  ylab("Coefficients and 95% confidence intervals") +
  theme(text = element_text(size=16)) +
  theme(axis.title.x = element_text(size=12, vjust=-0.5))+
facet_grid(~Topic)
p.senti.labuk

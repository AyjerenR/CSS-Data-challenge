##This script can be run on either a Mac or a PC. The koerperschaftlicherUrheber inputs can be viewed by looking at the source of the search webpage.

setwd ("C:/Users/Alice/Dropbox/Proksch RA Research Folder/German Parliamentary Debates/Metadata 2005-2015") #Olivia
setwd("~/Dropbox/Research/proksch ra research folder/German Parliamentary Debates/Metadata 2005-2015") #SOP
setwd("~/../Dropbox/SOP/Germany Parliamentary Debates 1949-2015/GermanyBundestagforJens/R scripts and data/Metadata 2005-2015/") #Jens

library(XML)
library(RCurl)
library(httr)
library(stringr)
library(plyr)

#functions as dictated by the textbook (p.236)
info<-debugGatherer()
handle<-getCurlHandle(cookiejar ="",cookiefile = "",followlocation=T, autoreferer=T, debugfunc=info$update, verbose=T, httpheader=list(from="olivia.podmore@gmail.com", 'user-agent'=str_c(R.version$ersion.string,", ", R.version$platform)))
xmlAttrsToDF<-function(parsedHTML, xpath) {
  x<-xpathApply(parsedHTML, xpath, xmlAttrs)
  x<-lapply(x, function(x) as.data.frame(t(x)))
  do.call(rbind.fill,x)
}

# go to Bundestag search site and get cookie
url1 <- "http://dipbt.bundestag.de/dip21.web/bt" #static base url
url2<-"http://dipbt.bundestag.de/dip21.web/searchProcedures/advanced.do" #url to be pasted to
baseurl<-htmlParse(getURL (url=url1, curl=handle)) #going to webpage to store cookie

#viewing form and inputs
form<-htmlParse(getURL (url=url2, curl=handle)) #parsing the webpage
xmlAttrsToDF(form, "//form") #works, seeing that there is a form
xmlAttrsToDF(form, "//form//input") #showing me the inputs to the form

##initializing some objects
offset<-0 #setting offset (initially) to zero so that it shows 1-100 of results
start<-1 #setting it so the first 100 results get stored in the 1-100th places
search.results<-"" #initializing an empty vector to store the results

##running the loop to extract 100 results, then offset by +100 and extract the next 100 and so on...until we reach the end of results
while(TRUE){
  #res<-postForm(uri=url2, curl=handle, style="POST", wahlperiode="5", vorgangstyp="6",  koerperschaftlicherUrheber=c("31", "45", "56", "34", "12", "117", "46", "42", "115", "93", "43", "38", "102", "103", "8", "1", "5"),includeVorgangstyp="UND",includeKu="UND", offset=offset, method="Suchen") #submitting form for first page of search results (17th leg)
  res<-postForm(uri=url2, curl=handle, style="POST", wahlperiode="6", vorgangstyp="6",  koerperschaftlicherUrheber=c("45", "152", "34", "12", "117", "46", "42", "115", "153", "155","38","156", "154", "8", "1", "2"),includeVorgangstyp="UND",includeKu="UND", offset=offset, method="Suchen") #submitting form for first page of search results (18th leg)
  #res<-postForm(uri=url2, curl=handle, style="POST", wahlperiode="4", vorgangstyp="6",  koerperschaftlicherUrheber=c("31", "45", "56", "34", "12", "117", "46", "92", "42", "115", "93", "43", "99", "74", "38", "102", "103", "8", "1", "2"),includeVorgangstyp="UND",includeKu="UND", offset=offset, method="Suchen") #submitting form for first page of search results (16th leg)
  all.links<-getHTMLLinks(res) #grabbing all html Links
  relevant.links<-grep("anzahl", all.links, value=T) #grabbing only relevant links
  length<-length(relevant.links) #counting the number of relevant links - i.e. the links to the bills
  search.results[start:(start+length-1)]<-relevant.links #storing the relevant links in the nth row in 'search.results' 
  offset<-offset+100 #increasing the offset value by 100 to view the next page
  start<-start+100 #increasing the 'n' value where the results will be stored by 100 so as not to overwrite previous results
  if(length < 100) break() #if fewer than 100 relevant links were returned we have reached the last page of results
print(length)
}

##formatting urls correctly
base_url<-"http://dipbt.bundestag.de"
for (i in 1:length(search.results)) {search.results[i]<-str_c(base_url, search.results[i])}

##redirecting to (and saving) stable links
stable_urls18<-c()
for (j in 1:length(search.results)) {
  search_url<-search.results[j]
  search_url_parsed<-htmlParse(getURL (url=search_url, curl=handle)) #parsing the webpage
  html_links<-getHTMLLinks(search_url_parsed)
  stable_url18<-grep("extrakt/ba", html_links, value=T)
  stable_urls18<-c(stable_urls18, stable_url18)
  search_url<-NULL
  search_url_parsed<-NULL
  html_links<-NULL
  stable_url18<-NULL
  print(j/length(search.results))
}

save(stable_urls18, file="stable_urls_18.Rdata")


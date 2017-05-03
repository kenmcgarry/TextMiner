# scrape_webpage.R
# Some functions to harvest UX data from from gaming forums.
# Use either the TOOLS -> WEBDEVELOPER -> INSPECTOR or the SelectorGadget in GoogleChrome to..
# figure out the .css details of where stuff is stored on a webpage.
# Selectorgadget is a javascript bookmarklet that allows you to interactively figure out what css selector 
# you need to extract desired components from a page.
# 1. download posts from Forum (must identify css selectors first!)
# 2. parse posts
# 3. create term document indexes
# 4. build a wordcloud
# 5. start sentiment analysis - barplots etc
# 6. extract semantics from phrases/sentences
# 7. use igraph to build network of terms.
# Ken McGarry 2/3/2017

library(rvest)
library(ggplot2)
library(tm)
library(wordcloud)
library(SnowballC)
library(tm.lexicon.GeneralInquirer)
library(tm.plugin.sentiment)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)

page1 <- read_html("http://forum.techland.pl/topic/18-ideas-and-feature-requests-for-dying-light-developers/")
# There are 80 pages of posts in this topic
pageN <- read_html("http://forum.techland.pl/topic/18-ideas-and-feature-requests-for-dying-light-developers/?page=2")
techland <- "http://forum.techland.pl/topic/18-ideas-and-feature-requests-for-dying-light-developers/"

# Get the base page and then grab the remaining HTML pages
allposts <- parsePage(techland,0) # 0=base page, anyother number is added as an offset

for(i in 2:80){
  url <- paste(techland,"?page=",i)
  url <- gsub(" ", "",url) # get rid of any spaces found in url
  pageN <- read_html(url)
  # specify the css selector in html_nodes() and extract the text with html_text(). 
  posts <- pageN %>% html_nodes(".ipsContained") %>% html_text() 
  posts <- gsub("[\t\n]", "", posts) # get rid of newlines and tabs
  posts <- gsub("[[:punct:]]"," ",posts)
  length(posts)
  allposts <- c(allposts,posts)
}

myCorpus <- Corpus(VectorSource(allposts))
myTDM <- DocumentTermMatrix(myCorpus)
myDTM <- TermDocumentMatrix(myCorpus)
inspect(myCorpus)

#myCorpus = tm_map(myCorpus, tolower)
#myCorpus = tm_map(myCorpus, removePunctuation)
#myCorpus = tm_map(myCorpus, removeNumbers)
#myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
#myCorpus = tm_map(myCorpus, stemDocument) # Snowball does not work with ver3 of R
#inspect(myCorpus)
#myTDM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
#myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))

myCorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus,content_transformer(stripWhitespace))
myCorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
#myCorpus <- tm_map(myCorpus, stemDocument)

wordcloud(myCorpus,scale=c(5,0.5),min.freq=5,max.words=250,random.order=FALSE,rot.per=0.35,use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))

# myCorpus[[2]]$content  # How to access content of parsed posts
# allposts[2] # Access original post

some_txt<- c("I am very happy at stack overflow, excited, and optimistic.",
             "I hate statistics, the nasty people, the stupid people, and everybody else I ever met",
             "I love statistics, I think text mining is wonderful and get excited when data mining",
             "I am very scared from OP question, annoyed, and irritated.", 
             "I am completely neutral about blandness.",
             "Ken is wonderful, Ken is great but Ken gets angry sometimes and very impatient")

#corpus <- Corpus(VectorSource(some_txt)) 
pos <- sum(sapply(myCorpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv")))
neg <- sum(sapply(myCorpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ")))

pos.score <- tm_term_score(TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE)), 
                           terms_in_General_Inquirer_categories("Positiv")) # this lists each document with number below

neg.score <- tm_term_score(TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE)), 
                           terms_in_General_Inquirer_categories("Negativ")) 

total.df <- data.frame(positive = pos.score, negative = neg.score)
total.df <- transform(total.df, net = positive - negative)
total.df

# FUNCTIONS DEFINED HERE

# parsePage() will access the webpage given the URL and if page is the first one or not.
# This is flagged by 'base', if its 0 then first page, else base will hold a number indicating page number
# Its imporant to realise that only 25 posts are shown on a topic, so topics with many posts
# will have several pages, parsePage() will return the posts found on that page.
parsePage <- function (webpage,base){
  if(base >= 0){
    url <- paste(webpage,"?page=",i)}
  url <- gsub(" ", "",url) # get rid of any spaces found in url
  pageN <- read_html(url)
  # specify the css selector in html_nodes() and extract the text with html_text(). 
  myposts <- pageN %>% html_nodes(".ipsContained") %>% html_text() 
  myposts <- gsub("[\t\n]", "", myposts) 
  myposts <- gsub("[[:punct:]]"," ",myposts)
return(myposts)}








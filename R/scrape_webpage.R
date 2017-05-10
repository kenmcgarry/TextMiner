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
library(viridis)
library(widyr)
library(xtable)
library(sentimentr)

setwd("C:/R-files/UX") # point to where my code lives
load("UX-9thMay-AM-2017.RData") # last known build
source("textmine_func.R")  # load in the functions required for downloading posts

request_root <- "http://forum.techland.pl/topic/18-ideas-and-feature-requests-for-dying-light-developers/"
faq_root <- "http://forum.techland.pl/topic/2722-dying-light-frequently-asked-questions/"
follow_root <- "http://forum.techland.pl/topic/11106-dying-light-the-following-enhanced-edition/"
tools_root <- "http://forum.techland.pl/forum/106-developer-tools/"

# Get the base page and then grab the remaining HTML pages
reqposts <- parsePage(request_root,0) # 0=base page, any other number is added as an offset
faqposts <- parsePage(faq_root,0) # 0=base page, any other number is added as an offset
followposts <- parsePage(follow_root,0) # 0=base page, any other number is added as an offset
toolsposts <- parsePage(tools_root,0) # 0=base page, any other number is added as an offset

# The number of pages in a topic can only be found manually at the moment - sorry.
reqposts <- parseAll(request_root,81,reqposts)
faqposts <- parseAll(faq_root,4,faqposts)
followposts <- parseAll(follow_root,4,followposts)
toolsposts <- parseAll(tools_root,19,toolsposts)


# cant save posts just yet as they contain punctuation that causes file errors
#write.csv(reqposts,faqposts,followposts, toolsposts, file = "userposts.csv") # save the posts for later.

myCorpus <- Corpus(VectorSource(allposts))
myTDM <- DocumentTermMatrix(myCorpus)
myDTM <- TermDocumentMatrix(myCorpus)
inspect(myCorpus)

myCorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus,content_transformer(stripWhitespace))
myCorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
#myCorpus <- tm_map(myCorpus, stemDocument)

wordcloud(myCorpus,scale=c(5,0.5),min.freq=5,max.words=250,random.order=FALSE,rot.per=0.35,use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))

# myCorpus[[20]]$content  # How to access content of parsed posts
# allposts[20] # Access original post

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

total.df <- cbind(total.df,index=seq(1,nrow(total.df),1))

# Fudge for moment - fudge other five topics - create as Factors
# 1. FAQ        2. Enhanced Edition    3. The Following
# 4. Feature requests    5. General discussions

topic <- c(rep("FAQ",520),rep("Enhanced Edition",520),rep("Feature requests",520),rep("General discussions",520))

total.df <- cbind(total.df, topic)
total.df <- total.df[-5]   # I have no idea why "topic[1,]" is created - but this kills it.

head(total.df)
tail(total.df)

# xtable for LaTex file
tli.table <- xtable(total.df[1:10,])
digits(tli.table)[c(3,4,5)] <- print(tli.table,floating=TRUE)


ggplot(total.df, aes(index, net, fill=topic)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, ncol = 2, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(title = NULL,y = "Topic sentiment",x=NULL,size=12,face="bold") +
  scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text = element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(strip.text = element_text(face="bold", size = 12, colour = "black")) +
  theme(axis.title.y =element_text(size=12,face="bold")) +
  theme(axis.title.x =element_blank()) +
  theme(axis.ticks.x =element_blank()) +
  theme(axis.text.x  =element_blank())

#---------------------- analysis ----------------------
# Need to convert array of posts into "tidy" data format
# faqposts; followposts; toolsposts; reqposts

original_posts <- make_topics() %>%
  group_by(topic) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

original_posts

tidy_posts <- original_posts %>%
  unnest_tokens(word, text)

tidy_posts

data("stop_words")
tidy_posts <- tidy_posts %>%
  anti_join(stop_words)

tidy_posts %>%
  count(word, sort = TRUE)

bingposts <- sentiments %>%   #find a sentiment score for each word using the Bing lexicon, 
  filter(lexicon == "bing") %>%
  select(-score)

bingposts

post_sentiment <- tidy_posts %>%
  inner_join(bingposts) %>% 
  count(topic, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

post_sentiment

post_words <- tidy_posts %>%
  filter(topic == "Frequently asked questions")
post_words

word_cooccurences <- post_words %>% 
  pairwise_count(word,linenumber, sort = TRUE)
word_cooccurences

best_cooccurences <- filter(word_cooccurences, n > 3)
best_cooccurences

posts_bing <- sentiments %>%   #find a sentiment score for each word using the Bing lexicon, 
  filter(lexicon == "bing") %>%
  select(-score)

postsnegative <- sentiments %>%
  filter(lexicon == "bing", sentiment == "negative")

# Pretty chart of bi-words coocurrences
word_cooccurences %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  ggtitle(expression(paste("Word Network in "))) +
  theme_void()  


post_sentences <- make_topics() %>%
  mutate(text = iconv(text, to = 'latin1')) %>%
  group_by(topic) %>%
  unnest_tokens(sentence, text, token = 'sentences') %>%
  ungroup() %>%
  #filter(row_number() == 39) %>%
  select(sentence)

# keywords <- c("inconvienince", "problem", "confusion", "complicated", "issue", "obstacle", "glitch") 
nrc_angrey <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

tidy_posts %>%
  #filter(topic == "Emma") %>%
  semi_join(nrcjoy) %>%
  count(word, sort = TRUE)

# Now plot sentiment for our genuine 4 posts categories
bing <- get_sentiments("bing")
newpostsentiment <- tidy_posts %>%
  inner_join(bing) %>%
  count(topic, index = linenumber %/% 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(newpostsentiment, aes(index, sentiment, fill = topic)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, ncol = 2, scales = "free_x")

# get word counts that contribute to each sentiment.
bing_word_counts <- tidy_posts %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# Get words and show this visually in a plot
bing_word_counts %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Contribution to sentiment")


# Another wordcloud
tidy_posts %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 250)


# Look at whole sentences
PandP_sentences <- data_frame(text = tidy_posts) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# plot sentences to webpage, +VE are green and -VE are red
x<-unlist(post_sentences)
websentiment <- sentiment_by(x[61:75])
highlight(websentiment)



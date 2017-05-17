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
library(quanteda)
library(hunspell)
library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(viridis)
library(widyr)
library(xtable)
library(sentimentr)
library(reshape2)
library(igraph)
library(ggraph)

setwd("C:/R-files/UX") # point to where my code and data lives
load("UX-16thMay-PM-2017.RData") # last known build
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

# cant save posts as CSV just yet as they contain punctuation that causes file errors
#write.csv(reqposts, file = "userposts.csv") # save the posts for later.

# Using TM package to create a corpus - just for wordcloud and some sentiment analysis.
myCorpus <- Corpus(VectorSource(original_posts))
myTDM <- DocumentTermMatrix(myCorpus)
myDTM <- TermDocumentMatrix(myCorpus)
inspect(myCorpus)

#myCorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus,content_transformer(stripWhitespace))
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus <- tm_map(myCorpus, stemDocument)

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
# Now using Julia Silges packages and data formats, need to convert array of 
# posts into "tidy" data format using make_topics()
# faqposts; followposts; toolsposts; reqposts

original_posts <- make_topics() %>%
  group_by(topic) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

original_posts$text <- gsub("[^[:alnum:]///' ]", "", original_posts$text)
original_posts$text  <- stringi::stri_trans_general(original_posts$text, "latin-ascii")
original_posts<- original_posts[!grepl("DDDNN", original_posts$text),] # gets rid of weird characters posts

#words <- c("love", "loving", "lovingly", "loved", "lover", "lovely", "love")
#x <- hunspell_stem(original_posts$text)
#hunspell_analyze(words)

write.csv(original_posts, file = "userposts.csv") # save the posts for later.

tidy_posts <- original_posts %>%
  unnest_tokens(word, text)

tidy_posts

data("stop_words")
tidy_posts <- tidy_posts %>%
  anti_join(stop_words)

# Remove these words since they come up as negative but they generally neutral since they 
# refer to the entities in the game name and and main objective.
neutral_words <- (c("zombie","zombies","dying","game","light","dd","ddd","ddddd","DDNDD","DD","NDDNDDDNNDDD","D","DDDDDDD","DDDDD","DNN","DDNDDD","DDDDNDDDDD", "DDN", "D"))
custom_stop_words <- bind_rows(data_frame(word = neutral_words,lexicon = c("custom")),stop_words)
custom_stop_words
tidy_posts <- tidy_posts %>%
  anti_join(custom_stop_words)


tidy_posts %>%
  count(word, sort = TRUE)

bingposts <- sentiments %>%   #find a sentiment score for each word using the Bing lexicon, 
  filter(lexicon == "bing") %>%
  select(-score)

bingposts

post_sentiment <- tidy_posts %>%
  inner_join(bingposts) %>% 
  count(topic, index = linenumber %/% 10, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

post_sentiment

post_words <- tidy_posts %>%
  filter(topic == "Feature requests")
post_words

word_cooccurences <- post_words %>% 
  pairwise_count(word,linenumber, sort = TRUE)
word_cooccurences

best_cooccurences <- filter(word_cooccurences, n > 80)
best_cooccurences

posts_bing <- sentiments %>%   #find a sentiment score for each word using the Bing lexicon, 
  filter(lexicon == "bing") %>%
  select(-score)

postsnegative <- sentiments %>%
  filter(lexicon == "bing", sentiment == "negative")
postspositive <- sentiments %>%
  filter(lexicon == "bing", sentiment == "positive")

# Pretty chart of bigram or bi-words coocurrences
best_cooccurences %>%
  filter(n >= 80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "cornflowerblue", size = 10) + #cornflowerblue darkslategray4
  geom_node_text(aes(label = name), vjust = 2) +
  #ggtitle(expression(paste("Word Network in "))) +
  theme_void()  

post_sentences <- make_topics() %>%
  mutate(text = iconv(text, to = 'latin1')) %>%
  group_by(topic) %>%
  unnest_tokens(sentence, text, token = 'sentences') %>%
  ungroup() %>%
  #filter(row_number() == 39) %>%
  select(sentence)

sentiment_pos <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_posts %>%
  semi_join(sentiment_pos) %>%
  count(word, sort = TRUE)

# Now plot sentiment for our 4 categories of posts
bing <- get_sentiments("bing")
newpostsentiment <- tidy_posts %>%
  inner_join(bing) %>%
  count(topic, index = linenumber %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(newpostsentiment, aes(index, sentiment, fill = topic)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = NULL,x = "Topic sentiment",y="Sentiment",size=12,face="bold") +
  facet_wrap(~topic, ncol = 2, scales = "free_x")

# get word counts that contribute to each sentiment.
bing_word_counts <- tidy_posts %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# --------------------plots used in paper ------------------------------------------
# xtable for LaTex file
bwc.table <- xtable(bing_word_counts[1:15,])

op.table <- xtable(original_posts[2020:2030,])
#digits(tli.table)[c(3,4,5)] <- print(tli.table,floating=TRUE)

# Get Word Contribution and show this visually in a plot
bing_word_counts %>%
  filter(n > 80) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Contribution to sentiment")

# Another wordcloud - divided into pos and neg sentiment
tidy_posts %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 250)

# plot sentences to webpage, +VE are green and -VE are red
# i maynot use this diagram
x<-unlist(post_sentences)
websentiment <- sentiment_by(x[100:110])
highlight(websentiment)

# Analysis of usability keywords, using complex network statistics (igraph)

keywords <- c("inconvienince", "problem", "confusion", "complicated", "issue", "obstacle", "glitch","bug",
              "annoying","stupid","unfair","difficult","hard","bad","issues","hate","wrong","cheat") 

results <- keyword_search(keywords,word_cooccurences)

rnames <- rownames_to_column(results[[1]],var = "rowname")

graphstats <- arrange(rnames,desc(hubness,drop=FALSE))
graph.table <- xtable(graphstats[1:20,])


# bigrams
tidy_bigrams <- tidy_posts %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

tidy_bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- tidy_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
AFINN <- get_sentiments("afinn")
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%   # print diagram of "NOT" words
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(10) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# Use keywords that appeared 25 times or more - compile their joint cooccurring words in data structure
# Only nine out of the 18 passed the cutoff point as being "useful". Currently set at 25 this may
# need to change, if level too harsh and misses stuff.
subkeywords <- c("issue", "annoying","stupid","difficult","hard","bad","issues","hate","wrong")
new_bigrams <-  filter(bigrams_filtered, word1 %in% subkeywords) 

bigram_tf_idf <- new_bigrams %>%
  count(word1, new_bigrams) %>%
  bind_tf_idf(new_bigrams, word1, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

biggraph <- graph.data.frame(new_bigrams[,3:4],directed=FALSE)
sg1 <- subgraph.vertices(graph=biggraph, eids=which(V(biggraph)$label=="release"), delete.vertices = FALSE)
sg2 <- subgraph.vertices(graph=biggraph, eids=which(V(biggraph)$label=="hard"), delete.vertices = FALSE)
sg3 <- induced_subgraph(biggraph, 10:15,delete.vertices = TRUE)
sg3 <- subgraph.edges(biggraph, eids=which(V(biggraph)$label=="hard"), delete.vertices = TRUE)
sg3 <- subgraph.edges(biggraph, 1:5, 1:5)
plot(sg3)

subv <- c('issue','annoying','stupid','hard','bad')
g2 <- induced.subgraph(graph=biggraph,vids=subv)
simplify(g2,remove.multiple = TRUE, remove.loops = TRUE)
plot(g2)

sg1 <- decompose.graph(biggraph,mode="weak")
neighverts <- unique(unlist(sapply(sg1,FUN=function(s){if(any(V(s)$name %in% subv)) V(s)$name else NULL})))
g3 <- induced.subgraph(graph=biggraph,vids=neighverts)
simplify(g2,remove.multiple = TRUE, remove.loops = TRUE)
plot_graph(g3)

bigstats <- get_statistics(g3)

rnames <- rownames_to_column(bigstats,var = "rowname")

bigstats <- arrange(rnames,desc(betweenness,drop=FALSE))
graph.table <- xtable(bigstats[1:20,],digits=c(0,0,0,4,4,4,4))

#----------------------------------------------------------------------

bigrams_filtered %>%
  filter(word1 == "4" ) %>%
  count(topic, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(topic, bigram) %>%
  bind_tf_idf(bigram, topic, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

tdf.table <- xtable(bigram_tf_idf[1:15,])


# trigrams
trigrams <- tidy_posts %>%
  unnest_tokens(trigram, word, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


trigrams_filtered <- trigrams %>%
  filter(word1 == "time") %>%
  count(word1, word3, sort = TRUE)
trigrams_filtered





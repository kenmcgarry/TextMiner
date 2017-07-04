# analyze_posts.R
# Ken McGarry 15/3/2017

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
library(janeaustenr)
library(viridis)
library(widyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(reshape2)
library(wordcloud)
library(sentimentr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%  ungroup()
original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data("stop_words")
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

bing <- sentiments %>%   #find a sentiment score for each word using the Bing lexicon, 
  filter(lexicon == "bing") %>%
  select(-score)

bing

janeaustensentiment <- tidy_books %>%
  inner_join(bing) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

janeaustensentiment

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Jane Austen's Novels",
       y = "Sentiment") +
  scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())

austen_sentences <- austen_books() %>%
  mutate(text = iconv(text, to = 'latin1')) %>%
  group_by(book) %>%
  unnest_tokens(sentence, text, token = 'sentences') %>%
  ungroup() %>%
  #filter(row_number() == 39) %>%
  select(sentence)

bingnegative <- sentiments %>%
  filter(lexicon == "bing", sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1)


pride_prejudice_words <- tidy_books %>%
  filter(book == "Pride & Prejudice")
pride_prejudice_words

word_cooccurences <- pride_prejudice_words %>% 
  pairwise_count(word,linenumber, sort = TRUE)
word_cooccurences

best_cooccurences <- filter(word_cooccurences, n > 10)
best_cooccurences

jane <- graph.data.frame(best_cooccurences,directed=FALSE)
jane <- simplify(jane)
plot(jane)

set.seed(1813)
word_cooccurences %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  ggtitle(expression(paste("Word Network in Jane Austen's ",italic("Pride and Prejudice")))) +
  theme_void()

# Creates a wordcloud divided into positive and negative sentiments
tidy_books %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100,title.size=1)

# looking at whole sentences for overall sentiment value, using sentimentr package by Tyler Rinker
austen_sentences <- austen_books() %>% 
  group_by(book) %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  ungroup()

x<-unlist(austen_sentences)
janesentiment <- sentiment_by(x[1:10])
highlight(janesentiment)

# keywords <- c("inconvienince", "problem", "confusion", "complicated", "issue", "obstacle", "glitch") 





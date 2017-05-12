# textmine_func.R
# FUNCTIONS DEFINED FOR scrape_webpage.R -----------

# parsePage() will access the webpage given the URL and if page is the first one or not.
# This is flagged by 'base', if its 0 then first page, else base will hold a number indicating page number
# Its imporant to realise that only 25 posts are shown on a topic, so topics with many posts
# will have several pages, parsePage() will return the posts found on that page.
parsePage <- function (webpage,base){
  if(base >= 0){
    url <- paste(webpage,"?page=",base)}
  url <- gsub(" ", "",url) # get rid of any spaces found in url
  pageN <- read_html(url)
  # specify the css selector in html_nodes() and extract the text with html_text(). 
  myposts <- pageN %>% html_nodes(".ipsContained") %>% html_text() 
  myposts <- gsub("[\t\n]", "", myposts) 
  #myposts <- gsub("[[:punct:]]"," ",myposts)
  return(myposts)}

# gets the posts, needs the topics webpage and the number subsequent pages in that topic
parseAll <- function (webpage,numpages,allposts){
  for(i in 2:numpages){
    url <- paste(webpage,"?page=",i)
    url <- gsub(" ", "",url) # get rid of any spaces found in url
    pageN <- read_html(url)
    # specify the css selector in html_nodes() and extract the text with html_text(). 
    posts <- pageN %>% html_nodes(".ipsContained") %>% html_text() 
    posts <- gsub("[\t\n]", "", posts) # get rid of newlines and tabs
    #posts <- gsub("[[:punct:]]"," ",posts)
    length(posts)
    allposts <- c(allposts,posts)
  }
  return(allposts)
}


# make_topics() - creates the tidy data structure as per Julia Silge advice for
# my four topics of faqposts; followposts; toolsposts; reqposts
make_topics <- function(){
  topics <- list(`Frequently asked questions` = faqposts, `The Following` = followposts, 
                 `Developer Tools` = toolsposts, `Feature requests` = reqposts)
  ret <- data.frame(text = unlist(topics, use.names = FALSE), stringsAsFactors = FALSE)
  ret$topic <- factor(rep(names(topics), sapply(topics, length)))
  ret$topic <- factor(ret$topic, levels = unique(ret$topic))
  structure(ret, class = c("tbl_df", "tbl", "data.frame"))
}


# keyword_search() feed it with the words you think are important that negatively describe 
# the game usability, it will search all topics and provide main bi-grams of that words co-occurrence.
keyword_search <- function(mykeywords,wordpairs)
{
  knum <- length(mykeywords)
  CUTOFF <- 50
    #mygraph <- graph.data.frame(wordpairs)
  #mygraph <- as.undirected(mygraph)
  #graph_stats <- get_statistics(mygraph) # This actually does the hard work!
  # filter(msleep, sleep_total >= 16)
  
  for(i in 1:knum){
    cat("\n",i," word=",mykeywords[i])
    partners <- filter(wordpairs,item1==mykeywords[i])
    head(partners)
    #pause <- readline()
    #LCs <- calc_network_statistics(local_data)
  }
    #GCs <- calc_network_statistics(global_data)
    #list_of_stats <- #list(GCs,LCs)
  return(partners)
  tkid <- tkplot(mygraph)
    l <- tkplot.getcoords(tkid)
  tk_close(tkid,window.close = TRUE)
  
  plot(mygraph, edge.color="darkgray", 
       vertex.color="pink",
       #vertex.label=names(),
       vertex.label.cex=0.6, 
       vertex.label.font=0.5, 
       vertex.frame.color="darkgreen",
       vertex.size=18,
       vertex.label.color="black", 
       vertex.label.family = "sans", 
       layout=l)
 
    return(graph_stats)
}


# calc_network_statistics() obtains the graph theoretic calculations pertaining to hubness, betweeness etc.
calc_network_statistics <- function(pairedwords){
  # 1st stage is to convert bi-gram into igraph data structure
  
  mygraph <- graph.data.frame(pairedwords)
  
  graph_stats <- get_statistics(mygraph) # This actually does the hard work!
  
  return(graph_stats)  
}



# Calculate some statistics about the word connecitivity network
# gt is the igraph object
get_statistics <- function(gt) {
  gstats <- data.frame(
    modularity=modularity(gt, membership(cluster_walktrap(gt))),
    avepath=average.path.length(gt),
    nedges=ecount(gt),
    nverts=vcount(gt),
    transit=transitivity(gt),
    avedegree=mean(degree(gt)),
    diameter=diameter(gt,weights=NA),
    connect=is.connected(gt),
    closeness=closeness(gt),
    betweenness=betweenness(gt,directed=FALSE),
    density=graph.density(gt),
    hubness=hub_score(gt)$vector,
    authority=authority.score(gt)$vector)
  #power=bonpow(gt))
  return(gstats)
}


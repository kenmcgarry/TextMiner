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
make_topics <- function() 
{
  topics <- list(`Frequently asked questions` = faqposts, `The Following` = followposts, 
                 `Developer Tools` = toolsposts, `Feature requests` = reqposts)
  ret <- data.frame(text = unlist(topics, use.names = FALSE), stringsAsFactors = FALSE)
  ret$topic <- factor(rep(names(topics), sapply(topics, length)))
  ret$topic <- factor(ret$topic, levels = unique(ret$topic))
  structure(ret, class = c("tbl_df", "tbl", "data.frame"))
}






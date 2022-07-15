# Tweet Sentiment Analysis: Namjoon vs K-pop
# Artemis Maddox

#### Twitter Credentials ####

library(twitteR)

# API Keys and tokens - Insert within double quotes your own keys and tokens
api_key <- "8h95rsQca8z91TkG4IO7h68Cr"
api_secret <- "TfmWY1o1wIMVuYGTrg1yX9bafIIHGxF9zprbcxEqoBH7c5H11f"
access_token <- "1531350239642374146-8Kdo4dtRiIJJbP1smUoAUW1HayFVVq"
access_token_secret <- "kB0rZI6lpMMUSJzjlt4cgIAgKzzBZGYcULmxFrE68yyyj"

# Set up Twitter authorization with your keys and access tokens
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


#### Cleaning Tweets ####

# Set working directory
setwd("C:/Users/aemad/OneDrive/Documents/R/Classwork/INFS 6353/Assignment 2")

# Two tweet keywords
namjoon_tweets = searchTwitter("namjoon", n=2000, lang="en")
Kpop_tweets = searchTwitter("K-pop", n=2000, lang="en")

# Retrieve texts as vectors
namjoonTweets = sapply(namjoon_tweets, function(x) x$getText())
KpopTweets = sapply(Kpop_tweets, function(x) x$getText())

# Create function that converts all cases to lower case
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

# Create cleaning function
cleanTweets <- function(tweet){
  # Clean the tweet for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("#\\w+", " ", tweet)
  tweet = gsub("@\\w+", " ", tweet)
  tweet = gsub("[[:punct:]]", " ", tweet)
  tweet = gsub("[[:digit:]]", " ", tweet)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet=iconv(tweet, "UTF-8", "ascii",sub='')
  tweet = catch.error(tweet)
  tweet
}

# Create tweet cleaning and NA/duplicate removal function
cleanTweetsAndRemoveNAs <- function (Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove repetitive tweets
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

# Call cleanTweetsAndRemoveNAs function to clean the tweets
namjoonTweetsCleaned = cleanTweetsAndRemoveNAs(namjoonTweets)
KpopTweetsCleaned = cleanTweetsAndRemoveNAs(KpopTweets)



#### Text Mining and Word Cloud Libraries ####

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

#### Create Word Clouds ####
# Text corpus from cleaned tweets
namjoon_corp=Corpus(VectorSource(as.character(namjoonTweetsCleaned)))
Kpop_corp=Corpus(VectorSource(as.character(KpopTweetsCleaned)))

# Document-term matrix with text preprocessing 
namjoon_DTM=DocumentTermMatrix(namjoon_corp,control=list(removePunctuation=T,removeNumbers=T,stopwords=T))
Kpop_DTM=DocumentTermMatrix(Kpop_corp,control=list(removePunctuation=T,removeNumbers=T,stopwords=T))

# Displays first five terms in DTM
as.matrix(namjoon_DTM[1:10,1:5])
as.matrix(Kpop_DTM[1:10,1:5])

# Create matrix of terms and frequency
namjoon_terms=colSums(as.matrix(namjoon_DTM))
namjoon_terms_matrix=as.matrix(namjoon_terms)
namjoon_terms_matrix

Kpop_terms=colSums(as.matrix(Kpop_DTM))
Kpop_terms_matrix=as.matrix(Kpop_terms)
Kpop_terms_matrix

# Create word clouds
wordcloud(words=names(namjoon_terms), 
          freq=namjoon_terms, max.words=200, rot.per=0.25, 
          vfont=c('serif', 'bold italic'), colors=brewer.pal(8, 'Dark2'))

wordcloud(words=names(Kpop_terms), 
          freq=Kpop_terms, max.words=200, rot.per=0.25, 
          vfont=c('serif', 'bold italic'), colors=brewer.pal(8, 'Dark2'))

#### Frequent Terms with Bar Plot ####

# Frequent terms (cutoff at 50)
findFreqTerms(namjoon_DTM, lowfreq = 50)
findFreqTerms(Kpop_DTM, lowfreq = 50)

# Frequencies summed by tweet, descending
namjoon_FreqTerms=sort(rowSums(t(as.matrix(namjoon_DTM))), decreasing=TRUE)
View(namjoon_FreqTerms)

Kpop_FreqTerms=sort(rowSums(t(as.matrix(Kpop_DTM))), decreasing=TRUE)
View(Kpop_FreqTerms)

# Data frame: Word and Frequency
namjoon_FreqTermsDF=data.frame(word=names(namjoon_FreqTerms), freq=namjoon_FreqTerms)
row.names(namjoon_FreqTermsDF)=NULL # set row names to NULL
View(namjoon_FreqTermsDF)

Kpop_FreqTermsDF=data.frame(word=names(Kpop_FreqTerms), freq=Kpop_FreqTerms)
row.names(Kpop_FreqTermsDF)=NULL # set row names to NULL
View(Kpop_FreqTermsDF)


# Plot the ten most frequently used words
barplot(namjoon_FreqTermsDF[1:10,]$freq, las = 2, 
        names.arg = namjoon_FreqTermsDF[1:10,]$word,col ="lightblue", 
        main ="Most Frequent Words for Namjoon", ylab = "Word frequencies")

barplot(Kpop_FreqTermsDF[1:10,]$freq, las = 2, 
        names.arg = Kpop_FreqTermsDF[1:10,]$word,col ="lightblue", 
        main ="Most Frequent Words for K-Pop", ylab = "Word frequencies")

# Remove the first term from both as it's clearly redundant
barplot(namjoon_FreqTermsDF[2:11,]$freq, las = 2, 
        names.arg = namjoon_FreqTermsDF[2:11,]$word,col ="lightblue", 
        main ="Most Frequent Words for Namjoon", ylab = "Word frequencies")

barplot(Kpop_FreqTermsDF[2:11,]$freq, las = 2, 
        names.arg = Kpop_FreqTermsDF[2:11,]$word,col ="lightblue", 
        main ="Most Frequent Words for K-Pop", ylab = "Word frequencies")

#### Collect @NASAMILKTEA (Houston Kpop Cupsleeve Events) Friends and Followers ####
NASAMILKTEA_Friends=twListToDF(getUser('NASAMILKTEA')$getFriends(retryOnRateLimit=180))
View(NASAMILKTEA_Friends)

NASAMILKTEA_Followers=twListToDF(getUser('NASAMILKTEA')$getFollowers(retryOnRateLimit=180))
View(NASAMILKTEA_Followers)

NASAMILKTEA_FollowersDF=as.data.frame(NASAMILKTEA_Followers)
View(NASAMILKTEA_FollowersDF)

#### Social Network Graphs and Calculations ####
# From Assignment2.csv due to free Twiiter account limitations

library(igraph)

# Read in data
Ast2=read.csv('Assignment2.csv', header=T)

# Create an undirected graph
Ast2_Graph=graph_from_data_frame(Ast2, directed=F)

# List all egos
V(Ast2_Graph)

# Eliminate self-ties
Ast2_GraphSimplified=simplify(Ast2_Graph)

V(Ast2_GraphSimplified) # List all egos
E(Ast2_GraphSimplified) # List all edges

# Plot the graph
plot(Ast2_GraphSimplified)

# Plot in an interactive window
tkplot(Ast2_GraphSimplified)

#### Node Calculations ####
# Degree Centrality
degree(Ast2_GraphSimplified)

# Betweenness Centrality
betweenness(Ast2_GraphSimplified)

# Closeness Centrality
closeness(Ast2_GraphSimplified)

# Eigenvector Centrality
evcent(Ast2_GraphSimplified)

# Collate metrics
Ast2_Metrics=data.frame(cbind(degree(Ast2_GraphSimplified)))
Ast2_Metrics=cbind(Ast2_Metrics, data.frame(cbind(betweenness(Ast2_GraphSimplified))))
Ast2_Metrics=cbind(Ast2_Metrics, data.frame(cbind(closeness(Ast2_GraphSimplified))))
Ast2_Metrics=cbind(Ast2_Metrics, data.frame(cbind(evcent(Ast2_GraphSimplified)$vector)))
colnames(Ast2_Metrics)=c('Degree', 'Betweenness', 'Closeness', 'Eigenvector')
write.csv(Ast2_Metrics, 'Ast2_Metrics.csv')
View(Ast2_Metrics)

# Graph with vertex size proportional to Eigenvector centrality
# Graph colors and font size edited for clarity in visual output
plot(Ast2_GraphSimplified, vertex.size=evcent(Ast2_GraphSimplified)$vector*5^1.5, vertex.shape='sphere', 
     vertex.color='red', vertex.label.family='sans',
     vertex.label.cex=.85, vertex.label.font=1, margin=-.02)

#### Network Clusters and Communities ####
# Is the network a single cluster?
is.connected(Ast2_GraphSimplified, mode=c("weak", "strong"))
# [1] FALSE

# Identify number of clusters in the network, cluster size, and cluster of each node
clusters(Ast2_GraphSimplified, mode=c("weak", "strong"))
# $csize
# [1] 97  2  2  2  4  2  2  2  2  2
# $no
# [1] 10

# Identify Communities
Ast2_community=walktrap.community(Ast2_GraphSimplified)

# Network Modularity
modularity(Ast2_community)
# [1] 0.6582947

# Plot identified communities in network:
# Fruchterman-Reingold
plot(Ast2_community, Ast2_GraphSimplified, vertex.size=10, vertex.label.cex=0.5, 
     vertex.label=NA, edge.arrow.size=0, edge.curved=TRUE, layout=layout.fruchterman.reingold)

# Kamada-Kawai
plot(Ast2_community, Ast2_GraphSimplified, vertex.size=10, vertex.label.cex=0.5, 
     vertex.label=NA, edge.arrow.size=0, edge.curved=TRUE, layout=layout.kamada.kawai)

# Circle
plot(Ast2_community, Ast2_GraphSimplified, vertex.size=10, vertex.label.cex=0.5, 
     vertex.label=NA, edge.arrow.size=0, edge.curved=TRUE, layout=layout.circle)

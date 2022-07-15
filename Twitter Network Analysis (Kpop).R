# Twitter Network Analysis: K-pop 
# Artemis Maddox

#### Libraries ####
library(twitteR)
library(stringr)
library(igraph)
library(tm)
library(topicmodels)

#### Twitter RT Data: "Kpop" ####
# API Key, Secrets, Tokens
api_key <- "8h95rsQca8z91TkG4IO7h68Cr"
api_secret <- "TfmWY1o1wIMVuYGTrg1yX9bafIIHGxF9zprbcxEqoBH7c5H11f"
access_token <- "1531350239642374146-8Kdo4dtRiIJJbP1smUoAUW1HayFVVq"
access_token_secret <- "kB0rZI6lpMMUSJzjlt4cgIAgKzzBZGYcULmxFrE68yyyj"

# Twitter Authorization
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Working Directory
setwd("C:/Users/aemad/OneDrive/Documents/R/Classwork/INFS 6353/Assignment 3")

# Two tweet keywords
Kpop_tweets = searchTwitter("K-pop", n=600, lang="en")

# Disply first ten tweets
head(Kpop_tweets, 10)

# Get tweet texts
KpopCollect_txt = sapply(Kpop_tweets, function(x) x$getText())

# Disply only the texts of first ten tweets
head(KpopCollect_txt, 10)

# Find retweets using grep
grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
     Kpop_tweets, 
     ignore.case=TRUE, 
     value=TRUE) 

# Save results
KpopRT_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                   KpopCollect_txt, ignore.case=TRUE)


# Identify first ten retweets
head(KpopRT_patterns, 10)

# Show texts of first ten retweets
head(KpopCollect_txt[KpopRT_patterns], 10)

# Create two lists to store usernames and retweets
who_retweet_Kpop = as.list(1:length(KpopRT_patterns))
who_post_Kpop = as.list(1:length(KpopRT_patterns))


for (i in 1:length(KpopRT_patterns))
{ 
  twit = Kpop_tweets[[KpopRT_patterns[i]]]
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  poster = gsub(":", "", unlist(poster)) 
  who_post_Kpop[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  who_retweet_Kpop[[i]] = rep(twit$getScreenName(), length(poster)) 
}

# Display first 10 users for each list
head(who_post_Kpop, 10)
head(who_retweet_Kpop, 10)

# Convert the two lists to two vectors with the results
who_post_Kpop = unlist(who_post_Kpop)
who_retweet_Kpop = unlist(who_retweet_Kpop)

# Display first 10 users for each vector
head(who_post_Kpop, 10)
head(who_retweet_Kpop, 10)

# Combine the two vectors so each post author and retweeted user are paired together
RT_poster_Kpop = cbind(who_retweet_Kpop, who_post_Kpop)

# Display first 10 pairs
head(RT_poster_Kpop, 10)


#### Directed Retweet Network: "Kpop" ####
# Create a directed retweet network graph with the vertex size proportional to 
# the Eigenvector centrality of each vertex.

# Create retweet network graph with each user pair as an edge of the graph
Kpop_RTgraph = graph.edgelist(RT_poster_Kpop, directed = TRUE)

# remove self ties (i.e., people who retweeted themselves)
Kpop_RTgraph = simplify(Kpop_RTgraph)

# Get the labels' name attribute of the vertices/nodes as the vertex labels
ver_labs_Kpop = get.vertex.attribute(Kpop_RTgraph, "name", index=V(Kpop_RTgraph))

# Disply first 10 users
head(ver_labs_Kpop, 10)

# Sets the layout of the graph to the Fruchterman & Reingold layout
glay_Kpop = layout.fruchterman.reingold(Kpop_RTgraph)

# Set graph background to white and margins of graph
par(bg="white", mar=c(1,1,1,1))  

# Plot the graph
# Font made smaller and vertex color changed to yellow for visual clarity
plot(Kpop_RTgraph, layout=glay_Kpop,
     vertex.color='yellow', # Set vertex/node fill color to white
     vertex.size=3, # Vertex size
     frame.color='yellow', # Vertex border color
     vertex.label=ver_labs_Kpop, # Vertex label
     vertex.label.family="sans", # Vertex label font
     vertex.shape="sphere", # Vertex shape
     vertex.label.color='blue', # Vertex label color
     vertex.label.cex=0.4, # Vertex font size
     edge.arrow.size=0.2, # Edge arrow size
     edge.arrow.width=0.4, # Edge arrow width
     edge.width=2, # Edge width
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5)) # Edge color

# add title, font size and color
title("\nTweets with 'Kpop':  Who retweets whom",
      cex.main=1, col.main="black") 

#### Vertex Calculations ####
# Calculate the in-degree, out-degree, betweenness, closeness, and Egenvector 
# centralities of each vertex.

# Collating metrics for further analysis
RTMetrics_Kpop=data.frame(cbind(degree(Kpop_RTgraph, mode=("in"))))   # in-degree
RTMetrics_Kpop=cbind(RTMetrics_Kpop, data.frame(cbind(degree(Kpop_RTgraph, mode=("out")))))   # out-degree
RTMetrics_Kpop=cbind(RTMetrics_Kpop, data.frame(cbind(betweenness(Kpop_RTgraph))))  # betweenness
RTMetrics_Kpop=cbind(RTMetrics_Kpop, data.frame(cbind(closeness(Kpop_RTgraph))))    # closeness
RTMetrics_Kpop=cbind(RTMetrics_Kpop, data.frame(cbind(evcent(Kpop_RTgraph)$vector)))  # eigenvector
colnames(RTMetrics_Kpop)=c('In-Degree', 'Out-Degree','Betweenness', 'Closeness', 'Eigenvector')  # add column headings

# View results
View(RTMetrics_Kpop)

# Save results to a csv file
write.csv(RTMetrics_Kpop, 'RTMetrics_Kpop.csv')


#### Network Plot ####
# Plot the retweet network with the vertex size proportional to its Eigenvector 
# centrality and identify the opinion leaders in the ntework.

# Plot graph with vertex size proportional to Eigenvector centrality
# Font made smaller and vertex color changed to yellow for visual clarity
plot(Kpop_RTgraph, layout=glay_Kpop,
     vertex.color='yellow', # Set vertex/node fill color to white
     vertex.size=evcent(Kpop_RTgraph)$vector*5^1.5, # Vertex size proportional to Eigenvector centrality
     frame.color='yellow', # Vertex border color
     vertex.label=ver_labs_Kpop, # Vertex label
     vertex.label.family="sans", # Vertex label font
     vertex.shape="sphere", # Vertex shape
     vertex.label.color='blue', # Vertex label color
     vertex.label.cex=0.4, # Vertex font size
     edge.arrow.size=0.2, # Edge arrow size
     edge.arrow.width=0.4, # Edge arrow width
     edge.width=2, # Edge width
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5)) # Edge color

title("\nTweets with 'Kpop':  Who retweets whom, Vertex size proportional to Eigenvector centrality",
      cex.main=1, col.main="black") 


#### Network Communities ####
# Identify the communities in the network.

# Identify communities
Kpop_RT_community=walktrap.community(Kpop_RTgraph)

# Plot identified communities in network
plot(Kpop_RT_community, Kpop_RTgraph, vertex.size=3, vertex.label.cex=0.5, 
     vertex.label=NA, edge.arrow.size=0.2, edge.curved=TRUE, layout=layout.fruchterman.reingold)
title("\nTweets with 'Kpop':  Who retweets whom with communities",
      cex.main=1, col.main="black") 


#### Topic Modeling: LDA ####
# Perform topic modeling on the tweets using the LDA. Try two different topic 
# numbers and compare the results. Discuss which number of topics fit the data 
# better. Identify the topic each tweet is the mostly related to.

RTCorp_Kpop=Corpus(VectorSource(KpopCollect_txt))

# Text preprocessing
RTCorp_Kpop=tm_map(RTCorp_Kpop,content_transformer(tolower))

#other pre-processing tasks
RTCorp_Kpop=tm_map(RTCorp_Kpop, removeWords, stopwords('english')) # Remove stop words
RTCorp_Kpop=tm_map(RTCorp_Kpop, removePunctuation) # Remove punctuation marks 
RTCorp_Kpop=tm_map(RTCorp_Kpop, removeNumbers) # Remove numbers
RTCorp_Kpop=tm_map(RTCorp_Kpop, stripWhitespace) # Remove whitespace

# Keep only terms that appear in at least 2.5% of the tweets
RTTDM_Kpop=removeSparseTerms(TermDocumentMatrix(RTCorp_Kpop),.975)

#get a complete DTM, otherwise will have to delete documents with
RTDTM_Kpop=DocumentTermMatrix(RTCorp_Kpop)

# run LDA; Gibbs alternative is VEM; k is the number of topics we think we have
RTTopics_Kpop=LDA(RTDTM_Kpop, method='Gibbs', k=7, control=list(seed = 77))
terms(RTTopics_Kpop,10)
#output the terms and their betas - densities within topics - for each topic
RTTerms_Kpop=data.frame(row.names(t(as.matrix(RTDTM_Kpop))),t(as.matrix(RTTopics_Kpop@beta))) 
colnames(RTTerms_Kpop)=c('Term', 'Topic1', 'Topic2', 'Topic3', 'Topic4', 'Topic5', 'Topic6', 'Topic7')

#re-order data frame to highlight top 10 terms for 1st topic
RTTerms_Kpop=RTTerms_Kpop[order(RTTerms_Kpop$Topic1, decreasing = 'T'),]
RTTerms_Kpop[1:20,] # reports natural log of probabilities, hence negative

topics(RTTopics_Kpop)				#determine which topic dominates each document

#add the topics to original RTPosts data frame
RTPosts_Kpop=data.frame(KpopCollect_txt,Topic=topics(RTTopics_Kpop))

#add the density of each topic within each document
#to the data frame
RTPosts_Kpop=data.frame(RTPosts_Kpop,RTTopics_Kpop@gamma) # gamma is the document densities over different topics

View(RTPosts_Kpop)

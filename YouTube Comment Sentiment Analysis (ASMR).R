# YouTube Comment Sentiment Analysis: ASMR
# Artemis Maddox

#### Libraries ####
library(tuber)
library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(curl)
library(plyr)
library(ggplot2)
library(gridExtra)

#### YouTube API Client Info ####

# Working Directory: 
setwd("C:/Users/aemad/OneDrive/Documents/R/Classwork/INFS 6353/Assignment 4")

# Client Info:
client_id="563911353285-clpog4u9ksif289ekht5shj4dkpvp9f3.apps.googleusercontent.com"
client_secret="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"
yt_oauth(client_id, client_secret, token='')

#### Video Data and Comments ####
# Video: "ASMR for Those Who Want to Sleep Soundly Now / 3Hr (No Talking)"
get_stats(video_id="-SYwOAe6V_4")
get_video_details(video_id="-SYwOAe6V_4")

# Comments:
ASMR_comments <- get_comment_threads(c(video_id="-SYwOAe6V_4"))
View(ASMR_comments)
write.csv(ASMR_comments, file='ASMR_Comments.csv')

#### Comment Word Cloud ####
# Comment Corpus:
ASMR_comments_corp=Corpus(VectorSource(ASMR_comments$textOriginal))

# Text Processing: DTM
ASMR_comments_DTM=DocumentTermMatrix(ASMR_comments_corp,
                  control=list(removePunctuation=T,removeNumbers=T,stopwords=T))
as.matrix(ASMR_comments_DTM[,1:5])

# Term Matrix:
ASMR_comments_terms=colSums(as.matrix(ASMR_comments_DTM))
ASMR_comments_terms_matrix=as.matrix(ASMR_comments_terms)
ASMR_comments_terms_matrix

# Word Cloud:
wordcloud(words=names(ASMR_comments_terms), freq=ASMR_comments_terms, 
          vfont=c('serif', 'bold italic'), colors=1:nrow(ASMR_comments_terms_matrix))

#### Comment Emotion Barplot ####
# Raw Comment Sentiment:
ASMR_video_sentiment=get_nrc_sentiment(as.character(ASMR_comments$textOriginal))
View(ASMR_video_sentiment)
ASMR_video_sentimentDF=t(data.frame(ASMR_video_sentiment)) 
View(ASMR_video_sentimentDF)

# Emotional Comments:
ASMR_EmotionsDFCount=data.frame(rownames(ASMR_video_sentimentDF), rowSums(ASMR_video_sentimentDF > 0))
View(ASMR_EmotionsDFCount)
rownames(ASMR_EmotionsDFCount)=NULL 
colnames(ASMR_EmotionsDFCount)=c('Emotion','Frequency') 
View(ASMR_EmotionsDFCount)

# Comment Sentiment Barplot:
barplot(ASMR_EmotionsDFCount$Frequency,  names.arg = ASMR_EmotionsDFCount$Emotion, 
        main="YouTube Video Comments Sentiment", xlab="Emotions", ylab="Frequency")


#### Comment Polarity Score ####
# Comment Sentiment:
ASMR_Polarity=data.frame(as.character(ASMR_comments$textOriginal),
                         get_sentiment(as.character(ASMR_comments$textOriginal)))
colnames(ASMR_Polarity)=c('Comments','Polarity') # Set column name to "Polarity"
View(ASMR_Polarity)

#### Videos by Keyword ####
# Search YouTube for ASMR Videos:
ASMR_vids <- yt_search(term = "ASMR", max_results = 50)
ASMR_vids2 <- yt_search("ASMR")
View(ASMR_vids)
#I'm getting an error during my final run-through; I don't know why.
#> ASMR_vids <- yt_search(term = "ASMR", max_results = 50)
#Error: HTTP failure: 403
#> ASMR_v2 <- yt_search("ASMR")
#Error: HTTP failure: 403

#Create Dataframe: 
# NOTE: This is the only way I found that works, due to using yt_search() instead 
#       of list_videos() for the keyword search function causing a difference in the 
#       returned data.
ASMR_vidsDF = as.data.frame(ASMR_vids[,1])
colnames(ASMR_vidsDF)=c("Video ID")

# Results:
View(ASMR_vidsDF)

# Function: Scrape Video Stats
get_all_stats <- function(id) {
  get_stats(id)
} 

# Data Frame of Stats:
ASMR_video_stats <- lapply(as.vector(ASMR_vidsDF$`Video ID`), get_all_stats)
ASMR_video_stats_df <- rbind(ldply(ASMR_video_stats, data.frame))
ASMR_video_stats_df$viewCount<-as.numeric(ASMR_video_stats_df$viewCount)
ASMR_video_stats_df$likeCount<-as.numeric(ASMR_video_stats_df$likeCount)
ASMR_video_stats_df$favoriteCount<-as.numeric(ASMR_video_stats_df$favoriteCount)
ASMR_video_stats_df$commentCount<-as.numeric(ASMR_video_stats_df$commentCount)
View(ASMR_video_stats_df)


# Save to csv:
#write.csv(ASMR_video_stats_df, file='ASMR_VideoStats.csv')
# Pulled from csv:
ASMR_video_stats_df <- as.data.frame(ASMR_VideoStats) 

# Plots: Video View Count vs. Likes, Dislikes, Comments
p1 = ggplot(data = ASMR_video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = ASMR_video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = favoriteCount))
p3 = ggplot(data = ASMR_video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)


#### Channel Stats ####
# Get channel statistics: CoromoSaraASMR
CS_ASMR_chstat = get_channel_stats("UCmZb4LwQRhEzZX5Uqpcqziw")

CS_ASMR_playlists_ids <- get_playlists(filter=c(channel_id="UCmZb4LwQRhEzZX5Uqpcqziw"))
CS_ASMR_playlist_id <- CS_ASMR_playlists_ids$items[[2]]$id

CS_ASMR_videos <- get_playlist_items(
  filter = c(playlist_id=CS_ASMR_playlist_id)) 
CS_ASMR_video_ids <- as.vector(CS_ASMR_videos$contentDetails.videoId)

# Data Frame of Stats: 
CS_ASMR_video_stats <- lapply(CS_ASMR_video_ids, get_all_stats)
CS_ASMR_video_stats_df <- do.call(rbind, lapply(CS_ASMR_video_stats, data.frame))
CS_ASMR_video_stats_df$viewCount<-as.numeric(CS_ASMR_video_stats_df$viewCount)
CS_ASMR_video_stats_df$likeCount<-as.numeric(CS_ASMR_video_stats_df$likeCount)
CS_ASMR_video_stats_df$favoriteCount<-as.numeric(CS_ASMR_video_stats_df$favoriteCount)
CS_ASMR_video_stats_df$commentCount<-as.numeric(CS_ASMR_video_stats_df$commentCount)
View(CS_ASMR_video_stats_df)

# Save to csv:
#write.csv(CS_ASMR_ASMR_video_stats_df, file='CS_ASMR_VideoStats.csv')
# Pull from csv:
CS_ASMR_video_stats_df <- as.data.frame(CS_ASMR_VideoStats)


# Plots: Video View Count vs. Likes, Dislikes, Comments
p1 = ggplot(data = CS_ASMR_video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = CS_ASMR_video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = favoriteCount))
p3 = ggplot(data = CS_ASMR_video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)

#### Channel Comments Word Cloud ####
# Function: Retrieve Comments on Single Video
#   Note: Max Results reduced to 100 due to data processing time. 100 gives 1MB of results
get_video_comments <- function(id){
  get_comment_threads(c(video_id = id), max_results = 100)
}

# Comments on Videos
CS_ASMR_comments=lapply(as.character(CS_ASMR_video_ids), get_video_comments)

# Data Frame: Video 1 Comments
View(data.frame(CS_ASMR_comments[1]))

# Data Frame: Video 2 Comments
View(data.frame(CS_ASMR_comments[2]))

# Transform to Text for Processing:
CS_ASMR_comments_text = lapply(CS_ASMR_comments,function(x){
  as.character(x$textOriginal)
})

View(data.frame(CS_ASMR_comments_text[1]))

# Merge All Comments:
CS_ASMR_text = Reduce(c, CS_ASMR_comments_text)
View(CS_ASMR_text)

# Comment Text Corpus:
CS_ASMR_comments_corp=Corpus(VectorSource(CS_ASMR_text))
CS_ASMR_comments_DTM=DocumentTermMatrix(CS_ASMR_comments_corp,
                    control=list(removePunctuation=T,removeNumbers=T,stopwords=T))
as.matrix(CS_ASMR_comments_DTM[,1:5])

# Term Matrix:
CS_ASMR_comments_terms=colSums(as.matrix(CS_ASMR_comments_DTM))
CS_ASMR_comments_terms_matrix=as.matrix(CS_ASMR_comments_terms)
CS_ASMR_comments_terms_matrix

# Word Cloud:
wordcloud(words=names(CS_ASMR_comments_terms), freq=CS_ASMR_comments_terms, 
          vfont=c('serif', 'bold italic'), colors=brewer.pal(8, 'Dark2'))

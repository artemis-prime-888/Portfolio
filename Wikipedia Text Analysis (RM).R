# Wikipedia Text Analysis: RM (Rap Monster, aka Namjoon)
# Artemis Maddox

#### Libraries ####
library(WikipediR)

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

#### Text Data from Wikipedia ####
# Wikipedia page: "RM (rapper)" ~ RM is the professional name of Kim Namjoon, 
# from previous analyses
namjoonContent <- page_content("en","wikipedia", page_name = "RM (rapper)")
head(namjoonContent)
class(namjoonContent)

#### Cleaning Text ####
# Set working directory
setwd("C:/Users/aemad/OneDrive/Documents/R/Classwork/INFS 6353/Assignment 6")

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
cleanText <- function(text){
  # Clean the text for sentiment analysis
  text = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", text)
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", text)
  text = gsub("#\\w+", " ", text)
  text = gsub("@\\w+", " ", text)
  text = gsub("[[:punct:]]", " ", text)
  text = gsub("[[:digit:]]", " ", text)
  text = gsub("[ \t]{2,}", " ", text)
  text = gsub("^\\s+|\\s+$", "", text)
  text=iconv(text, "UTF-8", "ascii",sub='')
  text = catch.error(text)
  text
}

# Create text cleaning and NA/duplicate removal function
cleanTextsAndRemoveNAs <- function (Texts) {
  TextCleaned = sapply(Texts, cleanText)
  # Remove the "NA" tweets
  TextCleaned = TextCleaned[!is.na(TextCleaned)]
  names(TextCleaned) = NULL
  # Remove repetitive tweets
  TextCleaned = unique(TextCleaned)
  TextCleaned
}

# Call cleanTextsAndRemoveNAs function to clean the tweets
namjoonTextCleaned = cleanTextsAndRemoveNAs(namjoonContent)

#### Create Word Clouds ####
# Text corpus from cleaned tweets
namjoon_corp=Corpus(VectorSource(as.character(namjoonTextCleaned)))

# Document-term matrix with text preprocessing 
namjoon_DTM=DocumentTermMatrix(namjoon_corp,control=list(removePunctuation=T,removeNumbers=T,stopwords=T))
namjoon_DTM


# Displays first fifteen terms in DTM
as.matrix(namjoon_DTM[1:3,1:15])
#> # Displays first fifteen terms in DTM
#  > as.matrix(namjoon_DTM[1:3,1:15])
#Terms
#Docs rapper active age align alt amp apgujeong artist async auto awards background bday behind
#1      1      0   0     0   0   0         0      0     0    0      0          0    0      0
#2      0      0   0     0   0   0         0      0     0    0      0          0    0      0
#3      2      1   1     1   2   2         3      1     2    2      1          2    2      9
#Terms
#Docs big
#1   0
#2   0
#3   3


# Create matrix of terms and frequency
namjoon_terms=colSums(as.matrix(namjoon_DTM))
namjoon_terms_matrix=as.matrix(namjoon_terms)
namjoon_terms_matrix

# Create word clouds
wordcloud(words=names(namjoon_terms), 
          freq=namjoon_terms, max.words=200, rot.per=0.25, 
          vfont=c('serif', 'bold italic'), colors=brewer.pal(8, 'Dark2'))


#### Frequent Terms with Bar Plot ####

# Frequent terms
findFreqTerms(namjoon_DTM, lowfreq = 15)
# Entirely page coding words:
# > findFreqTerms(namjoon_DTM, lowfreq = 15)
# [1] "class"   "data"    "div"     "href"    "infobox" "output"  "parser"  "style"   "title"  
# [10] "wiki" 

findFreqTerms(namjoon_DTM, lowfreq = 10)
# More style words:
# > findFreqTerms(namjoon_DTM, lowfreq = 10)
# [1] "boy"      "class"    "cultural" "data"     "div"      "href"     "infobox"  "label"   
# [9] "luv"      "merit"    "order"    "output"   "parser"   "row"      "scope"    "span"    
# [17] "style"    "title"    "wiki" 

# Frequencies summed by text, descending
namjoon_FreqTerms=sort(rowSums(t(as.matrix(namjoon_DTM))), decreasing=TRUE)
View(namjoon_FreqTerms)

# Data frame: Word and Frequency
namjoon_FreqTermsDF=data.frame(word=names(namjoon_FreqTerms), freq=namjoon_FreqTerms)
row.names(namjoon_FreqTermsDF)=NULL # set row names to NULL
View(namjoon_FreqTermsDF)

# Remove the ten most frequently used words, plot the next ten
barplot(namjoon_FreqTermsDF[11:20,]$freq, las = 2, 
        names.arg = namjoon_FreqTermsDF[11:20,]$word,col ="lightblue", 
        main ="Most Frequent Words for Namjoon", ylab = "Word frequencies")


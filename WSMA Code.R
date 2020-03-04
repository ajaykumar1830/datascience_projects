install.packages("RColorBrewer")
install.packages("tm")
install.packages("wordcloud")
install.packages('base64enc')
install.packages('plyr')
install.packages('stringi')
install.packages('twitteR')
install.packages("SnowballC")
install.packages("qdap")
install.packages("RCurl")
install.packages("purrr")
install.packages("readtext")
install.packages("topicmodels")##say no to binary compilation package type

library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(dplyr)
library(stringr)
library(SnowballC)
library(base64enc)
library(rJava)
library(qdap)
library(RCurl)
library(purrr)
library(plyr)
library(readtext)
library(ggplot2)
library(topicmodels)

setwd("C:/Users/AK57630/Downloads/GreatLakes/Text Analytics")

insta_text <- readLines("Obama.txt")
##Corpus from collection of text files(Corpus is colletion of documents in R readable format)
insta_text_corpus <- Corpus(VectorSource(insta_text))

##Transform Text To Lower Case

insta_text_clean_corpus <- tm_map(insta_text_corpus, content_transformer(tolower))

# Remove numbers
insta_text_clean_corpus <- tm_map(insta_text_clean_corpus, removeNumbers)

##Remove punctuation
insta_text_clean_corpus <- tm_map(insta_text_clean_corpus, removePunctuation) 
##Remove Stop Words
insta_text_clean_corpus <- tm_map(insta_text_clean_corpus, removeWords, c("are", "that", "the", "of", "if","what", 
                                                              "when", "where", "how", "in", "out","then", "therefore", "is", "you",
                                                              "who", "all", "will", "with", "our", "they", "from", "about", "your", 
                                                              "this", "has", "was", "but", "this", "for", "and", "or","his", "just", 
                                                              "their", "due", "get", "have", "can", "would", "should","passed",
                                                              "last","front", "just", "year", "than", "been", "see", "why", 
                                                              "smaller", "women", "men", "old", "new","some", "few", "must", "he",
                                                              "she", "let","off", "on", "word", "lets", "more", "less"))


##Stemming
insta_text_clean_corpus <- tm_map(insta_text_clean_corpus, stemDocument)

wordcloud(insta_text_clean_corpus,min.freq=2)
wordcloud(insta_text_clean_corpus,min.freq=2, colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F)
wordcloud(insta_text_clean_corpus,min.freq=5, colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F)


dtmObama = DocumentTermMatrix(insta_text_clean_corpus)
findFreqTerms(dtmObama, lowfreq = 5)
findAssocs(dtmObama,"hope",0.5)
findAssocs(dtmObama,"forward", 0.5)


President_Corpus <- Corpus(DirSource(directory = "C:/Users/AK57630/Downloads/GreatLakes/Text Analytics/President Comparison"))
President_clean_corpus <- tm_map(President_Corpus, content_transformer(tolower))

# Remove numbers
President_clean_corpus <- tm_map(President_clean_corpus, removeNumbers)

##Remove punctuation
President_clean_corpus <- tm_map(President_clean_corpus, removePunctuation)
##Remove Stop Words
President_clean_corpus <- tm_map(President_clean_corpus, removeWords, c("are", "that", "the", "of", "if","what", 
                                                                          "when", "where", "how", "in", "out","then", "therefore", "is", "you",
                                                                          "who", "all", "will", "with", "our", "they", "from", "about", "your", 
                                                                          "this", "has", "was", "but", "this", "for", "and", "or","his", "just", 
                                                                          "their", "due", "get", "have", "can", "would", "should","passed",
                                                                          "last","front", "just", "year", "than", "been", "see", "why", 
                                                                          "smaller", "women", "men", "old", "new","some", "few", "must", "he",
                                                                          "she", "let","off", "on", "word", "lets", "more", "less"))


##Stemming
President_clean_corpus <- tm_map(President_clean_corpus, stemDocument)
wordcloud(President_clean_corpus,min.freq=2)
wordcloud(President_clean_corpus,  width=12,height=8, min.freq=3, colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F)

#Create Comparison Cloud

STM = TermDocumentMatrix(President_clean_corpus)
STM = as.matrix(STM)
colnames(STM) <- c("Obama","Trump")
comparison.cloud(STM, max.words = 100, random.order = F)
commonality.cloud(STM, max.words = 100, random.order = F)


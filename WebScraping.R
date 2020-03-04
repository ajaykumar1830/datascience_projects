library("rvest")
#Website call to scrape
url = "https://www.cardekho.com/best-suv"
webpage = read_html(url)

car_name_html = html_nodes(webpage, 'a.title.undefined')
car_name_data = html_text(car_name_html)
head(car_name_data)

car_price_html = html_nodes(webpage, 'div.price')
car_price_data = html_text(car_price_html)
head(car_price_data)


car_details_html = html_nodes(webpage, 'div.dotlist.BottomMarginRemove')
car_details_data = html_text(car_details_html)
head(car_details_data)

amazon_url <- "https://www.amazon.in/Apple-iPhone-XR-64GB-Black/product-reviews/B07JWV47JW" 
web_page <- read_html(amazon_url)
feedback_html = html_nodes(web_page, 'div.a-row.a-spacing-small.review-data')
feedback = html_text(feedback_html)
head(feedback)
write.table(feedback,"IphoneFeedback.txt", sep ="")

library(tm)
library(wordcloud)
Iphone_Cloud <- readLines("IphoneFeedback.txt")
#Create Corpus
IXRCorpus = Corpus(VectorSource(Iphone_Cloud))
IXRCorpus

#Clean Corpus
IXR_Clean_Corpus <- tm_map(IXRCorpus, tolower) #convert all text to lower
IXR_Clean_Corpus <- tm_map(IXR_Clean_Corpus, removeNumbers) #remove numbers
IXR_Clean_Corpus <- tm_map(IXR_Clean_Corpus, removePunctuation) #remove Punctuation
IXR_Clean_Corpus <- tm_map(IXR_Clean_Corpus, stripWhitespace) #remove any extra whitespaces
IXR_Clean_Corpus <- tm_map(IXR_Clean_Corpus, removeWords, c("are", "that", "the", "of", "if","what", 
                                                     "when", "where", "how", "in", "out","then", "therefore", "is", "you",
                                                     "who", "all", "will", "with", "our", "they", "from", "about", "your", 
                                                     "this", "has", "was", "but", "this", "for", "and", "or","his", "just", 
                                                     "their", "due", "get", "have", "can", "would", "should","passed",
                                                     "last","front", "just", "year", "than", "been", "see", "why", 
                                                     "smaller", "women", "men", "old", "new","some", "few", "must", "he",
                                                     "she", "let","off", "on", "word", "lets", "more", "less")) #remove stop words
IXR_Clean_Corpus <- tm_map(IXR_Clean_Corpus, stemDocument) # Stem Document

wordcloud(IXR_Clean_Corpus, min.freq = 2)
wordcloud(IXR_Clean_Corpus,min.freq=2, colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F)


library(sentimentr)
library(syuzhet)
library(ggplot2)
txt1 = gsub("http[^[:blank:]]+"," ",feedback) #Remove html
txt2 = gsub("@\\w+","",txt1) #Remove names
txt3 = gsub("[[:punct:]]"," ",txt2) 
txt4 = gsub("[^[:alnum:]]+"," ",txt3)

IXRSentiment <- get_nrc_sentiment(txt4)
SentimentScore <-  data.frame(colSums(IXRSentiment[,]))
names(SentimentScore) = "Score"
SentimentScore = cbind("sentiment" = rownames(SentimentScore), SentimentScore)
rownames(SentimentScore) = NULL
ggplot(data=SentimentScore, aes(x=sentiment, y=Score))+ geom_bar(aes(fill=sentiment), stat = "identity")+theme(legend.position = "none")
+xlab("Sentiment") + ylab("Score") + ggtitle("Sentiment Score derived from reviews")

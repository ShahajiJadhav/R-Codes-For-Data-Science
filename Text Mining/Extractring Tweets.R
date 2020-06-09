#Name- Shahaji Jadhav
##1) Extract tweets for any user (try choosing a user who has more tweets)
##2) Perform sentimental analysis on the tweets extracted from the above

library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
#Setting tweeter Authorization
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

twitteR:::setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                              "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                              "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                              "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#Extracting tweets from tweeter of PMOIndia
Tweets <-userTimeline('PMOIndia', n=3000,includeRts = T)#User has 3200 of tweets cap
TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, 'PMOIndia.csv', row.names = F)
getwd()
#Perform sentimental analysis on the tweets extracted from the above
PMOIndia <- read.csv(file.choose())
View(PMOIndia)
#Some of the rows & columns are not required. So we will remove those columns
PMOIndia <- PMOIndia$text

View(PMOIndia)
library(tm)
my_stopwords <- c(stopwords('english'),"the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

PMO_Corp <- Corpus(VectorSource(PMOIndia))
PMO_Corp <- tm_map(PMO_Corp, removePunctuation) 
PMO_Corp <- tm_map(PMO_Corp, removeNumbers) 
PMO_Corp <- tm_map(PMO_Corp, stripWhitespace)
PMO_Corp <- tm_map(PMO_Corp, removeWords, my_stopwords)
inspect(PMO_Corp)

tdm <- TermDocumentMatrix(PMO_Corp)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)
library("wordcloud")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, 
          colors=brewer.pal(8, "Dark2"))

df <- data.frame(text = sapply(PMO_Corp, as.character), stringsAsFactors = FALSE)
View(df)
##Sentiment Analysis
library(syuzhet)
sent_nrc <- get_nrc_sentiment(df$text)
barplot(colSums(sent_nrc), col = rainbow(15), las= 2 )

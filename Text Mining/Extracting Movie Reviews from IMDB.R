##Name- Shahaji jadhav
##Extract movie reviews for any movie from IMDB and perform sentimental analysis
library(rvest)
library(xml2)
library(dplyr)
url<-read_html("https://www.imdb.com/title/tt4154796/reviews?ref_=tt_urv")

Avengers_review <- url%>%
      html_nodes('.show-more__control')%>%
      html_text()

View(Avengers_review)

class(Avengers_review)
write.csv(Avengers_review, 'D:\\Avengers_review.csv', row.names = FALSE)
av_rev <- read.csv('D:\\Avengers_review.CSV')

library(tm)
my_stopwords <- c(stopwords('english'),"the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

av <- Corpus(VectorSource(av_rev))
av <- tm_map(av, removePunctuation) 
av <- tm_map(av, removeNumbers) 
av <- tm_map(av, stripWhitespace)
av <- tm_map(av, removeWords, my_stopwords)
inspect(av)

tdm <- TermDocumentMatrix(av)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)
library("wordcloud")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, 
          colors=brewer.pal(8, "Dark2"))


review <- read.table('D:\\Avengers_review.txt', header = T)
View(review)
str(review)
##Sentiment Analysis for the whole reviews
library(syuzhet)
re <- as.character(review$x)
s <- get_nrc_sentiment(re)
barplot(colSums(s), las = 2, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Inception movie reviews')

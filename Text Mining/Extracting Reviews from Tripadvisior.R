## Extracting reviews from a travel website ###################
library(rvest)
a<-10
rev<-NULL
url1<-"https://www.tripadvisor.com/Restaurant_Review-g297628-d2216191-Reviews-or"
url2<-"-Kebabs_Kurries-Bengaluru_Bangalore_District_Karnataka.html"

for(i in 0:15){
  url<-read_html(as.character(paste(url1,i*a,url2,sep="")))
  ping<-url %>%
    html_nodes(".partial_entry") %>%
    html_text() 
  rev<-c(rev,ping)
}
write.table(rev,"D:\\K&K_Review.txt")
knk_Review <- read.table('D:\\K&K_Review.txt')
View(knk_Review)
stop <- read.table(file.choose())
stop1 <- unlist(stop)
library(tm)
my_stopwords <- c(stopwords('english'), stop1)

knk <- Corpus(VectorSource(knk_Review))
knk <- tm_map(knk, removeNumbers) 
knk <- tm_map(knk, removePunctuation) 
knk <- tm_map(knk, stripWhitespace)
knk <- tm_map(knk, removeWords, my_stopwords)
knk <- tm_map(knk, tolower)
inspect(knk)

dtm <- TermDocumentMatrix(knk)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)
library("wordcloud")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, 
          colors=brewer.pal(8, "Dark2"))

df <- data.frame(text = sapply(knk, as.character), stringsAsFactors = FALSE)
View(df)
##Sentiment Analysis
library(syuzhet)
sent_nrc <- get_nrc_sentiment(df$text)
barplot(colSums(sent_nrc), col = rainbow(15), las= 2 )

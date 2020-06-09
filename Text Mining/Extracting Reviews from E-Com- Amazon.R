url <- "https://www.amazon.in/Samsung-Galaxy-Ocean-Blue-Storage/product-reviews/B07HGJKDQL/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber="
amazon_reviews <- NULL
i=1
while(i<100){
  murl <- read_html(paste(url,i,sep=""))
  a <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  i = i+1
  amazon_reviews <-rbind(amazon_reviews, a)
   }

View(amazon_reviews)
class(amazon_reviews)
AR <- data.frame(amazon_reviews)
View(AR)
AR <- data.frame(AR)
colnames(AR) <- "Reviews"
write.table(AR, 'D:/AR.csv',sep = ',', row.names = F)

ar <- read.csv(file.choose())
View(ar)

library(tm)
my_stopwords <- c(stopwords('english'),"the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

ar <- Corpus(VectorSource(ar))
ar <- tm_map(ar, removePunctuation)
ar <- tm_map(ar, removeNumbers)
ar <- tm_map(ar, stripWhitespace)
ar <- tm_map(ar, removeWords, my_stopwords)
inspect(ar)

dtm <- TermDocumentMatrix(ar)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
View(d)
library("wordcloud")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, 
          colors=brewer.pal(8, "Dark2"))

df <- data.frame(text = sapply(ar, as.character), stringsAsFactors = FALSE)
View(df)
##Sentiment Analysis
library(syuzhet)
sent_nrc <- get_nrc_sentiment(df$text)
barplot(colSums(sent_nrc), col = rainbow(15), las= 2 )

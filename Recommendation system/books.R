##Name- Shahaji Jadhav
#Recommend a best book based on the author, publisher and ratings.
# Libraries Requiered
library(recommenderlab)
library(reshape2)

#Read Data- Books
books <- read.csv(file.choose())
View(books)
#Changing colomn names
books <- books[1:150,]
books <- na.omit(books)
colnames(books)[1] <- 'UserID' 
colnames(books)[5] <- 'Rating' 
#colomn names
#"UserID" "Book.Title" "Book.Author" "Publisher" "Rating" 
head(books)
books$Book.Title <- as.factor(books$Book.Title)
books$Book.Author <- as.factor(books$Book.Author)
## covert to matrix format
books_matrix <- as.matrix(acast(books, UserID~Book.Title , fun.aggregate = mean))

#the datatype should be realRatingMatrix inorder to build recommendation engine
books_realR_matrix <- as(books_matrix, 'realRatingMatrix')

## User-based collaborative filtering
uid <- 50
books_UBFC = Recommender(books_realR_matrix, method="UBCF") 
pred_books_UBFC <- predict(books_UBFC, books_realR_matrix[uid], n=8)
as(pred_books_UBFC, 'list')

## Item-based collaborative filtering
books_IBFC = Recommender(books_realR_matrix, method="IBCF") 
pred_books_IBFC <- predict(books_IBFC, books_realR_matrix[uid], n=2)
as(pred_books_IBFC, 'list')

##SVD- based collaborative filtering
books_SVD = Recommender(books_realR_matrix, method="SVD")
pred_books_SVD <- predict(books_SVD, books_realR_matrix, n=2)
as(pred_books_SVD, 'list')

##POPULAR-based collaborative filtering
books_POPULAR = Recommender(books_realR_matrix, method="POPULAR")
pred_books_POPULAR <- predict(books_POPULAR, books_realR_matrix[uid], n=2)
as(pred_books_POPULAR, 'list')

## binarize all 2+ rating to 1
books_UBFC_binarize = Recommender(binarize(books_realR_matrix,minRating=2), method="UBCF") 
pred_books_UBFC_bI <- predict(books_UBFC_binarize, books_realR_matrix[uid], n=7)
as(pred_books_UBFC_bI, 'list')



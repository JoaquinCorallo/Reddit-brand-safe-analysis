#reads the reddit2 file
reddit2 <- read.csv(file="C:/.../Reddit2.csv")
reddit2 <- reddit2[,-1]

#clean the title data
library(tm)
title <- reddit2$title
title <- VectorSource(title)
corpus <- Corpus(title)
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
library(SnowballC)
corpus <- tm_map(corpus, stemDocument)

#create a matrix of word frequency
dtm <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(500,Inf))))
dtm$v <- rep(1,length(dtm$v))
dtm2 <- as.matrix(dtm)

#create a list with words that have 500 or more frequency
v <- sort(rowSums(dtm2),decreasing=TRUE)
d <- data.frame(freq=v)
d

#delete title variable and bind the new matrix
dtm2 <- t(dtm2)
reddit2 <- reddit2[,-54]
reddit2 <- cbind(reddit2,dtm2)

#create a new reddit document
write.csv(reddit2, file="C:/.../Reddit3.csv")


#reads the reddit3 file
reddit3 <- read.csv(file="C:/.../Big Data/Project/Reddit/Reddit3.csv")
reddit3 <- reddit3[,-1]

#library to manage factors
library(tidyverse)
library(forcats)

#look at each variable to know if a value is seen less than 30 times and to see if its binary
tail(fct_count(reddit3[,1], sort=TRUE)) # recode and remove 
tail(fct_count(reddit3[,2], sort=TRUE)) # recode
tail(fct_count(reddit3[,3], sort=TRUE)) #
tail(fct_count(reddit3[,4], sort=TRUE)) # recode
tail(fct_count(reddit3[,5], sort=TRUE)) # recode
tail(fct_count(reddit3[,6], sort=TRUE)) # recode
tail(fct_count(reddit3[,7], sort=TRUE)) # 
tail(fct_count(reddit3[,8], sort=TRUE)) #
tail(fct_count(reddit3[,9], sort=TRUE)) #
tail(fct_count(reddit3[,10], sort=TRUE)) # recode
tail(fct_count(reddit3[,11], sort=TRUE)) # recode
tail(fct_count(reddit3[,12], sort=TRUE)) # 
tail(fct_count(reddit3[,13], sort=TRUE)) # 
tail(fct_count(reddit3[,14], sort=TRUE)) # recode
tail(fct_count(reddit3[,15], sort=TRUE)) # recode
tail(fct_count(reddit3[,16], sort=TRUE)) # recode
tail(fct_count(reddit3[,17], sort=TRUE)) # recode
tail(fct_count(reddit3[,18], sort=TRUE)) # recode
tail(fct_count(reddit3[,20], sort=TRUE)) # recode and remove
tail(fct_count(reddit3[,21], sort=TRUE)) #recode
tail(fct_count(reddit3[,22], sort=TRUE)) #recode
tail(fct_count(reddit3[,23], sort=TRUE)) #
tail(fct_count(reddit3[,24], sort=TRUE)) #
tail(fct_count(reddit3[,25], sort=TRUE)) #
tail(fct_count(reddit3[,26], sort=TRUE)) #recode
tail(fct_count(reddit3[,27], sort=TRUE)) #recode
tail(fct_count(reddit3[,28], sort=TRUE)) #
tail(fct_count(reddit3[,29], sort=TRUE)) #recode
tail(fct_count(reddit3[,30], sort=TRUE)) #recode
tail(fct_count(reddit3[,31], sort=TRUE)) #recode
tail(fct_count(reddit3[,34], sort=TRUE)) #
tail(fct_count(reddit3[,35], sort=TRUE)) #
tail(fct_count(reddit3[,36], sort=TRUE)) #recode
tail(fct_count(reddit3[,37], sort=TRUE)) #
tail(fct_count(reddit3[,38], sort=TRUE)) #recode
tail(fct_count(reddit3[,39], sort=TRUE)) #
tail(fct_count(reddit3[,40], sort=TRUE)) #
tail(fct_count(reddit3[,42], sort=TRUE)) #recode
tail(fct_count(reddit3[,43], sort=TRUE)) #recode
tail(fct_count(reddit3[,44], sort=TRUE)) #recode
tail(fct_count(reddit3[,45], sort=TRUE)) #
tail(fct_count(reddit3[,46], sort=TRUE)) #
tail(fct_count(reddit3[,47], sort=TRUE)) #recode
tail(fct_count(reddit3[,48], sort=TRUE)) #recode
tail(fct_count(reddit3[,49], sort=TRUE)) #recode
tail(fct_count(reddit3[,50], sort=TRUE)) #recode
tail(fct_count(reddit3[,51], sort=TRUE)) #recode
tail(fct_count(reddit3[,54], sort=TRUE)) #recode
tail(fct_count(reddit3[,55], sort=TRUE)) #

reddit3[0,c(1,2,4,5,6,10,11,14,15,16,17,18,20,21,22,26,27,29,30,31,36,38,42,43,44,47,48,49,50,51,54)]

#group factors that appear less than 30 times in a category called "Other"
thresh <- 30
for(i in 1:241){
  if (is.factor( reddit3[,i])) {
    levels(reddit3[,i])[table(reddit3[,i]) < thresh] <- "Other"
  }
}

#list of variables with more than 100 factors
levelsTable<-c()
for(i in 1:56){
    levelsTable <- rbind(levelsTable,c(i,nlevels(reddit3[,i])))
}  
levelsTable

reddit3[0,c(2,14,26,27,47,48)]

#keep only 100 factors
for(i in 1:241){
  if (is.factor( reddit3[,i])) {
    reddit3[,i] <- fct_lump(reddit3[,i],n = 100, ties.method = "random")
  }
}

#remove binary factors with one value less than 30 times
reddit3[0,c(1,20)]
reddit3 <- reddit3[,c(-1,-20)]

#remove columns with only 1 factor again
reddit3 <- reddit3[c(TRUE, lapply(reddit3[-1], var, na.rm = TRUE) != 0)]

#create a new reddit document
write.csv(reddit3, file="C:/.../Big Data/Project/Reddit/Reddit4.csv")

#reads the reddit3 file
reddit4 <- read.csv(file="C:/.../Reddit4.csv")
reddit4 <- reddit4[,-1]

#remove colums with na
reddit4[0, colSums(is.na(reddit4)) != 0]
reddit4 <- reddit4[ , colSums(is.na(reddit4)) == 0]

#create a list of all factors and number of levels
levelsTable <- reddit4[0,] %>%  select_if(is.factor)
levelsTable2<-c()
for(i in 1:233){
  if (is.factor( reddit4[,i])) {
  levelsTable2 <- rbind(levelsTable2,c(nlevels(reddit4[,i])))
  }  
}
cbind(t(levelsTable),levelsTable2)

#create a new reddit document
write.csv(reddit4, file="C:/.../Reddit5.csv")
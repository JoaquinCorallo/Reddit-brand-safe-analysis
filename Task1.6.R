#reads clean data and librarys
load("C:/.../clean.RData")
library(tidyverse)

#separate numeric var
numericvar <- select(reddit, c('gilded','score','num_comments','num_crossposts'))
reddit <- select(reddit,-c('gilded','score','num_comments', 'num_crossposts'))

#transform numeric var
numericvar <- numericvar %>% 
  mutate(log_gilded = log10(gilded + 1)) %>%
  mutate(log_score = log10(gilded + 1)) %>%
  mutate(log_num_comments = log10(num_comments + 1)) %>%
  mutate(log_num_crossposts = log10(num_crossposts + 1))
numericvar <- select(numericvar,-c('gilded','score','num_comments','num_crossposts'))

#convert word matrix to factors
for (i in 1:224){
  if (is.numeric( reddit[,i])) {
    reddit[,i] <- as.factor(reddit[,i])
  }  
}

#bind variables
reddit <- cbind(reddit,numericvar)

#linear model
lm.full <- lm(log_score~.,data=reddit)

#performance
glance <- broom::glance(lm.full)

#check linearity
plot(lm.full, which = 1)
#check constant spread
plot(lm.full, which = 3)
#check normality
plot(lm.full, which = 2)
#independance?

#Aliasing
names(coef(lm.full))[is.na(coef(lm.full))]
reddit[0,c('whitelist_status', 'subreddit_id', 'subreddit', 'secure_media_embed', 'promoted', 'preview', 'parent_whitelist_status', 'href_url', 'domain', 'disable_comments', 'crosspost_parent_list', 'author_flair_text')]
reddit <- select(reddit,-c('whitelist_status', 'subreddit_id', 'subreddit', 'secure_media_embed', 'promoted', 'preview', 'parent_whitelist_status', 'href_url', 'domain', 'disable_comments', 'crosspost_parent_list', 'author_flair_text'))
lm.full <- update(lm.full)
names(coef(lm.full))[is.na(coef(lm.full))]

#create a models without authorcake and contestmode variables
lmauthorcake <- select(reddit,-c('author_cakeday'))
lmcontestmode <- select(reddit,-c('contest_mode'))
lm.authorcake <- lm(log_score~.,data=lmauthorcake)
lm.contestmode <- lm(log_score~.,data=lmcontestmode)

#check performance with and without those variables
AIC(lm.full)
AIC(lm.authorcake)
BIC(lm.full)
BIC(lm.authorcake)

AIC(lm.full)
AIC(lm.contestmode)
BIC(lm.full)
BIC(lm.contestmode)

#predict case 1
case1 <- predict.lm(lm.full,reddit[1400,], interval = "prediction")
case1num <- as.numeric(case1)
score1 <- (10^case1num)-1
scoretable1 <- rbind(case1,score1)
scoretable1

#predict case 2
redditcase2 <-  reddit[1400,]
redditcase2$author <- fct_other(redditcase2$author, keep = NULL)
redditcase2$author

newnumcrossposts <- log10(7 + 1)
redditcase2$log_num_crossposts <- newnumcrossposts
redditcase2$log_num_crossposts

case2 <- predict.lm(lm.full,redditcase2, interval = "prediction")
case2num <- as.numeric(case2)
score2 <- (10^case2num)-1
scoretable2 <- rbind(case2,score2)
scoretable2
filter(reddit$log_num_crossposts)
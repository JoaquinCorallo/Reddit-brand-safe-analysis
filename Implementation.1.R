#reads the reddit file
reddit <- read.csv(file="C:/.../Reddit.csv")

#select relevant variables
library(tidyverse)
reddit <- select(reddit,c('brand_safe','num_crossposts','over_18', 'is_self', 'is_reddit_media_domain', 'is_video', 'stickied', 'spoiler'))
summary(reddit)

#define formulas
glm_formula <- formula(brand_safe ~ over_18 + is_self + is_reddit_media_domain + 
                        is_video + stickied + spoiler)
glm_formula2 <- formula(brand_safe ~ over_18 + is_self + is_reddit_media_domain + 
                         is_video + stickied + spoiler + num_crossposts)

#General linear model
reddit.glm <- glm(glm_formula, family = binomial(), reddit)
summary(reddit.glm)

reddit.glm2 <- glm(glm_formula2, family = binomial(), reddit)
summary(reddit.glm2)

#AIC and BIC for both linear models
AIC(reddit.glm)
AIC(reddit.glm2)
BIC(reddit.glm)
BIC(reddit.glm2)

#Stepwise Forward
glm.null <- glm(brand_safe ~ 1, family = binomial(), reddit)
step(glm.null, scope = list(upper = reddit.glm), direction = "forward", steps = 3)
reddit[0,c('over_18', 'is_self', 'is_reddit_media_domain')]

#Stepwise Backward
step(reddit.glm, scope = list(lower = glm.null), direction = "backward", steps = 3)
reddit[0,c('stickied', 'is_video', 'spoiler')]

#split the data into train and test
set.seed(13)
splitdata <-  sort(sample(nrow(reddit), nrow(reddit)*.8))
train <- reddit[splitdata,]
test <- reddit[-splitdata,]

#make predictions with full model
reddit.glmtrain1 <- glm(glm_formula, family = binomial(), train)
pred1 <- predict.glm(reddit.glmtrain1,test, type = "response")
test$pred1 <- pred1
test$pred1 <- ifelse(test$pred1>0.5,1,0)
test$brand_safe <- ifelse(test$brand_safe=="True",1,0)
test$acc1 <- ifelse(test$pred1==test$brand_safe,1,0)
accuarcyfullmodel <- (sum(test$acc1)/nrow(test))*100
accuarcyfullmodel

#make predictions with simplified model
glm_formula3 <- formula(brand_safe ~ over_18 + is_self + is_reddit_media_domain) 
reddit.glmtrain2 <- glm(glm_formula3, family = binomial(), train)
pred2 <- predict.glm(reddit.glmtrain2,test, type = "response")
test$pred2 <- pred2
test$pred2 <- ifelse(test$pred2>0.5,1,0)
test$acc2 <- ifelse(test$pred2==test$brand_safe,1,0)
accuarcysimplemodel <- (sum(test$acc2)/nrow(test))*100

#compare accuarcy of 2 models
accuarcytable <- rbind(accuarcyfullmodel,accuarcysimplemodel)
accuarcytable

#ROC function
get_roc <- function(L, f) {
  # Calculate P and N
  P <- sum(L==1)
  N <- sum(L==0)
  # Order the observations by prediction
  df  <- tibble(L, f)
  df <- df %>% arrange(desc(f))
  # Set TP and FP to zero
  TP <- 0
  FP <- 0
  # Set up matrix for results
  R <- NULL
  # Set previous f
  f_prev <- -Inf
  # set counter
  i <- 1
  while(i <= length(df$L)){
    if( df$f[i] != f_prev){
      R <- rbind(R, c(FP/N, TP/P, df$f[i]))
      f_prev <- df$f[i]
    }
    if(df$L[i] == 1){
      TP <- TP + 1
    } else {
      FP <- FP + 1
    }
    i <- i + 1
  }
  R <- rbind(R, c(FP/N, TP/P, f_prev))
  R <- data.frame(R)
  colnames(R) <- c("FPR","TPR", "Score")
  return(R)
}

#ROC curves for simplified and full model
test$bad_pred  <- ifelse(test$brand_safe>0,1,1)
bad_ROC  <- get_roc(L = test$brand_safe, f = test$bad_pred)
ROCfullmodel  <- get_roc(L = test$brand_safe, f = test$pred1)
ROCsimplemodel  <- get_roc(L = test$brand_safe, f = test$pred2)
ggplot(bad_ROC, aes(x = FPR, y = TPR)) + geom_line(aes(col = "Bad prediction")) +
  geom_line(data = ROCfullmodel, aes(col = "Full model prediction")) +
  geom_line(data = ROCsimplemodel, aes(col = "Simple model prediction"))
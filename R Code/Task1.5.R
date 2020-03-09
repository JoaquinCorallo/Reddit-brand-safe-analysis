#reads clean data and librarys
load("C:/.../clean.RData")
library(tidyverse)
library(hexbin)

#view numeric variables
reddit[0,] %>%  select_if(is.numeric)

#create dataframes vs score to plot and see the difference
scoregilded <- reddit[,c('score', 'gilded')]
scorenumcomments <- reddit[,c('score', 'num_comments')]
scorenum_crossposts <- reddit[,c('score', 'num_crossposts')]

#plot with and without transformation
scoregilded %>% 
  ggplot(aes(x = gilded, y = score)) +
  geom_hex()  

scoregilded %>% 
  mutate(log_gilded = log10(gilded + 1)) %>%
  mutate(log_score = log10(gilded + 1)) %>%
  ggplot(aes(x = log_gilded, y = log_score)) +
  geom_point()  

scorenumcomments %>% 
  ggplot(aes(x = num_comments, y = score)) +
  geom_hex()
  

scorenumcomments %>% 
  mutate(log_num_comments = log10(num_comments + 1)) %>%
  mutate(log_score = log10(num_comments + 1)) %>%
  ggplot(aes(x = log_num_comments, y = log_score)) +
  geom_hex()
  

scorenum_crossposts %>% 
  ggplot(aes(x = num_crossposts, y = score)) +
  stat_bin_hex()  

scorenum_crossposts %>% 
  mutate(log_num_crossposts = log10(num_crossposts + 1)) %>%
  mutate(log_score = log10(num_crossposts + 1)) %>%
  ggplot(aes(x = log_num_crossposts, y = log_score)) +
  geom_point()
  

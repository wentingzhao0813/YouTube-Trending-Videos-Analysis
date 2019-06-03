# URL to project: https://www.kaggle.com/datasnaek/youtube-new/home

# US Videos

library(tidyverse)
library(jsonlite)
library(forcats)
library(broom)

# Load data from each country
can_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/CAvideos.csv")
de_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/DEvideos.csv")
fr_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/FRvideos.csv")
gb_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/GBvideos.csv")
us_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/USvideos.csv")

# get the category list
ca_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/CA_category_id.json")
de_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/DE_category_id.json")
fr_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/FR_category_id.json")
gb_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/GB_category_id.json")
us_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/US_category_id.json")

glimpse(us_videos)
glimpse(de_videos)
glimpse(fr_videos)
glimpse(gb_videos)
glimpse(us_videos)


# Clean US data
## Obtain Category
us_item <- us_category$items

us_item$id <- as.integer(us_item$id)
us_videos <- us_videos %>%
  left_join(us_item, by=c('category_id'='id'))

us_videos$category <- us_videos$snippet$title
us_videos <- subset(us_videos, select = -snippet)

## Get correct trending date
us_videos$trending_date <- as.Date(us_videos$trending_date, format = "%y.%d.%m")

# US VIDEOS
# Channels with the most trending videos

channels <- us_videos %>%
  group_by(channel_title) %>%
  summarize(n.t.videos = n_distinct(title),
            views = max(views)) %>%
  arrange(desc(n.t.videos)) %>%
  slice(1:20) %>%
  ggplot(aes(x=fct_reorder(channel_title,n.t.videos),y=n.t.videos, fill = n.t.videos)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Youtube Channel')+ylab('# of Trending Videos') + theme(legend.position = 'none')

# Categories with the most trending videos

categories <- us_videos %>%
  group_by(category) %>%
  drop_na() %>%
  summarize(n.t.videos = n_distinct(title)) %>%
  arrange(desc(n.t.videos)) %>%
  slice(1:20) %>%
  ggplot(aes(x=fct_reorder(category,n.t.videos),y=n.t.videos, fill = n.t.videos)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Category')+ylab('Number of Trending Videos') + theme(legend.position = 'none')

# Top YouTube Channels in US of trending videos - by total views

views <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  group_by(channel_title) %>%
  summarize(views = sum(as.numeric(views)),
            likes = sum(as.numeric(likes)),
            dislikes = sum(dislikes),
            n.comments = sum(comment_count)) %>%
  arrange(desc(views)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(channel_title,views),y=views/1000000, fill = views)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Channel')+ylab('Video Views (Millions)') + theme(legend.position = 'none') +
  ggtitle('Channels with Most Trending Video Views')

# Of channels with most views, how many videos do they have?

highest.view.chan.videos <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  group_by(channel_title) %>%
  summarize(views = sum(as.numeric(views)),
            n.t.videos = n()) %>%
  arrange(desc(views)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(channel_title,n.t.videos),y=n.t.videos, fill = n.t.videos)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Channel')+ylab('# of Trending Videos') + theme(legend.position = 'none') +
  ggtitle('Number of Trending Videos for Channels with Most Views')

# Most liked channels
liked.channels <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  group_by(channel_title) %>%
  summarize(views = sum(as.numeric(views)),
            likes = sum(as.numeric(likes)),
            dislikes = sum(dislikes),
            n.comments = sum(comment_count)) %>%
  arrange(desc(likes)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(channel_title,likes),y=likes/1000000, fill = likes)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Channel')+ylab('Video Likes (Millions)') + theme(legend.position = 'none') +
  ggtitle('Channels with Most Likes')

# Most disliked channels
disliked.channels <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  group_by(channel_title) %>%
  summarize(dislikes = sum(dislikes)) %>%
  arrange(desc(dislikes)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(channel_title,dislikes),y=dislikes/1000000, fill = dislikes)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Channel')+ylab('Video Dislikes (Millions)') + theme(legend.position = 'none') +
  ggtitle('Trending Video Channels with Most Dislikes')

# Channels with the most Comments

most.comments <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  group_by(channel_title) %>%
  summarize(n.comments = sum(comment_count)) %>%
  arrange(desc(n.comments)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(channel_title,n.comments),y=n.comments/1000, fill = n.comments)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Channel')+ylab('# of Comments (Thousands)') + theme(legend.position = 'none') +
  ggtitle('Channels with Most Comments')


# What were some of the most trending videos over the time period?

trending.videos <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  summarize(views = sum(as.numeric(views)),
            likes = sum(as.numeric(likes)),
            dislikes = sum(dislikes),
            n.comments = sum(comment_count)) %>%
  arrange(desc(views)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(title,views),y=views/1000000, fill = views)) + 
  geom_bar(stat='identity') + coord_flip() +
  xlab('Video')+ylab('# of Views (Millions)') +
  ggtitle('Videos with the Most Views') + theme(legend.position = 'none')

# Top 10 Most liked videos

liked.videos <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  summarize(likes = sum(likes)) %>%
  arrange(desc(likes)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(title,likes),y=likes/1000, fill = likes)) + 
  geom_bar(stat='identity') + coord_flip() +
  xlab('Video')+ylab('# of Likes (Thousands)') +
  ggtitle('Videos with the Most Likes') + theme(legend.position = 'none')


# Top 10 Most Disliked Trending Videos

disliked.videos <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  summarize(dislikes = sum(dislikes)) %>%
  arrange(desc(dislikes)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(title,dislikes),y=dislikes/1000, fill = dislikes)) + 
  geom_bar(stat='identity') + coord_flip() +
  xlab('Video')+ylab('# of Dislikes (Thousands)') +
  ggtitle('Videos with the Most Dislikes') + theme(legend.position = 'none')

# Videos with greatest difference between likes and dislikes

likes.vs.dislikes <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  summarize(likes = sum(likes),
            dislikes = sum(dislikes),
            diff = (likes - dislikes)) %>%
  arrange(desc(likes)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(title,diff),y=diff/1000000, fill = diff)) + 
  geom_bar(stat='identity') + coord_flip() +
  xlab('Video')+ylab('Likes > Dislikes (Millions)') +
  ggtitle('Videos with the Most Likes > Dislikes') + theme(legend.position = 'none')


# Of trending videos, average views - this is not that cool 

avg.views <- us_videos %>%
  group_by(title) %>%
  filter(trending_date == max(trending_date)) %>%
  group_by(channel_title) %>%
  summarize(avg.views = mean(as.numeric(views)),
            avg.likes = mean(as.numeric(likes)),
            avg.dislikes = mean(dislikes),
            avg.comments = mean(comment_count)) %>%
  arrange(desc(avg.views)) %>%
  slice(1:10) %>%
  ggplot(aes(x=fct_reorder(channel_title,avg.views),y=avg.views/1000000, fill = avg.views)) + 
  geom_bar(stat='identity') + coord_flip()+
  xlab('Channel Title')+ylab('Average Video Views (Millions)') + theme(legend.position = 'none') +
  ggtitle('Channels with Highest Average Views')


# Regression on Views - this has errors

# Set test data
valFrac <- 0.8

test <- us_videos %>%
  sample_frac(1.0-valFrac)

us_videos %>%
  ggplot(aes(x=views/1000)) + geom_histogram(bindwidth = 50)  + xlab("Views")


lm.us.all <- lm(log(views)~likes+dislikes+comment_count, data=test)

summary(lm.us.all)

results.df <- tidy(lm.us.all,conf.int = T)

extract.coef <- function(res.df,var){
  res.df %>%
    filter(grepl(var,term)) %>%
    separate(term,into=c('var','level'),sep=nchar(var))
}

train2 <- train %>%
  sample_frac(1.0-0.2)

pred.df <- data.frame(log.views = log(test$views),
                      log.views.pred = predict(lm.us.all,newdata=test),
                      us.all = test$views)

pred.df %>%  
  ggplot(aes(x=log.views,y=log.views.pred)) + geom_point() + geom_line(aes(x=log.views,y=log.views)) + 
  xlab('Actual Log Views') + ylab('Predicted Log Views') + coord_fixed() +
  theme(legend.position="none")

lm.us.all.1 <- lm(log(views)~likes+dislikes+comment_count, data=train)

predTest <- data.frame(views = us_videos$views,
                       viewsPred = exp(predict(lm.us.all.1,newdata = us_videos))) 

lmPred <- predTest %>%
  summarize(MAE = mean(abs(views-viewsPred)),
            MSE = mean((views-viewsPred)**2),
            RMSE = sqrt(MSE))

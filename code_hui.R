library(tidyverse)
library(jsonlite)
library(forcats)
library(broom)
library(scales)
library(forcats)
library(wordcloud)
library(tidytext)
library(lubridate)
library(tm)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(topicmodels)
library(tidytext)
library(SnowballC)
library(LDAvis)


# Load data from each country
can_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/CAvideos.csv")
de_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/DEvideos.csv")
fr_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/FRvideos.csv",stringsAsFactors = F)
gb_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/GBvideos.csv")
us_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/USvideos.csv",stringsAsFactors = F)

# get the category list
ca_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/CA_category_id.json")
de_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/DE_category_id.json")
fr_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/FR_category_id.json")
gb_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/GB_category_id.json")
us_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/US_category_id.json")

glimpse(us_videos)
glimpse(ger_videos)
glimpse(fr_videos)
glimpse(gb_videos)
glimpse(us_videos)




# Clean data
## Obtain Category

de_item <- de_category$items

de_item$id <- as.integer(de_item$id)
de_videos <- de_videos %>%
  left_join(de_item, by=c('category_id'='id'))

de_videos$category <- de_videos$snippet$title
de_videos <- subset(de_videos, select = -snippet)

# number of videos under each title length
us_videos$title_length <- str_count(us_videos$title,"\\S+")
us_title.length_num.vidoes <- us_videos %>%
  group_by(title_length) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=title_length, y=n,fill=12))+geom_bar(stat = 'identity')+theme(legend.position = "none")+
  labs(title='What is the distribution of title length of trending videos in US',x='Title length',y='Number of trending videos') 
us_title.length_num.vidoes


# title lenth under each category
us_title_length_category <- us_videos %>%
  group_by(category) %>%
  summarize(title.mean=mean(title_length)) %>%
  ggplot(aes(x=fct_reorder(category,title.mean), y=title.mean,fill=title.mean))+geom_bar(stat = 'identity')+coord_flip()+theme(legend.position = "none")+
  labs(title="The average title length for each video category in France", x="Category", y="Title length")
us_title_length_category



  
### text mining
#top 10 title
commonwords <- c("?","wwe","?","2","4","1","5","10","2018","?","top","tb",'ft','season','mon','?','?','?','??','?','?','70','s5','e5','e1','21')

us_top10_title_word <- us_videos%>%
  select(category,title) %>%
  unnest_tokens(word,title) %>% 
  count(category,word) %>%
  anti_join(stop_words) %>%
  group_by(category) %>%
  filter(!word %in% commonwords) %>%
  mutate(word = wordStem(word)) %>%
  arrange(-n) %>%
  top_n(10)



plot_us_top10_title <- us_top10_title_word %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=category)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  facet_wrap(~category,scales = 'free',nrow=3) + 
  theme_bw() + 
  theme(legend.position = "none")+
  ylab("US Top 10 Title Words")

plot_us_top10_title



#top 10 tags
us_top10_tags_word <- us_videos%>%
  select(category,tags) %>%
  unnest_tokens(word,tags) %>% 
  count(category,word) %>%
  anti_join(stop_words) %>%
  group_by(category) %>%
  filter(!word %in% commonwords) %>%
  mutate(word = wordStem(word)) %>%
  top_n(10) 



plot_us_top10_tags <- us_top10_tags_word %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=category)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  facet_wrap(~category,scales = 'free',nrow=3) + 
  theme_bw() + 
  theme(legend.position = "none")+
  ylab("US Top 10 Tag Words")
plot_us_top10_tags


#Analyze on France
de_videos <- read.csv("DEvideos.csv",stringsAsFactors = F)
de_category <- fromJSON("DE_category_id.json")

# Clean data
## Obtain Category
de_item <- de_category$items

de_item$id <- as.integer(de_item$id)
de_videos <- de_videos %>%
  left_join(de_item, by=c('category_id'='id'))

de_videos$category <- de_videos$snippet$title
de_videos <- subset(de_videos, select = -snippet)

# number of videos under each title length
de_videos$title_length <- str_count(de_videos$title,"\\S+")
de_title.length_num.vidoes <- de_videos %>%
  group_by(title_length) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=title_length, y=n,fill=n))+geom_bar(stat = 'identity')+theme(legend.position = "none") +
  scale_fill_gradient(low='darkgoldenrod1',high='red1')+
  labs(title='What is the distribution of trending videos in Germany',x='Title length',y='Number of trending videos') 

fr_title.length_num.vidoes


# title lenth under each category
fr_title_length_category <- fr_videos %>%
  group_by(category) %>%
  filter(category != "NA") %>%
  summarize(title.mean=mean(title_length)) %>%
  ggplot(aes(x=fct_reorder(category,title.mean), y=title.mean,fill=title.mean))+geom_bar(stat = 'identity')+coord_flip()+theme(legend.position = "none")+
  scale_fill_gradient(low='darkorchid1',high='darkorchid4')+
  labs(title="The average title length for each video category in France", x="Category", y="Title length")
fr_title_length_category

## When were trending videos published? On which days of the week? at which times of the day?

library(lubridate)
library(scales)
library(radiant)

fr_videos$trending_date <- as.Date(fr_videos$trending_date,format='%y.%d.%m')

fr_videos$hour <- substr(fr_videos$publish_time,start=12,stop=13)

fr_videos <- fr_videos %>%
  mutate(weekday = wday(publish_time,label=FALSE, abbr = TRUE),
         hour = hour(publish_time))
### visualize

fr_weekday <- fr_videos %>%
  group_by(weekday) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = factor(weekday),y=n,fill=n)) + geom_bar(stat = 'identity') +
  scale_fill_gradient(low='darkorchid1',high='darkorchid4')+
  ylim(0,8000)+
  theme(legend.position = "none")+
  labs(title='How the day in a week influence the number of trending videos in fr?',x='the time in a week',y='the number of trending videos') 
fr_weekday


fr_time <- fr_videos %>%
  group_by(hour)%>%
  summarize(n=n())%>%
  ggplot(aes(x = hour,y=n,fill=n)) + geom_bar(stat = 'identity') + coord_flip()+
  scale_fill_gradient(low='darkorchid1',high='darkorchid4') +
  theme(legend.position = "none")+
  labs(title='How the time in a day influence the number of trending videos in France?',x='the time in a day(hour)',y='the number of trending videos') 

fr_time


## how many days does it take to make a video become trending?

fr_videos <- fr_videos %>%
  mutate(publish_time_day = as.Date(publish_time),
         trending_days = difftime(trending_date,publish_time_day,units = 'day'))


fr_videos$trending_days = as.integer(fr_videos$trending_days)

fr_trendingdays <- fr_videos %>%
  mutate(time_to_trend = cut(trending_days,breaks=c(-1,0,2,4,6,8,10,12,14,30,60,100,300),
                             include.lowest=T,
                             labels = c('0','1-2','3-4','5-6','7-8','9-10','10-12','13-14','15-30','31-60','61-100','101-300'))) %>%
  group_by(time_to_trend) %>%
  summarize(n=n())


fr_trendingdays_plot <- fr_trendingdays %>%
  filter(time_to_trend != 'NA') %>%
  ggplot(aes(x=factor(time_to_trend),y=n,fill=n)) + geom_bar(stat = 'identity') + 
  scale_fill_gradient(low='skyblue1', high="dodgerblue4") +
  labs(title='How long will take a video to be trending in fr?',x='Time to trend(days)',y='Number of trending videos')


fr_trendingdays_plot


## Will the number of tags have an influence on the trend?

fr_videos <- fr_videos %>%
  mutate(number_of_tags = sapply(strsplit(tags,'|',fixed = TRUE),length))

frtags <- fr_videos %>%
  group_by(number_of_tags) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=factor(number_of_tags),y=n, fill = number_of_tags)) + geom_bar(stat = 'identity') + 
  scale_fill_gradient(low='dodgerblue', high="firebrick1") +
  labs(title='How Number of tags influence the trending video in fr?',x='number of tags',y='number of trending videos')
frtags

## top 10 trending video(fre views)

top10_view <- fr_videos %>%
  group_by(video_id) %>%
  summarize(sum_view=sum(views),
            number_of_tags=median(number_of_tags)) %>%
  arrange(desc(sum_view)) %>%
  slice(1:10) %>%
  mutate(tag_range = cut(number_of_tags,breaks=c(6,10,15,20,30,40),
                         include.lowest=T,
                         labels = c('6-10','10-15','15-20','20-30','>30')))

top10_view <- inner_join(top10_view,select(fr_videos,video_id,category,title),by='video_id')
top10_view <- unique(top10_view[,])

top10_tag <- top10_view %>% 
  ggplot(aes(x=tag_range,fill=category)) +geom_bar()

top10_tag

### which category contains more trending videos?
sort(table(fr_videos$category))

### how long does every video last trending? 
fr_repeat <- fr_videos %>%
  count(title) %>%
  arrange(desc(n))


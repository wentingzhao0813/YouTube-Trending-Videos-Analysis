# load USvideos.csv file
library(readr)
library(tidyverse)
library(jsonlite)
USvideos <- read_csv("Data/USvideos.csv")
us_category <- fromJSON("~/Dropbox/MGTA-452 Group Project/Data/US_category_id.json")
us_item <- us_category$items


## Clean data

USvideos$category_id <- as.character(USvideos$category_id)

USvideos <- USvideos %>%
  left_join(us_item, by=c('category_id'='id'))

USvideos$category <- USvideos$snippet$title

USvideos <- select(USvideos,-snippet)

## When were trending videos published? On which days of the week? at which times of the day?

library(lubridate)
library(scales)
library(radiant)

USvideos$trending_date <- as.Date(USvideos$trending_date,format='%y.%d.%m')

USvideos <- USvideos %>%
  mutate(weekday = wday(publish_time_day,label=TRUE, abbr = TRUE),
         hour = hour(publish_time))
### visualize

US_weekday <- USvideos %>%
  group_by(weekday) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = factor(weekday),y=n,fill=n)) + geom_bar(stat = 'identity') +
  scale_fill_gradient(low='skyblue1', high="dodgerblue4") +
  ylim(0,8000)+
  theme(legend.position="none") + 
  labs(title='How the day in a week influence the number of trending videos in US?',x='the time in a week',y='the number of trending videos') 

  

US_time <- USvideos %>%
  group_by(hour)%>%
  summarize(n=n())%>%
  ggplot(aes(x = factor(hour),y=n,fill=n)) + geom_bar(stat = 'identity') + coord_flip()+
  scale_fill_gradient(low='skyblue1', high="dodgerblue4") +
  theme(legend.position="none") + 
  labs(title='How the time in a day influence the number of trending videos in US?',x='the time in a day(hour)',y='the number of trending videos') 

US_time
US_weekday

## how many days does it take to make a video become trending?

USvideos <- USvideos %>%
  mutate(publish_time_day = as.Date(publish_time),
  trending_days = difftime(trending_date,publish_time_day,units = 'day'))


USvideos$trending_days = as.integer(USvideos$trending_days)

US_trendingdays <- USvideos %>%
  mutate(time_to_trend = cut(trending_days,breaks=c(-1,0,2,4,6,8,10,12,14,30,60,100,300),
                             include.lowest=T,
                             labels = c('0','1-2','3-4','5-6','7-8','9-10','10-12','13-14','15-30','31-60','61-100','101-300'))) %>%
  group_by(time_to_trend) %>%
  summarize(n=n())


US_trendingdays_plot <- US_trendingdays %>%
  filter(time_to_trend != 'NA') %>%
  ggplot(aes(x=factor(time_to_trend),y=n,fill=n)) + geom_bar(stat = 'identity') + 
  scale_fill_gradient(low='skyblue1', high="dodgerblue4") +
  labs(title='How long will take a video to be trending in US?',x='Time to trend(days)',y='Number of trending videos')


US_trendingdays_plot


## Will the number of tags have an influence on the trend?

USvideos <- USvideos %>%
  mutate(number_of_tags = sapply(strsplit(tags,'|',fixed = TRUE),length))

UStags <- USvideos %>%
  group_by(number_of_tags) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=factor(number_of_tags),y=n, fill = number_of_tags)) + geom_bar(stat = 'identity') + 
  scale_fill_gradient(low='dodgerblue', high="firebrick1") +
  theme(legend.position="none") + 
  labs(title='How Number of tags influence the trending video in US?',x='number of tags',y='number of trending videos')
UStags

## top 10 trending video(use views)

top10_view <- USvideos %>%
  group_by(video_id) %>%
  summarize(sum_view=sum(views),
            number_of_tags=median(number_of_tags)) %>%
  arrange(desc(sum_view)) %>%
  slice(1:10) %>%
  mutate(tag_range = cut(number_of_tags,breaks=c(6,10,15,20,30,40),
                             include.lowest=T,
                             labels = c('6-10','10-15','15-20','20-30','>30')))

top10_view <- inner_join(top10_view,select(USvideos,video_id,category,title),by='video_id')
top10_view <- unique(top10_view[,])

top10_tag <- top10_view %>% 
  ggplot(aes(x=tag_range,fill=category)) +geom_bar()

top10_tag

### which category contains more trending videos?
sort(table(USvideos$category))

### how long does every video last trending? 
US_repeat <- USvideos %>%
  count(title) %>%
  arrange(desc(n))



### logistic regression(views~number_of_tags+weekday+hour+category)

#### adjust weekdays into weekday and weekends
USvideos$weekdayornot <- ifelse(USvideos$weekday==6|USvideos$weekday==7,0,1)
USvideos$weekdayornot <- factor(USvideos$weekdayornot)

#### adjust hour into two segments
USvideos$morningornot <- ifelse(USvideos$hour<13,1,0)
USvideos$morningornot <- factor(USvideos$morningornot)

  
#### adjust number of tags
USvideos <- USvideos %>%
  mutate(tagsrange=cut(number_of_tags,breaks = c(0,5,10,20,30,40,50,60,70)))

#### build model
library(glmnet)
USvideos$trendingornot <- ifelse(USvideos$views>median(USvideos$views)|USvideos$likes>median(USvideos$likes),1,0)
train<- USvideos[1:30000,]
validation <- USvideos[30001:40949,]


logit1 <- glm((trendingornot==1)~tagsrange+weekdayornot+morningornot+category,data=train,family=binomial(link="logit"))
summary(logit1)

validation$prop <- predict(logit1, newdata = validation, type = "response")

validation$pred <- ifelse(validation$prop>0.5,1,0)

accuracy <- mean(validation$pred==validation$trendingornot,na.rm=TRUE)

#### ggplot
validation %>%
  ggplot(aes(x=tagsrange,y=prop,color=weekdayornot)) + geom_line(linetype='dotted') + 
  facet_wrap(~category) + 
  geom_point() +
  theme(axis.text.x = element_text(angle=45))

validation %>%
  ggplot(aes(x=tagsrange,y=prop,color=morningornot)) + geom_line(linetype='dotted') + 
  geom_point() + facet_wrap(~category) +
  theme(axis.text.x = element_text(angle=45))

#### text mining
library(tidytext)
library(wordcloud)

for (i in USvideos$description){
  USvideos$sentence <- strsplit(i,' ')
}


USvideos %>%
  unnest_tokens(word,description) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


### across countries
countries <- readRDS("~/Dropbox (Your team)/MGTA-452 Group Project/robbie_code/countries.rds")
countries <- countries %>%
  mutate(publish_time_day = as.Date(publish_time),
         trending_days = difftime(trending_date,publish_time_day,units = 'day'))

countries$trending_days = as.integer(countries$trending_days)


countries_trendingdays <- countries %>%
  mutate(time_to_trend = cut(trending_days,breaks=c(-1,0,2,4,6,8,10,12,14,30,60,100,300),
                             include.lowest=T,
                             labels = c('0','1-2','3-4','5-6','7-8','9-10','10-12','13-14','15-30','31-60','61-100','101-300'))) %>%
  group_by(country,time_to_trend) %>%
  summarize(n=n())

countires_time_to_trend <- countries_trendingdays %>%
  filter(time_to_trend != 'NA') %>%
  ggplot(aes(x=factor(time_to_trend),y=n,fill=factor(country))) + geom_bar(stat = 'identity') + 
  scale_fill_manual(values=c("red2","sandybrown","seagreen",'lightpink','dodgerblue2'),labels=c('Canada','Germany','France','Great Britain','United States')) +
  labs(title='How long will it take a video to be trending across five countries?',x='Time to trend(days)',y='Number of trending videos')

countires_time_to_trend

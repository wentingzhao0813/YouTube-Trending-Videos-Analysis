library(tidyverse)
library(scales)
library(forcats)
library(RColorBrewer)
library(wordcloud)
library(tidytext)
library(lubridate)
library(NLP)
library(tm)

library(data.table)
library(dplyr)

library(DT)


library(ggplot2)
library(plotrix)


library(stringr)
library(RDocumentation)

gb <- fread("~/Dropbox/MGTA-452 Group Project/Data/GBvideos.csv",encoding = "UTF-8",nrows = 1000)
fr <- fread("~/Dropbox/MGTA-452 Group Project/Data/FRvideos.csv",encoding = "UTF-8",nrows = 1000)
ca <- fread("~/Dropbox/MGTA-452 Group Project/Data/CAvideos.csv",encoding = "UTF-8",nrows = 1000)
us <- fread("~/Dropbox/MGTA-452 Group Project/Data/USvideos.csv",encoding = "UTF-8",nrows = 1000)
de <- fread("~/Dropbox/MGTA-452 Group Project/Data/DEvideos.csv",encoding = "UTF-8",nrows = 1000)

videos <- as.data.table(rbind(gb,fr,ca,us,de))
#us_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/USvideos.csv")
set.seed(1000)
corpus <- Corpus(VectorSource(list(videos$title)))
corpus[[1]][1]
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("les","que","vous","une","des","der","sur","get","told","gave","took","can","said","asked","will","even","spoke","got","really"))

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq = v)

d %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(word,freq),y=freq),fill=freq) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+ylab("Frequency")+xlab("Top 25 Words")+
  labs(x="Top 25 Words", y="Frequency")
  scale_fill_gradient(low="skyblue2", high="dodgerblue4")

title.length_num.vidoes <- us_videos %>%
  group_by(title_length) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=title_length, y=n,fill=n))+geom_bar(stat = 'identity')+
  labs(title="in the US", x="Title Length", y="Number of trending videos")+
  theme(legend.position = "none")+scale_fill_gradient(low="skyblue2", high="dodgerblue4")
  
d


wordcloud(d$word,d$freq,random.order = FALSE,rot.per = 0.6,scale = c(1.9,0.05),min.freq = 3,max.words = 700, colors = brewer.pal(8,"Dark2"))

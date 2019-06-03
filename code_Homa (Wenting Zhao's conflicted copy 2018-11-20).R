library(data.table)
library(dplyr)

library(DT)
library(lubridate)


library(ggplot2)
library(plotrix)
install.packages("wordcloud")
install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)
install.packages("data.table")
library("data.table")

library(tidytext)
library(stringr)
library(NLP)
install.packages("openNLP")
library(tm)
install.packages("syuzhet")
library(syuzhet)

install.packages("RSentiment")
install.packages("RSentiment")
install.packages("sentimentr")
library(sentimentr)
library(RSentiment)
library(rjson)
install.packages("RDocumentation")
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


wordcloud(d$word,d$freq,random.order = FALSE,rot.per = 0.1,scale = c(3,0.55),margin=0.1,min.freq = 3,max.words = 100, colors = brewer.pal(6,"Dark2"))


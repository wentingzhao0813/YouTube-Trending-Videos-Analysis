# URL to project: https://www.kaggle.com/datasnaek/youtube-new/home

library(tidyverse)
library(jsonlite)

can_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/CAvideos.csv")
ger_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/DEvideos.csv")
fr_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/FRvideos.csv")
gb_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/GBvideos.csv")
us_videos <- read.csv("~/Dropbox/MGTA-452 Group Project/Data/USvideos.csv")

can_category <- fromJSON('~/Dropbox/MGTA-452 Group Project/Data/CA_category_id.json')
ger_category <- fromJSON('~/Dropbox/MGTA-452 Group Project/Data/DE_category_id.json')
fr_category <- fromJSON('~/Dropbox/MGTA-452 Group Project/Data/FR_category_id.json')
gb_category <- fromJSON('~/Dropbox/MGTA-452 Group Project/Data/GB_category_id.json')
us_category <- fromJSON('~/Dropbox/MGTA-452 Group Project/Data/US_category_id.json')


glimpse(can_videos)
glimpse(ger_videos)
glimpse(fr_videos)
glimpse(gb_videos)
glimpse(us_videos)


glimpse(can_category)

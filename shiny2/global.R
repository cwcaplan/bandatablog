#setwd("C:/Users/Seth/Documents/bandatablog/shiny2/")

#pazz and jop
pjM <- read.csv("data/pazz and jop 1974-2014.csv", stringsAsFactors=F)
pjsM <- read.csv("data/pj singles 1979-2014.csv", stringsAsFactors=F)
##ALBUMS
#Billboard 200
wnoM <- read.csv("data/BB200 weekly number ones.csv", stringsAsFactors=F)
wnoM$date <- as.Date(wnoM$date, "%Y-%m-%d")
wnoMnd <- read.csv("data/BB200 no duplicates.csv", stringsAsFactors=F)
wnoMnd$date <- as.Date(wnoMnd$date, "%Y-%m-%d")
#Billboard Country
coM <- read.csv("data/country weekly number ones.csv", stringsAsFactors=F)
coM$date <- as.Date(coM$date, "%Y-%m-%d")
coMnd <- read.csv("data/country no duplicates.csv", stringsAsFactors=F)
coMnd$date <- as.Date(coMnd$date, "%Y-%m-%d")
#Billboard R and B
rbM <- read.csv("data/r and b weekly number ones.csv", stringsAsFactors=F)
rbM$date <- as.Date(rbM$date, "%Y-%m-%d")
rbMnd <- read.csv("data/r and b no duplicates.csv", stringsAsFactors=F)
rbMnd$date <- as.Date(rbMnd$date, "%Y-%m-%d")
##SINGLES
#Hot 100
h1M <- read.csv("data/hot 100 weekly number ones.csv", stringsAsFactors=F)
h1M$date <- as.Date(h1M$date, "%Y-%m-%d")
h1Mnd <- read.csv("data/hot 100 no duplicates.csv", stringsAsFactors=F)
h1Mnd$date <- as.Date(h1Mnd$date, "%Y-%m-%d")
#country singles
cosM <- read.csv("data/country singles weekly number ones.csv", stringsAsFactors=F)
cosM$date <- as.Date(cosM$date, "%Y-%m-%d")
cosMnd <- read.csv("data/country singles no duplicates.csv", stringsAsFactors=F)
cosMnd$date <- as.Date(cosMnd$date, "%Y-%m-%d")
#r and b singles
rbsM <- read.csv("data/r and b singles weekly number ones.csv", stringsAsFactors=F)
rbsM$date <- as.Date(rbsM$date, "%Y-%m-%d")
rbsMnd <- read.csv("data/r and b singles no duplicates.csv", stringsAsFactors=F)
rbsMnd$date <- as.Date(rbsMnd$date, "%Y-%m-%d")
#rock singles
mrsM <- read.csv("data/rock singles weekly number ones.csv", stringsAsFactors=F)
mrsM$date <- as.Date(mrsM$date, "%Y-%m-%d")
mrsMnd <- read.csv("data/rock singles no duplicates.csv", stringsAsFactors=F)
mrsMnd$date <- as.Date(mrsMnd$date, "%Y-%m-%d")
##YEAR STATS
yearStats <- read.csv("data/yearStats.csv", stringsAsFactors=F)
coYearStats <- read.csv("data/coYearStats.csv", stringsAsFactors=F)
rbYearStats <- read.csv("data/rbYearStats.csv", stringsAsFactors=F)
RanGarTayStats <- read.csv("data/RanGarTayStats.csv", stringsAsFactors=F)
h1YearStats <- read.csv("data/h1YearStats.csv", stringsAsFactors=F)
cosYearStats <- read.csv("data/cosYearStats.csv", stringsAsFactors=F)
rbsYearStats <- read.csv("data/rbsYearStats.csv", stringsAsFactors=F)
mrsYearStats <- read.csv("data/mrsYearStats.csv", stringsAsFactors=F)

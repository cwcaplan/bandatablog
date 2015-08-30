setwd("C:/Users/Seth/Documents/bandatablog")

#pazz and jop
pjM <- read.csv("data/pazz and jop 1974-2014.csv", stringsAsFactors=F)
pjsM <- read.csv("data/pj singles 1979-2014.csv", stingsAsFactors=F)
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

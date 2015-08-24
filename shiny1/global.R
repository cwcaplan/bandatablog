#setwd("C:/Users/Seth/Documents/bandatablog/data")
setwd("C:/Users/Seth/Documents/bandatablog/")

#pazz and jop
pjM <- read.csv("data/pazz and jop 1974-2014.csv", stringsAsFactors=F)
#Billboard 200
wnoM <- read.csv("data/BB200 weekly number ones.csv", stringsAsFactors=F)
wnoM$date <- as.Date(wnoM$date, "%Y-%m-%d")
wnoMnd <- read.csv("data/BB200 no duplicates.csv", stringsAsFactors=F)
wnoMnd$date <- as.Date(wnoMnd$date, "%Y-%m-%d")
#Billboard R and B
rbM <- read.csv("data/r and b weekly number ones.csv", stringsAsFactors=F)
rbM$date <- as.Date(rbM$date, "%Y-%m-%d")
rbMnd <- read.csv("data/r and b no duplicates.csv", stringsAsFactors=F)
rbMnd$date <- as.Date(rbMnd$date, "%Y-%m-%d")
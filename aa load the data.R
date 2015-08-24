setwd("C:/Users/Seth/Documents/bandatablog/data")

#pazz and jop
pjM <- read.csv("pazz and jop 1974-2014.csv", stringsAsFactors=F)
#Billboard 200
wnoM <- read.csv("BB200 weekly number ones.csv", stringsAsFactors=F)
wnoM$date <- as.Date(wnoM$date, "%Y-%m-%d")
wnoMnd <- read.csv("BB200 no duplicates.csv", stringsAsFactors=F)
wnoMnd$date <- as.Date(wnoMnd$date, "%Y-%m-%d")
#Billboard R and B
rbM <- read.csv("r and b weekly number ones.csv", stringsAsFactors=F)
rbM$date <- as.Date(rbM$date, "%Y-%m-%d")
rbMnd <- read.csv("r and b no duplicates.csv", stringsAsFactors=F)
rbMnd$date <- as.Date(rbMnd$date, "%Y-%m-%d")
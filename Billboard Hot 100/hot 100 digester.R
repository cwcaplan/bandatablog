setwd("C:/Users/Seth/Documents/bandatablog/Billboard Hot 100/weekly top songs")

## got data from
## http://www.billboard.com/archive/charts/1983/hot-100

# builds weekly number ones list
wno <- list()
h1M <- data.frame()
year <- "1982"
for (i in 1:33) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    wno[[i]] <- read.delim(paste("hot100-", year, ".txt", sep=""), 
                           header=F, stringsAsFactors=F)
    #add year to date and convert to date format
    wno[[i]][,1] <- paste(wno[[i]][,1], year)
    wno[[i]][,1] <- as.Date(wno[[i]][,1], "%B %d %Y")
    #replace blanks for multiple week winners
    for (j in 1: nrow(wno[[i]])) {
        if(nchar(wno[[i]][j+1,2])==0) {
            wno[[i]][j+1,2] <- wno[[i]][j,2]
            wno[[i]][j+1,3] <- wno[[i]][j,3]
        }
    }
    h1M <- rbind(h1M, wno[[i]])
}
#write.csv(h1M, "weekly number ones.csv", row.names=F)
names(h1M) <- c("date", "song", "artist")

weeksSong <- data.frame(table(h1M[,2]), stringsAsFactors=F)
weeksSong <- weeksSong[order(weeksSong$Freq, decreasing=T),]
weeksSong$Var1 <- as.character(weeksSong$Var1)

weeksArtist <- data.frame(table(h1M[,3]), stringsAsFactors=F)
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
weeksArtist$Var1 <- as.character(weeksArtist$Var1)

#adding album index to match pjM
h1M$index <- rep(0, nrow(h1M))

#a start but misses a lot from spelling, etc. 
#for instance (FutureSex):
#View(h1M[grep("Timberlake", h1M$artist),])
#View(pjM[grep("Timberlake", pjM$artist),])
for (i in 1:nrow(h1M)) {
    pjPick <- pjsM[pjsM$year==substring(h1M[i,1], 1, 4) |
                      pjsM$year==as.character(as.numeric(substring(h1M[i,1], 1, 4))-1), ]
    #matching, but only lower case
    hotsong <- tolower(unlist(strsplit(h1M[i,2], " \\("))[1])
    hotsong <- gsub("'", " ", hotsong)
    hotsong <- gsub("&", "and", hotsong)
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            song <- tolower(unlist(strsplit(pjPick[j,3], " \\("))[1])
            song <- unlist(strsplit(song, "/"))
            song <- gsub("'", " ", song)
            song <- gsub("&", "and", song)
            if(length(song)==1) {
                if(hotsong==song) {
                    h1M[i,4] <- pjPick[j,7]
                }
            } else if (length(song)==2) {
                if(hotsong==song[1]|hotsong==song[2]) {
                    h1M[i,4] <- pjPick[j,7]
                }
            }
            
        } 
    }
}

#looking at all the albums (no duplicates for mulitple weeks on chart)
h1Mnd <- h1M[!duplicated(h1M$song),]

#setwd("C:/Users/Seth/Documents/bandatablog/data")
#write.csv(h1M, "hot 100 weekly number ones.csv", row.names=F)
#write.csv(h1Mnd, "hot 100 no duplicates.csv", row.names=F)



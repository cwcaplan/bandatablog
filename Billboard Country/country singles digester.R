setwd("C:/Users/Seth/Documents/bandatablog/Billboard Country/weekly top songs")

## got data from
## http://www.billboard.com/archive/charts/1985/country-songs

# builds weekly number ones list
wno <- list()
cosM <- data.frame()
year <- "1984"
for (i in 1:31) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    wno[[i]] <- read.delim(paste("country singles ", year, ".txt", sep=""), 
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
    cosM <- rbind(cosM, wno[[i]])
}
#write.csv(cosM, "weekly number ones.csv", row.names=F)
names(cosM) <- c("date", "song", "artist")

weeksSong <- data.frame(table(cosM[,2]), stringsAsFactors=F)
weeksSong <- weeksSong[order(weeksSong$Freq, decreasing=T),]
weeksSong$Var1 <- as.character(weeksSong$Var1)

weeksArtist <- data.frame(table(cosM[,3]), stringsAsFactors=F)
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
weeksArtist$Var1 <- as.character(weeksArtist$Var1)

#adding album index to match pjM
cosM$index <- rep(0, nrow(cosM))

#a start but misses a lot from spelling, etc. 
#for instance (FutureSex):
#View(cosM[grep("Timberlake", cosM$artist),])
#View(pjM[grep("Timberlake", pjM$artist),])
for (i in 1:nrow(cosM)) {
    pjPick <- pjsM[pjsM$year==substring(cosM[i,1], 1, 4) |
                      pjsM$year==as.character(as.numeric(substring(cosM[i,1], 1, 4))-1), ]
    #matching, but only lower case
    hotsong <- tolower(unlist(strsplit(cosM[i,2], " \\("))[1])
    hotsong <- gsub("[`'-]", " ", hotsong)
    hotsong <- gsub("  +", " ", hotsong)
    hotsong <- gsub("&", "and", hotsong)
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            song <- tolower(unlist(strsplit(pjPick[j,3], " \\("))[1])
            song <- unlist(strsplit(song, "/"))
            song <- gsub("[`'-]", " ", song)
            song <- gsub("  +", " ", song)
            song <- gsub("&", "and", song)
            if(length(song)==1) {
                if(hotsong==song) {
                    cosM[i,4] <- pjPick[j,7]
                }
            } else if (length(song)==2) {
                if(hotsong==song[1]|hotsong==song[2]) {
                    cosM[i,4] <- pjPick[j,7]
                }
            }
            
        } 
    }
}

#looking at all the albums (no duplicates for mulitple weeks on chart)
cosMnd <- cosM[!duplicated(cosM$song),]

#setwd("C:/Users/Seth/Documents/bandatablog/data")
#write.csv(cosM, "country singles weekly number ones.csv", row.names=F)
#write.csv(cosMnd, "country singles no duplicates.csv", row.names=F)



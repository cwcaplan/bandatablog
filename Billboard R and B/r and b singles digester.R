setwd("C:/Users/Seth/Documents/bandatablog/Billboard R and B/weekly top songs")

## got data from
## http://www.billboard.com/archive/charts/1985/country-songs

# builds weekly number ones list
wno <- list()
rbsM <- data.frame()
year <- "1984"
for (i in 1:31) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    wno[[i]] <- read.delim(paste("r and b songs ", year, ".txt", sep=""), 
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
    rbsM <- rbind(rbsM, wno[[i]])
}
names(rbsM) <- c("date", "song", "artist")

# fix censorship
rbsM$song <- gsub("Ni\\*\\*as", "Niggas", rbsM$song)
rbsM$song <- gsub("F\\*\\*k", "Fuck", rbsM$song)

weeksSong <- data.frame(table(rbsM[,2]), stringsAsFactors=F)
weeksSong <- weeksSong[order(weeksSong$Freq, decreasing=T),]
weeksSong$Var1 <- as.character(weeksSong$Var1)

weeksArtist <- data.frame(table(rbsM[,3]), stringsAsFactors=F)
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
weeksArtist$Var1 <- as.character(weeksArtist$Var1)

#adding album index to match pjM
rbsM$index <- rep(0, nrow(rbsM))

#a start but misses a lot from spelling, etc. 
#for instance (FutureSex):
#View(rbsM[grep("Timberlake", rbsM$artist),])
#View(pjM[grep("Timberlake", pjM$artist),])
for (i in 1:nrow(rbsM)) {
    pjPick <- pjsM[pjsM$year==substring(rbsM[i,1], 1, 4) |
                      pjsM$year==as.character(as.numeric(substring(rbsM[i,1], 1, 4))-1), ]
    #matching, but only lower case
    hotsong <- tolower(unlist(strsplit(rbsM[i,2], " \\("))[1])
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
                    rbsM[i,4] <- pjPick[j,7]
                }
            } else if (length(song)==2) {
                if(hotsong==song[1]|hotsong==song[2]) {
                    rbsM[i,4] <- pjPick[j,7]
                }
            }
            
        } 
    }
}

#looking at all the albums (no duplicates for mulitple weeks on chart)
rbsMnd <- rbsM[!duplicated(rbsM$song),]

#setwd("C:/Users/Seth/Documents/bandatablog/data")
#write.csv(rbsM, "r and b singles weekly number ones.csv", row.names=F)
#write.csv(rbsMnd, "r and b singles no duplicates.csv", row.names=F)



setwd("C:/Users/Seth/Documents/bandatablog/Billboard 200/weekly top albums")

# builds weekly number ones list
wno <- list()
wnoM <- data.frame()
year <- "1982"
for (i in 1:33) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    wno[[i]] <- read.delim(paste("weekly top albums ", year, ".txt", sep=""), 
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
    wnoM <- rbind(wnoM, wno[[i]])
}
#write.csv(wnoM, "weekly number ones.csv", row.names=F)
names(wnoM) <- c("date", "album", "artist")

weeksAlbum <- data.frame(table(wnoM[,2]), stringsAsFactors=F)
weeksAlbum <- weeksAlbum[order(weeksAlbum$Freq, decreasing=T),]
weeksAlbum$Var1 <- as.character(weeksAlbum$Var1)

weeksArtist <- data.frame(table(wnoM[,3]), stringsAsFactors=F)
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
weeksArtist$Var1 <- as.character(weeksArtist$Var1)

#adding album index to match pjM
wnoM$index <- rep(0, nrow(wnoM))

#a start but only gets about 75 records
for (i in 1:nrow(wnoM[wnoM$date<"2008-01-01",])) {
    pjPick <- pjM[pjM$year==substring(wnoM[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(wnoM[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies
    #pjPick$album <- gsub("'", " ", pjPick$album)
    #pjPick$album <- gsub("[^A-Za-z0-9 ]", "", pjPick$album)
    #matching, but only lower case
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            if(tolower(wnoM[i,2])==tolower(pjPick[j,3])) {
                wnoM[i,4] <- pjPick[j,8]
            }
        } 
    }
}
# head(table(wnoM[!duplicated(wnoM$album),4]))

wnoMND <- wnoM[!duplicated(wnoM$album),]

out <- wnoMND[wnoMND$index==0,]

looker <- function(name, years) {
    pjPick <- pjM[pjM$year==years | 
                pjM$year==as.character(as.numeric(years)-1)
                  | pjM$year==as.character(as.numeric(years)+1), ]
    artists <- pjPick[grep(name, pjPick$artist, fixed=F),]
    albums <- pjPick[grep(name, pjPick$album, fixed=F),]
    picks <- rbind(artists, albums)
    picks
}

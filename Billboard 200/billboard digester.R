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

#looking at all the albums (no duplicates for mulitple weeks on chart)
wnoMnd <- wnoM[!duplicated(wnoM$album),]

#dealing with out (the ones that didn't match)
out <- wnoMnd[wnoMnd$index==0,]

## looks for a match in in Pazz and Jop for an artist or album within a three year window
looker <- function(name, years) {
    pjPick <- pjM[pjM$year==years | 
                      pjM$year==as.character(as.numeric(years)-1)
                  | pjM$year==as.character(as.numeric(years)+1), ]
    #get rid of regex bombs, split words, and expunge stop words
    names <- gsub("[\\(\\)\\*\\$\\+\\?]", "", name)
    names <- unlist(strsplit(names, " "))
    names <- names[!grepl("^[Aa]$|^[Oo]f$|^[Ii]n$|^[Tt]he$|
                          ^[Aa]t$|^[Ii]$|^[Ii]t$|^[Tt]o$|^&$", names)]
    #names <- names[!grepl["in"]]
    picks <- data.frame()
    for (i in 1:length(names)){
        artists <- pjPick[grep(tolower(names[i]), tolower(pjPick$artist)),]
        albums <- pjPick[grep(tolower(names[i]), tolower(pjPick$album)),]
        picks <- rbind(picks, artists, albums)
    }
    picks
}

checker <- function(chart) {
    options <- list()
    for (i in 1:nrow(chart)) {
        options[[i]] <- looker(paste(chart[i,2], chart[i,3]), 
                               substring(chart[i,1], 1, 4))
    }
    options
}

# gives you potential options to scroll through for out
outOpts <- checker(out)

#checked manually through
#outOpts[[13]]
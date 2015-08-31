setwd("C:/Users/Seth/Documents/bandatablog/Billboard R and B/weekly top albums")

## got data from
## http://www.billboard.com/archive/charts/1985/r-b-hip-hop-albums

# builds weekly number ones list
wno <- list()
rbM <- data.frame()
year <- "1984"
for (i in 1:31) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    wno[[i]] <- read.delim(paste("r and b ", year, ".txt", sep=""), 
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
    rbM <- rbind(rbM, wno[[i]])
}
#write.csv(rbM, "weekly number ones.csv", row.names=F)
names(rbM) <- c("date", "album", "artist")

weeksAlbum <- data.frame(table(rbM[,2]), stringsAsFactors=F)
weeksAlbum <- weeksAlbum[order(weeksAlbum$Freq, decreasing=T),]
weeksAlbum$Var1 <- as.character(weeksAlbum$Var1)

weeksArtist <- data.frame(table(rbM[,3]), stringsAsFactors=F)
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
weeksArtist$Var1 <- as.character(weeksArtist$Var1)

#adding album index to match pjM
rbM$index <- rep(0, nrow(rbM))

#a start but misses a lot from spelling, etc. 
#for instance (FutureSex):
#View(rbM[grep("Timberlake", rbM$artist),])
#View(pjM[grep("Timberlake", pjM$artist),])
for (i in 1:nrow(rbM)) {
    pjPick <- pjM[pjM$year==substring(rbM[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(rbM[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
    #pjPick$album <- gsub("'", " ", pjPick$album)
    #pjPick$album <- gsub("[^A-Za-z0-9 ]", "", pjPick$album)
    #matching, but only lower case
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            if(tolower(rbM[i,2])==tolower(pjPick[j,3])) {
                rbM[i,4] <- pjPick[j,8]
            }
        } 
    }
}
#View(rbM[rbM$index>0,])

#nrow(rbM[rbM$date>"2008-01-01" & rbM$date<"2014-01-01",])

#looking at all the albums (no duplicates for mulitple weeks on chart)
rbMnd <- rbM[!duplicated(rbM$album),]

#dealing with out (the ones that didn't match)
rbMout <- rbMnd[rbMnd$index==0,]

## looks for a match in in Pazz and Jop for an artist or album within a three year window
looker <- function(name, years) {
    pjPick <- pjM[pjM$year==years | 
                      pjM$year==as.character(as.numeric(years)-1)
                  | pjM$year==as.character(as.numeric(years)+1), ]
    #get rid of regex bombs, split words, and expunge stop words
    names <- gsub("[^A-Za-z0-9 ]", "", name)
    #names <- gsub("[\\(\\)\\*\\$\\+\\?'\\[\\]]", "", name)
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

# gives you potential options to scroll through for rbMout
checkOpts <- function(chart) {
    optsData <- data.frame()
    tempOpts <- data.frame()
    for (i in 1:nrow(chart)) {
        options <- looker(paste(chart[i,2], chart[i,3]), 
                          substring(chart[i,1], 1, 4))
        options <- cbind(rep(substring(chart[i,1], 1, 4), nrow(options)),
                         rep(paste(chart[i,3], chart[i,2], sep = ": "), nrow(options)),
                         options[,c(3,2,8)], stringsAsFactors=F)
        names(options) <- c("year", "wno", "PJalbum", "PJartist", "index")
        optsData <- rbind(optsData, options)
    }
    optsData
}
rbLookAtEm <- checkOpts(rbMout)

#double check with things like:
#View(pjM[grep("Prince", pjM$artist),])
#View(pjM[grep("True", pjM$album),])

#now look through and match manually (there must be another way, right?)
# uses the rbLookAtEm index (108 in the sample) to look up the right
# entry in rbM and then replaces the index column with whatever (436 in the sample)
#   sample:
#   rbM[rbM$album==unlist(strsplit(rbLookAtEm[108,2], ": "))[2],4] <- 436

#setwd("C:/Users/Seth/Documents/bandatablog/data")
#write.csv(rbM, "r and b weekly number ones.csv", row.names=F)
#write.csv(rbMnd, "r and b no duplicates.csv", row.names=F)


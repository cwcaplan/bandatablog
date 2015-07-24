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

#a start but misses a lot from spelling, etc. 
#for instance (FutureSex):
#View(wnoM[grep("Timberlake", wnoM$artist),])
#View(pjM[grep("Timberlake", pjM$artist),])
for (i in 1:nrow(wnoM)) {
    pjPick <- pjM[pjM$year==substring(wnoM[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(wnoM[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
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

#nrow(wnoM[wnoM$date>"2008-01-01" & wnoM$date<"2014-01-01",])

#run for a specific year
for (i in nrow(wnoM[wnoM$date<"2008-01-01",]):nrow(wnoM[wnoM$date<="2009-01-01",])) {
    pjPick <- pjM[pjM$year==substring(wnoM[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(wnoM[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
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

#run for a specific album
for (i in 1348) {
    pjPick <- pjM[pjM$year==substring(wnoM[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(wnoM[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
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

#this guy is outdated for checkOpts, but I'm keeping him around for a minute to be sure
checker <- function(chart) {
    options <- list()
    for (i in 1:nrow(chart)) {
        options[[i]] <- looker(paste(chart[i,2], chart[i,3]), 
                               substring(chart[i,1], 1, 4))
    }
    options
}
#outOpts <- checker(out)

# gives you potential options to scroll through for out
checkOpts <- function(chart) {
    optsData <- data.frame()
    tempOpts <- data.frame()
    for (i in 1:nrow(chart)) {
        options <- looker(paste(chart[i,2], chart[i,3]), 
                          substring(chart[i,1], 1, 4))
        options <- cbind(rep(substring(chart[i,1], 1, 4), nrow(options)),
                         rep(paste(chart[i,3], chart[i,2], sep = ": "), nrow(options)),
                         options[,c(3,2,8)])
        names(options) <- c("year", "wno", "PJalbum", "PJartist", "index")
        optsData <- rbind(optsData, options)
    }
    optsData
}
#lookAtEm <- checkOpts(out)

#double check with things like:
#View(pjM[grep("Prince", pjM$artist),])

#first match is Bruce at lookAtEm[108,]
#what do we do once we find a match?

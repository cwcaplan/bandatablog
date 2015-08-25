setwd("C:/Users/Seth/Documents/bandatablog/Billboard Country/weekly top albums")

## got data from
## http://www.billboard.com/archive/charts/1985/country-albums

# builds weekly number ones list
wno <- list()
coM <- data.frame()
year <- "1984"
for (i in 1:31) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    wno[[i]] <- read.delim(paste("country albums ", year, ".txt", sep=""), 
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
    coM <- rbind(coM, wno[[i]])
}
names(coM) <- c("date", "album", "artist")

weeksAlbum <- data.frame(table(coM[,2]), stringsAsFactors=F)
weeksAlbum <- weeksAlbum[order(weeksAlbum$Freq, decreasing=T),]
weeksAlbum$Var1 <- as.character(weeksAlbum$Var1)

weeksArtist <- data.frame(table(coM[,3]), stringsAsFactors=F)
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
weeksArtist$Var1 <- as.character(weeksArtist$Var1)

#adding album index to match pjM
coM$index <- rep(0, nrow(coM))

#a start but misses a lot from spelling, etc. 
#for instance (FutureSex):
#View(coM[grep("Timberlake", coM$artist),])
#View(pjM[grep("Timberlake", pjM$artist),])
for (i in 1:nrow(coM)) {
    pjPick <- pjM[pjM$year==substring(coM[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(coM[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
    #pjPick$album <- gsub("'", " ", pjPick$album)
    #pjPick$album <- gsub("[^A-Za-z0-9 ]", "", pjPick$album)
    #matching, but only lower case
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            if(tolower(coM[i,2])==tolower(pjPick[j,3])) {
                coM[i,4] <- pjPick[j,8]
            }
        } 
    }
}
#View(coM[coM$index>0,])

#nrow(coM[coM$date>"2008-01-01" & coM$date<"2014-01-01",])

#looking at all the albums (no duplicates for mulitple weeks on chart)
coMnd <- coM[!duplicated(coM$album),]

#dealing with out (the ones that didn't match)
coMout <- coMnd[coMnd$index==0,]

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

# gives you potential options to scroll through for coMout
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
coLookAtEm <- checkOpts(coMout)

#double check with things like:
#View(pjM[grep("Prince", pjM$artist),])
#View(pjM[grep("True", pjM$album),])

#now look through and match manually (there must be another way, right?)
# uses the rbLookAtEm index (108 in the sample) to look up the right
# entry in coM and then replaces the index column with whatever (436 in the sample)
#   sample:
#   coM[coM$album==unlist(strsplit(rbLookAtEm[108,2], ": "))[2],4] <- 436

#setwd("C:/Users/Seth/Documents/bandatablog/data")
#write.csv(coM, "country weekly number ones.csv", row.names=F)
#write.csv(coMnd, "country no duplicates.csv", row.names=F)

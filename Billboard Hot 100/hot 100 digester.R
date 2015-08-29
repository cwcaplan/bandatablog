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
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
    #pjPick$album <- gsub("'", " ", pjPick$album)
    #pjPick$album <- gsub("[^A-Za-z0-9 ]", "", pjPick$album)
    #matching, but only lower case
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            song <- tolower(unlist(strsplit(pjPick[j,3], " \\("))[1])
            song <- unlist(strsplit(song, "/"))
            if(length(song)==1) {
                if(tolower(h1M[i,2])==song) {
                    h1M[i,4] <- pjPick[j,7]
                }
            } else if (length(song)==2) {
                if(tolower(h1M[i,2])==song[1]|tolower(h1M[i,2])==song[2]) {
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

########BREAKS HERE

#nrow(h1M[h1M$date>"2008-01-01" & h1M$date<"2014-01-01",])

#run for a specific year
for (i in nrow(h1M[h1M$date<"2008-01-01",]):nrow(h1M[h1M$date<="2009-01-01",])) {
    pjPick <- pjM[pjM$year==substring(h1M[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(h1M[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
    #pjPick$album <- gsub("'", " ", pjPick$album)
    #pjPick$album <- gsub("[^A-Za-z0-9 ]", "", pjPick$album)
    #matching, but only lower case
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            if(tolower(h1M[i,2])==tolower(pjPick[j,3])) {
                h1M[i,4] <- pjPick[j,8]
            }
        } 
    }
}

#run for a specific album
for (i in 1348) {
    pjPick <- pjM[pjM$year==substring(h1M[i,1], 1, 4) |
                      pjM$year==as.character(as.numeric(substring(h1M[i,1], 1, 4))-1), ]
    #dealing with puncuation idiosynchrosies, DECIDED AGAINST
    #pjPick$album <- gsub("'", " ", pjPick$album)
    #pjPick$album <- gsub("[^A-Za-z0-9 ]", "", pjPick$album)
    #matching, but only lower case
    if (nrow(pjPick)>0) {
        for (j in 1:nrow(pjPick)) {
            if(tolower(h1M[i,2])==tolower(pjPick[j,3])) {
                h1M[i,4] <- pjPick[j,8]
            }
        } 
    }
}


#looking at all the albums (no duplicates for mulitple weeks on chart)
h1Mnd <- h1M[!duplicated(h1M$song),]

#dealing with out (the ones that didn't match)
out <- h1Mnd[h1Mnd$index==0,]

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
                         options[,c(3,2,8)], stringsAsFactors=F)
        names(options) <- c("year", "wno", "PJalbum", "PJartist", "index")
        optsData <- rbind(optsData, options)
    }
    optsData
}
lookAtEm <- checkOpts(out)

#double check with things like:
#View(pjM[grep("Prince", pjM$artist),])
#View(pjM[grep("True", pjM$album),])

#now look through and match manually (there must be another way, right?)
h1M[h1M$album==unlist(strsplit(lookAtEm[108,2], ": "))[2],4] <- 436
h1M[h1M$album==unlist(strsplit(lookAtEm[491,2], ": "))[2],4] <- 565
h1M[h1M$album==unlist(strsplit(lookAtEm[499,2], ": "))[2],4] <- 565
h1M[h1M$album=="Soundtrack: Batman",4] <- 584
h1M[h1M$album==unlist(strsplit(lookAtEm[701,2], ": "))[2],4] <- 582
h1M[h1M$album==unlist(strsplit(lookAtEm[2912,2], ": "))[2],4] <- 938
h1M[h1M$album==unlist(strsplit(lookAtEm[4850,2], ": "))[2],4] <- 1232
h1M[h1M$album==unlist(strsplit(lookAtEm[5229,2], ": "))[2],4] <- 1260
h1M[h1M$album==unlist(strsplit(lookAtEm[5750,2], ": "))[2],4] <- 1333




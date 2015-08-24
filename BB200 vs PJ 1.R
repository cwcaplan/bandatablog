#need to load billboard digester.R and p and j digester.R

#IDEAS

#percentage of chart toppers that made the critics list
length(wnoMnd$index[wnoMnd$index>0]) / length(wnoMnd$index)

#### % of charttoppers on crit list per year
year <- "1983"
yearPerc <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearPerc[i,1] <- year
    yearPerc[i,2] <- length(indices[indices>0]) / length(indices)
}
# should add a column with the album names
#plot(yearPerc$V1, yearPerc$V2, type="l")

### NUMBER of chart toppers on crit list per year
year <- "1983"
yearNum <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearNum[i,1] <- year
    yearNum[i,2] <- length(indices[indices>0])
}
# should add a column with the album names
#plot(yearNum$V1, yearNum$V2, type="l")

### COMBO
year <- "1983"
yearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearStats[i,1] <- year
    yearStats[i,2] <- length(indices[indices>0]) / length(indices)
    yearStats[i,3] <- length(indices[indices>0])
    yearStats[i,4] <- length(indices)
        ya <- wnoMnd[substring(wnoMnd$date, 1, 4)==year,]
        weeks <- rep(0, nrow(ya))
        ya <- cbind(ya, weeks)
        for (j in 1:nrow(ya)) {
            ya[j,5] <- nrow(wnoM[ya[j,2]==wnoM[,2] & ya[j,3]==wnoM[,3],])
        }
    yearStats[i, 5] <- round(mean(ya$weeks),1)
    yearStats[i, 6] <- max(ya$weeks)
}
names(yearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")
# should add a column with the album names
#plot(yearNum$V1, yearNum$V2, type="l")

### LOOKING AT WHICH ALBUMS WERE PICKED AND SNUBBED EACH YEAR
yearBoth <- function(year) {
    both <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index>0, ]
    albums <- character()
    for (i in 1:nrow(both)) {
        albums[i] <- paste(both[i,2], " - ", both[i,3], "\n", sep="")
    }
    albums
}

yearBB <- function(year) {
    BB <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index==0, ]
    albums <- character()
    for (i in 1:nrow(BB)) {
        albums[i] <- paste(BB[i,2], " - ", BB[i,3], "\n", sep="")
    }
    albums
}

### number of chart toppers per year
plot(yearStats$year, yearStats$total, type="l")

### avg weeks on the chart
year <- "1987"
ytc <- wnoM[substring(wnoM$date, 1, 4)==year,]
yam <- ytc[ya[2,2]==ytc[,2] & ya[2,3]==ytc[,3],]
yam <- wnoM[ya[2,2]==wnoM[,2] & ya[2,3]==wnoM[,3],]

ya <- wnoMnd[substring(wnoMnd$date, 1, 4)==year,]
weeks <- rep(0, nrow(ya))
ya <- cbind(ya, weeks)
for (j in 1:nrow(ya)) {
    ya[j,5] <- nrow(wnoM[ya[j,2]==wnoM[,2] & ya[j,3]==wnoM[,3],])
}

###weeks on top chart
#year <- "1986"
ya <- wnoM[substring(wnoM$date, 1, 4)==year,]
ya <- ya[!duplicated(ya$album),]
weeks <- rep(0, nrow(ya))
ya <- cbind(ya, weeks)
for (j in 1:nrow(ya)) {
    ya[j,5] <- nrow(wnoM[ya[j,2]==wnoM[,2] & ya[j,3]==wnoM[,3],])
}

weeksTop5 <- function(year, chart) {
    ya <- chart[substring(chart$date, 1, 4)==year,]
    ya <- ya[!duplicated(ya$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(chart[ya[j,2]==chart[,2] & ya[j,3]==chart[,3],])
    }
    ya <- ya[order(ya$weeks, decreasing=T),]
    top5 <- character()
    for (i in 1:5) {
        top5[i] <- paste(ya[i,5], " weeks:", "\n", "  ", ya[i,2], "\n", "   - ", ya[i,3], "\n", sep="")
    }
    top5
}

####
library(ggplot2)
g <- ggplot(data=yearStats, aes(x = year, y = total))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = critPicks+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=critPicks, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g


######
### OLD STUFF TO CLEAN UP
# gets too many, Sharon Jones for instance
##because part of the weeksAlbum[i] will show up in an erronous pjM$album
pjC <- data.frame()
for (i in 1:nrow(weeksAlbum)) {
    pjPick <- pjM[grep(weeksAlbum[i,1], pjM$album), ]
    pjC <- rbind(pjC, pjPick)
}

#line graph of #1's on the p and J list
plot(table(pjC$year), type="l")

#table of label scores on pazz and jop
View(table(pjM$label))

#sort by date
View(wnoM[wnoM$date>"2003-01-01",])

View(wnoM[grep("Prince", wnoM$artist),])

## WINNER, sort of , but doesn't get inexact matches
pjC <- data.frame()
for (i in 1:nrow(weeksAlbum)) {
    pjPick <- pjM[pjM$album==weeksAlbum[i,1], ]
    pjC <- rbind(pjC, pjPick)
}
pjC <- pjC[order(pjC$year),]

## sort of, but not quite
pjC <- data.frame()
for (i in 1:nrow(wnoM)) {
    pjPick <- pjM[pjM$artist==wnoM[i,3] & pjM$year==substring(wnoM[i,1], 1, 4), ]
    pjC <- rbind(pjC, pjPick)
}
pjC <- pjC[order(pjC$year),]

##
pjC <- data.frame()
artistlist <- character()
for (i in 1:nrow(wnoM)) {
    pjPick <- pjM[pjM$year==substring(wnoM[i,1], 1, 4) |
                pjM$year==as.character(as.numeric(substring(wnoM[i,1], 1, 4))-1), ]
    artist <- unlist(strsplit(wnoM[i,3], " "))
    artistbig <- artist[order(nchar(artist), decreasing=T)][1]
    #artistlist[i] <- artistbig
    
    #pjPick <- pjM[pjM$artist==wnoM[i,3] & pjM$year==substring(wnoM[i,1], 1, 4), ]
    #pjC <- rbind(pjC, pjPick)
}
pjC <- pjC[order(pjC$year),]




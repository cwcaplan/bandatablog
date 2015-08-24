#need to load billboard digester.R and p and j digester.R

#IDEAS

#percentage of chart toppers that made the critics list
length(rbMnd$index[rbMnd$index>0]) / length(rbMnd$index)

#the album names of charttoppers that made the critics list
View(rbMnd[rbMnd$index>0,])

# % of charttoppers on crit list per year
year <- "1984"
rbYearPerc <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- rbMnd$index[substring(rbMnd$date, 1, 4)==year]
    rbYearPerc[i,1] <- year
    rbYearPerc[i,2] <- length(indices[indices>0]) / length(indices)
}
# should add a column with the album names
#plot(rbYearPerc$V1, rbYearPerc$V2, type="l")
#### OR compare it to overal with 
#plot(yearPerc$V1, yearPerc$V2, type="l")
#lines(rbYearPerc$V1, rbYearPerc$V2, col="red")
#### (have to run BB200 vs PJ1.R first to get yearPerc)



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




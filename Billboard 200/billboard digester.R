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

weeksAlbum <- data.frame(table(wnoM[,2]))
weeksAlbum <- weeksAlbum[order(weeksAlbum$Freq, decreasing=T),]

weeksArtist <- data.frame(table(wnoM[,3]))
weeksArtist <- weeksArtist[order(weeksArtist$Freq, decreasing=T),]
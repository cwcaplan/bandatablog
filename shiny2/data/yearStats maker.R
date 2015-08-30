#setwd("C:/Users/Seth/Documents/bandatablog/shiny2/")

#BILLBOARD 200
year <- "1983"
yearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearStats[i,1] <- year
    #percentage of #1's on crit list
    yearStats[i,2] <- length(indices[indices>0]) / length(indices)
    #number of #1's on crit list
    yearStats[i,3] <- length(indices[indices>0])
    #number of #1's (per year)
    yearStats[i,4] <- length(indices)
    #weeks at number #1 (avg and max)
    ya <- wnoMnd[substring(wnoMnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(wnoM[ya[j,2]==wnoM[,2] & ya[j,3]==wnoM[,3],])
    }
    yearStats[i, 5] <- round(mean(ya$weeks),1)
    yearStats[i, 6] <- max(ya$weeks)
    #perc of weeks with a crit pick at #1
    yaD <- wnoM[substring(wnoM$date, 1, 4)==year,]
    yearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
names(yearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(yearStats, "data/yearStats.csv", row.names=F)

# COUNTRY
year <- "1983"
coYearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- coMnd$index[substring(coMnd$date, 1, 4)==year]
    coYearStats[i,1] <- year
    coYearStats[i,2] <- length(indices[indices>0]) / length(indices)
    coYearStats[i,3] <- length(indices[indices>0])
    coYearStats[i,4] <- length(indices)
    ya <- coMnd[substring(coMnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(coM[ya[j,2]==coM[,2] & ya[j,3]==coM[,3],])
    }
    coYearStats[i, 5] <- round(mean(ya$weeks),1)
    coYearStats[i, 6] <- max(ya$weeks)
    yaD <- coM[substring(coM$date, 1, 4)==year,]
    coYearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
coYearStats[1,2:7] <- NA
names(coYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(coYearStats, "data/coYearStats.csv", row.names=F)


# R&B
year <- "1983"
rbYearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- rbMnd$index[substring(rbMnd$date, 1, 4)==year]
    rbYearStats[i,1] <- year
    rbYearStats[i,2] <- length(indices[indices>0]) / length(indices)
    rbYearStats[i,3] <- length(indices[indices>0])
    rbYearStats[i,4] <- length(indices)
    ya <- rbMnd[substring(rbMnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(rbM[ya[j,2]==rbM[,2] & ya[j,3]==rbM[,3],])
    }
    rbYearStats[i, 5] <- round(mean(ya$weeks),1)
    rbYearStats[i, 6] <- max(ya$weeks)
    yaD <- rbM[substring(rbM$date, 1, 4)==year,]
    rbYearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
rbYearStats[1,2:7] <- NA
names(rbYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(rbYearStats, "data/rbYearStats.csv", row.names=F)

### RANDY VS. GARTH VS. TAYLOR
year <- "1983"
RanGarTayStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    coMy <- coM[substring(coM$date, 1, 4)==year,]
    RanGarTayStats[i,1] <- year
    RanGarTayStats[i,2] <- round(length(grep("Randy Travis", coMy$artist)) / 52, 3)
    RanGarTayStats[i,3] <- round(length(grep("Garth Brooks", coMy$artist)) / 52, 3)
    RanGarTayStats[i,4] <- round(length(grep("Taylor Swift", coMy$artist)) / 52, 3)
    RanGarTayStats[i,5] <- round(length(grep("Shania", coMy$artist)) / 52, 3)
}
RanGarTayStats[1,2:4] <- NA
names(RanGarTayStats) <- c("year", "Randy", "Garth", "Taylor", "Shania")
#write.csv(RanGarTayStats, "data/RanGarTayStats.csv", row.names=F)


##SINGLES
#HOT 100
year <- "1983"
h1YearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- h1Mnd$index[substring(h1Mnd$date, 1, 4)==year]
    h1YearStats[i,1] <- year
    #percentage of #1's on crit list
    h1YearStats[i,2] <- length(indices[indices>0]) / length(indices)
    #number of #1's on crit list
    h1YearStats[i,3] <- length(indices[indices>0])
    #number of #1's (per year)
    h1YearStats[i,4] <- length(indices)
    #weeks at number #1 (avg and max)
    ya <- h1Mnd[substring(h1Mnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(h1M[ya[j,2]==h1M[,2] & ya[j,3]==h1M[,3],])
    }
    h1YearStats[i, 5] <- round(mean(ya$weeks),1)
    h1YearStats[i, 6] <- max(ya$weeks)
    #perc of weeks with a crit pick at #1
    yaD <- h1M[substring(h1M$date, 1, 4)==year,]
    h1YearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
names(h1YearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(h1YearStats, "data/h1YearStats.csv", row.names=F)



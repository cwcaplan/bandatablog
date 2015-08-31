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

# COUNTRY SINGLES
year <- "1983"
cosYearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- cosMnd$index[substring(cosMnd$date, 1, 4)==year]
    cosYearStats[i,1] <- year
    #percentage of #1's on crit list
    cosYearStats[i,2] <- length(indices[indices>0]) / length(indices)
    #number of #1's on crit list
    cosYearStats[i,3] <- length(indices[indices>0])
    #number of #1's (per year)
    cosYearStats[i,4] <- length(indices)
    #weeks at number #1 (avg and max)
    ya <- cosMnd[substring(cosMnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(cosM[ya[j,2]==cosM[,2] & ya[j,3]==cosM[,3],])
    }
    cosYearStats[i, 5] <- round(mean(ya$weeks),1)
    cosYearStats[i, 6] <- max(ya$weeks)
    #perc of weeks with a crit pick at #1
    yaD <- cosM[substring(cosM$date, 1, 4)==year,]
    cosYearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
cosYearStats[1,2:7] <- NA
names(cosYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(cosYearStats, "data/cosYearStats.csv", row.names=F)

#R AND B SINGLES
year <- "1983"
rbsYearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- rbsMnd$index[substring(rbsMnd$date, 1, 4)==year]
    rbsYearStats[i,1] <- year
    #percentage of #1's on crit list
    rbsYearStats[i,2] <- length(indices[indices>0]) / length(indices)
    #number of #1's on crit list
    rbsYearStats[i,3] <- length(indices[indices>0])
    #number of #1's (per year)
    rbsYearStats[i,4] <- length(indices)
    #weeks at number #1 (avg and max)
    ya <- rbsMnd[substring(rbsMnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(rbsM[ya[j,2]==rbsM[,2] & ya[j,3]==rbsM[,3],])
    }
    rbsYearStats[i, 5] <- round(mean(ya$weeks),1)
    rbsYearStats[i, 6] <- max(ya$weeks)
    #perc of weeks with a crit pick at #1
    yaD <- rbsM[substring(rbsM$date, 1, 4)==year,]
    rbsYearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
rbsYearStats[1,2:7] <- NA
names(rbsYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(rbsYearStats, "data/rbsYearStats.csv", row.names=F)

# MAINSTREAM ROCK SINGLES
year <- "1983"
mrsYearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- mrsMnd$index[substring(mrsMnd$date, 1, 4)==year]
    mrsYearStats[i,1] <- year
    #percentage of #1's on crit list
    mrsYearStats[i,2] <- length(indices[indices>0]) / length(indices)
    #number of #1's on crit list
    mrsYearStats[i,3] <- length(indices[indices>0])
    #number of #1's (per year)
    mrsYearStats[i,4] <- length(indices)
    #weeks at number #1 (avg and max)
    ya <- mrsMnd[substring(mrsMnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(mrsM[ya[j,2]==mrsM[,2] & ya[j,3]==mrsM[,3],])
    }
    mrsYearStats[i, 5] <- round(mean(ya$weeks),1)
    mrsYearStats[i, 6] <- max(ya$weeks)
    #perc of weeks with a crit pick at #1
    yaD <- mrsM[substring(mrsM$date, 1, 4)==year,]
    mrsYearStats[i, 7] <- length(yaD$index[yaD$index>0])/length(yaD$index)
}
mrsYearStats[1,2:7] <- NA
names(mrsYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks", "percWeeks")
#write.csv(mrsYearStats, "data/mrsYearStats.csv", row.names=F)

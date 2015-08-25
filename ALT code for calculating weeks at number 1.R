###THREE OPTIONS

##puts an album only under the year it came out
##and then counts its TOTAL weeks at #1

#BILLBOARD 200
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
}
coYearStats[1,2:6] <- NA
names(coYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")


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
}
rbYearStats[1,2:6] <- NA
names(rbYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")


weeksTop5 <- function(year, chart) {
    if(chart==1) {
        chart <- wnoM
        chartnd <- wnoMnd
    } else if (chart==2) {
        chart <- coM
        chartnd <- coMnd
    } else if (chart==3) {
        chart <- rbM
        chartnd <- rbMnd
    }
    ya <- chartnd[substring(chartnd$date, 1, 4)==year,]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(chart[ya[j,2]==chart[,2] & ya[j,3]==chart[,3],])
    }
    ya <- ya[order(ya$weeks, decreasing=T),]
    top5 <- character()
    for (i in 1:5) {
        top5[i] <- paste(ya[i,5], " weeks:", "\n   ", ya[i,2], "\n   \t- ", ya[i,3], "\n", sep="")
    }
    top5
}

##gives Stats based on weeks at #1 in the given year
#######

#BILLBOARD 200
year <- "1983"
yearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearStats[i,1] <- year
    yearStats[i,2] <- length(indices[indices>0]) / length(indices)
    yearStats[i,3] <- length(indices[indices>0])
    yearStats[i,4] <- length(indices)
    yaD <- wnoM[substring(wnoM$date, 1, 4)==year,]
    ya <- yaD[!duplicated(yaD$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(yaD[ya[j,2]==yaD[,2] & ya[j,3]==yaD[,3],])
    }
    yearStats[i, 5] <- round(mean(ya$weeks),1)
    yearStats[i, 6] <- max(ya$weeks)
}
names(yearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")

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
    yaD <- coM[substring(coM$date, 1, 4)==year,]
    ya <- yaD[!duplicated(yaD$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(yaD[ya[j,2]==yaD[,2] & ya[j,3]==yaD[,3],])
    }
    coYearStats[i, 5] <- round(mean(ya$weeks),1)
    coYearStats[i, 6] <- max(ya$weeks)
}
coYearStats[1,2:6] <- NA
names(coYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")


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
    yaD <- rbM[substring(rbM$date, 1, 4)==year,]
    ya <- yaD[!duplicated(yaD$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(yaD[ya[j,2]==yaD[,2] & ya[j,3]==yaD[,3],])
    }
    rbYearStats[i, 5] <- round(mean(ya$weeks),1)
    rbYearStats[i, 6] <- max(ya$weeks)
}
rbYearStats[1,2:6] <- NA
names(rbYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")

weeksTop5 <- function(year, chart) {
    if(chart==1) {
        chart <- wnoM
    } else if (chart==2) {
        chart <- coM
    } else if (chart==3) {
        chart <- rbM
    }
    yaD <- chart[substring(chart$date, 1, 4)==year,]
    ya <- yaD[!duplicated(yaD$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(yaD[ya[j,2]==yaD[,2] & ya[j,3]==yaD[,3],])
    }
    ya <- ya[order(ya$weeks, decreasing=T),]
    top5 <- character()
    for (i in 1:5) {
        top5[i] <- paste(ya[i,5], " weeks:", "\n   ", ya[i,2], "\n   \t- ", ya[i,3], "\n", sep="")
    }
    top5
}


##gives Stats based on TOTAL weeks at #1 for all albums
##that hit #1 that year (as opposed to weeks at #1 in the given year)

#BILLBOARD 200
year <- "1983"
yearStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearStats[i,1] <- year
    yearStats[i,2] <- length(indices[indices>0]) / length(indices)
    yearStats[i,3] <- length(indices[indices>0])
    yearStats[i,4] <- length(indices)
    ya <- wnoM[substring(wnoM$date, 1, 4)==year,]
    ya <- ya[!duplicated(ya$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(wnoM[ya[j,2]==wnoM[,2] & ya[j,3]==wnoM[,3],])
    }
    yearStats[i, 5] <- round(mean(ya$weeks),1)
    yearStats[i, 6] <- max(ya$weeks)
}
names(yearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")

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
    ya <- coM[substring(coM$date, 1, 4)==year,]
    ya <- ya[!duplicated(ya$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(coM[ya[j,2]==coM[,2] & ya[j,3]==coM[,3],])
    }
    coYearStats[i, 5] <- round(mean(ya$weeks),1)
    coYearStats[i, 6] <- max(ya$weeks)
}
coYearStats[1,2:6] <- NA
names(coYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")


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
    ya <- rbM[substring(rbM$date, 1, 4)==year,]
    ya <- ya[!duplicated(ya$album),]
    weeks <- rep(0, nrow(ya))
    ya <- cbind(ya, weeks)
    for (j in 1:nrow(ya)) {
        ya[j,5] <- nrow(rbM[ya[j,2]==rbM[,2] & ya[j,3]==rbM[,3],])
    }
    rbYearStats[i, 5] <- round(mean(ya$weeks),1)
    rbYearStats[i, 6] <- max(ya$weeks)
}
rbYearStats[1,2:6] <- NA
names(rbYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")

ALTweeksTop5 <- function(year, chart) {
    if(chart==1) {
        chart <- wnoM
    } else if (chart==2) {
        chart <- coM
    } else if (chart==3) {
        chart <- rbM
    }
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
        top5[i] <- paste(ya[i,5], " weeks:", "\n   ", ya[i,2], "\n   \t- ", ya[i,3], "\n", sep="")
    }
    top5
}
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


year <- "1983"
RanGarTayStats <- data.frame()
for (i in 1:31) {
    year <- (as.character(as.numeric(year)+1))
    coMy <- coM[substring(coM$date, 1, 4)==year,]
    RanGarTayStats[i,1] <- year
    RanGarTayStats[i,2] <- round(length(grep("Randy Travis", coMy$artist)) / 52, 3)
    RanGarTayStats[i,3] <- round(length(grep("Garth Brooks", coMy$artist)) / 52, 3)
    RanGarTayStats[i,4] <- round(length(grep("Taylor Swift", coMy$artist)) / 52, 3)
}
RanGarTayStats[1,2:4] <- NA
names(RanGarTayStats) <- c("year", "Randy", "Garth", "Taylor")


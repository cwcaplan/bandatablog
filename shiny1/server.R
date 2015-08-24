#setwd("C:/Users/Seth/Documents/bandatablog/")

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

# R&B
year <- "1983"
rbYearStats <- data.frame()
for (i in 1:30) {
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
names(rbYearStats) <- c("year", "percentage", "critPicks", "total", "avgWeeks", "mostWeeks")


pickAYear <- function(year) {
    pickedYear <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index>0, ]
    pickedYear
}

yearBoth <- function(year) {
    both <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index>0, ]
    albums <- character()
    for (i in 1:nrow(both)) {
        albums[i] <- paste(both[i,2], "\n    - ", both[i,3], "\n", sep="")
    }
    albums
}

yearBB <- function(year) {
    BB <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index==0, ]
    albums <- character()
    for (i in 1:nrow(BB)) {
        albums[i] <- paste(BB[i,2], "\n    - ", BB[i,3], "\n", sep="")
    }
    albums
}

shinyServer(
    function(input, output) {
        #pickedYear <- reactive({pickAYear(input$year)})
        #output$albums <- renderDataTable({
        #    pickedYear()[,2:3]
        #    })
        winners <- reactive({yearBoth(input$year)})
        output$winners <- renderText({winners()})
        losers <- reactive({yearBB(input$year)})
        output$losers <- renderText({losers()})
        chartChoice <- reactive({input$chartChoice})
        output$chartChoice <- renderText({chartChoice()})
        output$percPlot <- renderPlot({
            plot(yearStats$year, yearStats$percentage, 
                 type="n", main="Percentage of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="Percentage of Albums", ylim=c(0,.5))
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$percentage, col="gray70")
                points(yearStats$year, yearStats$percentage)
            }
            if(any(chartChoice()==2)){
                abline(h=.2, col="firebrick3")
                #points(mrYearStats$year, mrYearStats$percentage, col="firebrick4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$percentage, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$percentage, col="darkorchid4")
            }
        })
        output$percPlotI <- renderPlot({
            plot(yearStats$year, yearStats$percentage, 
                 type="l", main="Percentage of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="Percentage of Albums", col="gray70")
            points(yearStats$year, yearStats$percentage, pch=21, 
                   col="darkorchid4", bg="darkorchid1", cex=.1*yearStats$total)
            points(yearStats$year, yearStats$percentage, pch=20,
                   col="firebrick4", cex=.35*yearStats$critPicks)
        })
        output$numPlotC <- renderPlot({
            plot(yearStats$year, yearStats$critPicks, 
                 type="l", main="Number of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="Number of Albums", ylim=c(0,8),
                 col="gray70")
            points(yearStats$year, yearStats$critPicks, col="midnightblue")
        })
        output$numPlotT <- renderPlot({
            plot(yearStats$year, yearStats$total, 
                 type="l", main="Number of Albums on Both Lists", ylim=c(0,43),
                 xlab="Year", ylab="Number of Chart-Toppers", col="gray70")
            points(yearStats$year, yearStats$total, col="midnightblue")
            lines(yearStats$year, yearStats$critPicks, col="darkred")
            points(yearStats$year, yearStats$critPicks)
            legend("topleft", lty=1, pch=1, col=c("midnightblue", "darkred"),
                   legend=c("Total", "on Critic's List"))
        })
        output$weeksPlot <- renderPlot({
            plot(yearStats$year, yearStats$mostWeeks, ylim=c(0,25), 
            main="Weeks At Number One", xlab="Year", ylab="Weeks")
            lines(yearStats$year, yearStats$avgWeeks, col="darkred")
            lines(yearStats$year, yearStats$mostWeeks, lty=2, col="grey")
            legend("topright", lty=c(2,1), col=c("grey", "darkred"), 
                   legend=c("Yearly High", "Average"))
        })
        output$weeksPlotAvg <- renderPlot({
            plot(yearStats$year, yearStats$avgWeeks, 
                 type="l", main="Average Weeks At Number One",
                 xlab="Year", ylab="Weeks", col="darkred")
            points(yearStats$year, yearStats$avgWeeks)
        })
    }
)


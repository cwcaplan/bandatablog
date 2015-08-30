#setwd("C:/Users/Seth/Documents/bandatablog/")

pickAYear <- function(year) {
    pickedYear <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index>0, ]
    pickedYear
}

yearBoth <- function(year, chart) {
    if(chart==1) {
        chart <- wnoMnd
    } else if (chart==2) {
        chart <- coMnd
    } else if (chart==3) {
        chart <- rbMnd
    }
    both <- chart[substring(chart$date, 1, 4)==year & chart$index>0, ]
    albums <- character()
    for (i in 1:nrow(both)) {
        albums[i] <- paste(both[i,2], "\n\t- ", both[i,3], "\n", sep="")
    }
    albums
}

yearBB <- function(year, chart) {
    if(chart==1) {
        chart <- wnoMnd
    } else if (chart==2) {
        chart <- coMnd
    } else if (chart==3) {
        chart <- rbMnd
    }
    BB <- chart[substring(chart$date, 1, 4)==year & chart$index==0, ]
    albums <- character()
    for (i in 1:nrow(BB)) {
        albums[i] <- paste(BB[i,2], "\n\t- ", BB[i,3], "\n", sep="")
    }
    albums
}

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

weeksAvg <- function(year, chart) {
    if(chart==1) {
        chart <- yearStats
    } else if (chart==2) {
        chart <- coYearStats
    } else if (chart==3) {
        chart <- rbYearStats
    }
    chart <- chart[chart$year==year,]
    avg <- chart$avgWeeks
    as.character(avg)
}


shinyServer(
    function(input, output) {
        #pickedYear <- reactive({pickAYear(input$year)})
        #output$albums <- renderDataTable({
        #    pickedYear()[,2:3]
        #    })
        par(lwd=4)
        avg <- reactive({weeksAvg(input$year, input$top5ChartPick)})
        output$avg <- renderText({paste("Avg Weeks At #1:", avg())})
        top5 <- reactive({weeksTop5(input$year, input$top5ChartPick)})
        output$top5 <- renderText({top5()})
        winners <- reactive({yearBoth(input$year, input$top5ChartPick)})
        output$winners <- renderText({winners()})
        losers <- reactive({yearBB(input$year, input$top5ChartPick)})
        output$losers <- renderText({losers()})
        chartChoice <- reactive({input$chartChoice})
        output$chartChoice <- renderText({chartChoice()})
        output$percPlot <- renderPlot({
            par(lwd=2, cex=1.05)
            plot(yearStats$year, yearStats$percentage, 
                 type="n", main="Percentage of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="", ylim=c(0,.5), yaxt="n")
            axis(2, at=c(.1, .2, .3, .4, .5), lab=c("10%", "20%", "30%", "40%", "50%"))
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$percentage, col="gray70")
                points(yearStats$year, yearStats$percentage, col="midnightblue")
                #lines(yearStats$year, yearStats$percWeeks, col="gray70", lty=2)
                #points(yearStats$year, yearStats$percWeeks, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(coYearStats$year, coYearStats$percentage, col="springgreen3")
                points(coYearStats$year, coYearStats$percentage, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$percentage, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$percentage, col="darkorchid4")
            }
        })
        output$percPlotI <- renderPlot({
            par(lwd=2, cex=1.05)
            plot(yearStats$year, yearStats$percentage, 
                 type="n", main="Percentage of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="", yaxt="n")
            axis(2, at=c(.1, .2, .3, .4, .5), lab=c("10%", "20%", "30%", "40%", "50%"))
            lines(yearStats$year, yearStats$percentage, col="gray70")
            points(yearStats$year, yearStats$percentage, pch=21, 
                   col="darkorchid4", bg="darkorchid1", cex=.1*yearStats$total)
            points(yearStats$year, yearStats$percentage, pch=20,
                   col="firebrick4", cex=.35*yearStats$critPicks)
            if(any(chartChoice()==1)){
                
            }
            if(any(chartChoice()==2)){
                
            }
            if(any(chartChoice()==3)){
                
            }
        })
        output$numPlotC <- renderPlot({
            par(lwd=2, cex=1.05)
            plot(yearStats$year, yearStats$critPicks, 
                 type="n", main="Number of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="Number of Albums", ylim=c(0,8))
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$critPicks, col="gray70")
                points(yearStats$year, yearStats$critPicks, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(coYearStats$year, coYearStats$critPicks, col="springgreen3")
                points(coYearStats$year, coYearStats$critPicks, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$critPicks, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$critPicks, col="darkorchid4")
            }
        })
        output$numPlotT <- renderPlot({
            par(lwd=2, cex=1.05)
            plot(yearStats$year, yearStats$total,
                 type="n", main="Number of Chart-Topping Albums in a Given Year", ylim=c(0,43),
                 xlab="Year", ylab="Number of Chart-Toppers")
            legend("topleft", lty=c(2,1), col="midnightblue",
                   legend=c("Total", "on Critic's List"))
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$total, col="gray70", lty=2)
                points(yearStats$year, yearStats$total, col="midnightblue")
                lines(yearStats$year, yearStats$critPicks, col="gray70")
                points(yearStats$year, yearStats$critPicks, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(coYearStats$year, coYearStats$total, col="springgreen3", lty=2)
                points(coYearStats$year, coYearStats$total, col="springgreen4")
                lines(coYearStats$year, coYearStats$critPicks, col="springgreen3")
                points(coYearStats$year, coYearStats$critPicks, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$total, col="darkorchid3", lty=2)
                points(rbYearStats$year, rbYearStats$total, col="darkorchid4")
                lines(rbYearStats$year, rbYearStats$critPicks, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$critPicks, col="darkorchid4")
            }
        })
        output$weeksPlotAvg <- renderPlot({
            par(lwd=2, cex=1.05)
            if(any(chartChoice()==2)) {
                plot(yearStats$year, yearStats$avgWeeks, 
                     type="n", main="Average Weeks At Number One",
                     xlab="Year", ylab="Weeks", ylim=c(0,19))
            } else {
                plot(yearStats$year, yearStats$avgWeeks, 
                     type="n", main="Average Weeks At Number One",
                     xlab="Year", ylab="Weeks", ylim=c(0,11))
            }
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$avgWeeks, col="gray70")
                points(yearStats$year, yearStats$avgWeeks, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(coYearStats$year, coYearStats$avgWeeks, col="springgreen3")
                points(coYearStats$year, coYearStats$avgWeeks, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$avgWeeks, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$avgWeeks, col="darkorchid4")
            }
        })
        output$weeksPlot <- renderPlot({
            par(lwd=2, cex=1.05)
            if(any(chartChoice()==2)) {
                plot(yearStats$year, yearStats$mostWeeks, ylim=c(0,50), 
                     main="Weeks At Number One", xlab="Year", ylab="Weeks")
            } else {
                plot(yearStats$year, yearStats$mostWeeks, ylim=c(0,30), 
                     main="Weeks At Number One", xlab="Year", ylab="Weeks")
            }
            
            legend("topright", lty=c(2,1), col=c("gray70", "gray70"), 
                   legend=c("Yearly High", "Average"))
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$mostWeeks, col="gray70", lty=2)
                points(yearStats$year, yearStats$mostWeeks, col="midnightblue")
                lines(yearStats$year, yearStats$avgWeeks, col="gray70")
                points(yearStats$year, yearStats$avgWeeks, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(coYearStats$year, coYearStats$mostWeeks, col="springgreen3", lty=2)
                points(coYearStats$year, coYearStats$mostWeeks, col="springgreen4")
                lines(coYearStats$year, coYearStats$avgWeeks, col="springgreen3")
                points(coYearStats$year, coYearStats$avgWeeks, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$mostWeeks, col="darkorchid3", lty=2)
                points(rbYearStats$year, rbYearStats$mostWeeks, col="darkorchid4")
                lines(rbYearStats$year, rbYearStats$avgWeeks, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$avgWeeks, col="darkorchid4")
            }
        })
        output$RanGarTayPlot <- renderPlot({
            if(any(chartChoice()==2)) {
                par(lwd=2, cex=1.05)
                plot(RanGarTayStats$year, RanGarTayStats$Garth,
                     type="n", main="Percentage of the Year that Randy Travis, Garth Brooks, or Taylor Swift were #1",
                     xlab="Year", ylab="", yaxt="n")
                axis(2, at=c(.15, .30, .45, .6, .75), lab=c("15%", "30%", "45%", "60%", "75%"))
                lines(RanGarTayStats$year, RanGarTayStats$Randy, col="springgreen3")
                points(RanGarTayStats$year, RanGarTayStats$Randy, col="springgreen4")
                lines(RanGarTayStats$year, RanGarTayStats$Garth, col="gray70")
                points(RanGarTayStats$year, RanGarTayStats$Garth, col="midnightblue")
                #lines(RanGarTayStats$year, RanGarTayStats$Shania, col="darkorchid3")
                #points(RanGarTayStats$year, RanGarTayStats$Shania, col="darkorchid4")
                lines(RanGarTayStats$year, RanGarTayStats$Taylor, col="firebrick3")
                points(RanGarTayStats$year, RanGarTayStats$Taylor, col="firebrick4")
                legend("topright", lty=c(1,1,1), col=c("springgreen4", "midnightblue", "firebrick4"), 
                       pch=21, legend=c("Randy", "Garth", "Taylor"))
                
            }
            
        })
    }
)


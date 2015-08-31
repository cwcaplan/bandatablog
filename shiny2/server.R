#setwd("C:/Users/Seth/Documents/bandatablog/")

pickAYear <- function(year) {
    pickedYear <- wnoMnd[substring(wnoMnd$date, 1, 4)==year & wnoMnd$index>0, ]
    pickedYear
}

weeksAvg <- function(year, chart) {
    if(chart==1) {
        chart <- yearStats
    } else if (chart==2) {
        chart <- h1YearStats
    } else if (chart==3) {
        chart <- coYearStats
    } else if (chart==4) {
        chart <- cosYearStats
    } else if (chart==5) {
        chart <- rbYearStats
    } else if (chart==6) {
        chart <- rbsYearStats
    } else if (chart==7) {
        chart <- mrsYearStats
    }
    chart <- chart[chart$year==year,]
    avg <- chart$avgWeeks
    as.character(avg)
}

weeksTop5 <- function(year, chart) {
    if(chart==1) {
        chart <- wnoM
        chartnd <- wnoMnd
    } else if (chart==2) {
        chart <- h1M
        chartnd <- h1Mnd
    } else if (chart==3) {
        chart <- coM
        chartnd <- coMnd
    } else if (chart==4) {
        chart <- cosM
        chartnd <- cosMnd
    } else if (chart==5) {
        chart <- rbM
        chartnd <- rbMnd
    } else if (chart==6) {
        chart <- rbsM
        chartnd <- rbsMnd
    } else if (chart==7) {
        chart <- mrsM
        chartnd <- mrsMnd
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

yearBoth <- function(year, chart) {
    if(chart==1) {
        chart <- wnoMnd
    } else if (chart==2) {
        chart <- h1Mnd
    } else if (chart==3) {
        chart <- coMnd
    } else if (chart==4) {
        chart <- cosMnd
    } else if (chart==5) {
        chart <- rbMnd
    } else if (chart==6) {
        chart <- rbsMnd
    } else if (chart==7) {
        chart <- mrsMnd
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
        chart <- h1Mnd
    } else if (chart==3) {
        chart <- coMnd
    } else if (chart==4) {
        chart <- cosMnd
    } else if (chart==5) {
        chart <- rbMnd
    } else if (chart==6) {
        chart <- rbsMnd
    } else if (chart==7) {
        chart <- mrsMnd
    }
    BB <- chart[substring(chart$date, 1, 4)==year & chart$index==0, ]
    albums <- character()
    for (i in 1:nrow(BB)) {
        albums[i] <- paste(BB[i,2], "\n\t- ", BB[i,3], "\n", sep="")
    }
    albums
}

lookAtChart <- function(year, chart) {
    if(chart==1) {
        chart <- wnoM
    } else if (chart==2) {
        chart <- h1M
    } else if (chart==3) {
        chart <- coM
    } else if (chart==4) {
        chart <- cosM
    } else if (chart==5) {
        chart <- rbM
    } else if (chart==6) {
        chart <- rbsM
    } else if (chart==7) {
        chart <- mrsM
    }
    yc <- chart[substr(chart$date,1,4)==year,]
    ycprint <- character()
    for (i in 1:nrow(yc)) {
        ycprint[i] <- paste(format(yc[i,1], format="%b %d"), "<br/>-- ", 
                         yc[i,3], ": ", 
                         yc[i,2], if(yc[i,4]>0){"*"}, "<br/>", sep="")
    }
    ycprint <- c("<br/>", "* indicates Critic's Pick<br/>", ycprint)
    ycprint
}

lookAtPJ <- function(year, chart) {
    if(chart==1|chart==3|chart==5) {
        chart <- pjM
        yc <- chart[chart$year==year,]
        ycprint <- character()
        for (i in 1:nrow(yc)) {
            ycprint[i] <- paste(yc[i,1], ". ", 
                                yc[i,2], ": ", 
                                yc[i,3], "<br/>", sep="")
        }
        ycprint <- c("<br/>", "Pazz & Jop ", year, " Albums List ", "<br/><br/>", ycprint)
        ycprint
    } else if (chart==2|chart==4|chart==6|chart==7) {
        chart <- pjsM
        yc <- chart[chart$year==year,]
        ycprint <- character()
        for (i in 1:nrow(yc)) {
            ycprint[i] <- paste(yc[i,1], ". ", 
                                yc[i,2], ": ", 
                                yc[i,3], "<br/>", sep="")
        }
        ycprint <- c("<br/>", "Pazz & Jop ", year, " Singles List ", "<br/><br/>", ycprint)
        ycprint
    } 
}

shinyServer(
    function(input, output) {
        #pickedYear <- reactive({pickAYear(input$year)})
        #output$albums <- renderDataTable({
        #    pickedYear()[,2:3]
        #    })
        albSing <- reactive({input$albSing})
        pickList <- reactive({input$pickList})
        top5ChartPick <- reactive({
            if(albSing()==1){
                input$pick1
            } else if (albSing()==2) {
                input$pick2
            } else if (albSing()==3) {
                input$pick3
            }
            
        })
        avg <- reactive({weeksAvg(input$year, top5ChartPick())})
        output$avg <- renderText({if(pickList()==1|pickList()==2){
            paste("Avg Weeks At #1:", avg())
        }})
        top5 <- reactive({weeksTop5(input$year, top5ChartPick())})
        output$top5 <- renderText({top5()})
        winners <- reactive({yearBoth(input$year, top5ChartPick())})
        output$winners <- renderText({winners()})
        losers <- reactive({yearBB(input$year, top5ChartPick())})
        output$losers <- renderText({losers()})
        bbList <- reactive({lookAtChart(input$year, top5ChartPick())})
        output$bbList <- renderUI({
            HTML(if(pickList()==2){paste(bbList(), collapse = '')})
            })
        pjList <- reactive({lookAtPJ(input$year, top5ChartPick())})
        output$pjList <- renderUI({
            HTML(if(pickList()==3){paste(pjList(), collapse = '')})
        })
        chartChoice <- reactive({input$chartChoice})
        output$chartChoice <- renderText({chartChoice()})
        output$percPlot <- renderPlot({
        if(albSing()==1){
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
        } else if (albSing()==2) {
            par(lwd=2, cex=1.05)
            plot(h1YearStats$year, h1YearStats$percentage, 
                 type="n", main="Percentage of Chart-Toppers on Critic's List",
                 xlab="Year", ylab="", ylim=c(0,.68), yaxt="n")
            axis(2, at=c(.1, .2, .3, .4, .5, .6), lab=c("10%", "20%", "30%", "40%", "50%", "60%"))
            if(any(chartChoice()==1)){
                lines(h1YearStats$year, h1YearStats$percentage, col="gray70")
                points(h1YearStats$year, h1YearStats$percentage, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(cosYearStats$year, cosYearStats$percentage, col="springgreen3")
                points(cosYearStats$year, cosYearStats$percentage, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbsYearStats$year, rbsYearStats$percentage, col="darkorchid3")
                points(rbsYearStats$year, rbsYearStats$percentage, col="darkorchid4")
            }
            if(any(chartChoice()==4)){
                lines(mrsYearStats$year, mrsYearStats$percentage, col="firebrick3")
                points(mrsYearStats$year, mrsYearStats$percentage, col="firebrick4")
            }
        } else if (albSing()==3) {
            par(lwd=2, cex=1.05, mfrow=c(1,2), mar=c(4,2,7,2))
            plot(yearStats$year, yearStats$percentage, 
                 type="n", main="Albums",
                 xlab="", ylab="", ylim=c(0,.62), yaxt="n")
            axis(2, at=c(.1, .2, .3, .4, .5, .6), lab=c("10%", "20%", "30%", "40%", "50%", "60%"))
            if(any(chartChoice()==1)){
                lines(yearStats$year, yearStats$percentage, col="gray70")
                points(yearStats$year, yearStats$percentage, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(coYearStats$year, coYearStats$percentage, col="springgreen3")
                points(coYearStats$year, coYearStats$percentage, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbYearStats$year, rbYearStats$percentage, col="darkorchid3")
                points(rbYearStats$year, rbYearStats$percentage, col="darkorchid4")
            }
            plot(h1YearStats$year, h1YearStats$percentage, 
                 type="n", main="Singles",
                 xlab="", ylab="", ylim=c(0,.62), yaxt="n")
            axis(2, at=c(.1, .2, .3, .4, .5, .6), lab=c("10%", "20%", "30%", "40%", "50%", "60%"))
            if(any(chartChoice()==1)){
                lines(h1YearStats$year, h1YearStats$percentage, col="gray70")
                points(h1YearStats$year, h1YearStats$percentage, col="midnightblue")
            }
            if(any(chartChoice()==2)){
                lines(cosYearStats$year, cosYearStats$percentage, col="springgreen3")
                points(cosYearStats$year, cosYearStats$percentage, col="springgreen4")
            }
            if(any(chartChoice()==3)){
                lines(rbsYearStats$year, rbsYearStats$percentage, col="darkorchid3")
                points(rbsYearStats$year, rbsYearStats$percentage, col="darkorchid4")
            }
            if(any(chartChoice()==4)){
                lines(mrsYearStats$year, mrsYearStats$percentage, col="firebrick3")
                points(mrsYearStats$year, mrsYearStats$percentage, col="firebrick4")
            }
            title("Percentage of Chart-Toppers on Critic's List", outer = T, 
                  line = -2, cex = 1.5)
        }
        })
        output$numPlotC <- renderPlot({
            if(albSing()==1){
                par(lwd=2, cex=1.05)
                plot(yearStats$year, yearStats$critPicks, 
                     type="n", main="Number of Chart-Toppers on Critic's List",
                     xlab="", ylab="Number of Albums", ylim=c(0,8))
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
            } else if (albSing()==2) {
                par(lwd=2, cex=1.05)
                plot(h1YearStats$year, h1YearStats$critPicks, 
                     type="n", main="Number of Chart-Toppers on Critic's List",
                     xlab="", ylab="Number of Singles", ylim=c(0,9))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$critPicks, col="gray70")
                    points(h1YearStats$year, h1YearStats$critPicks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$critPicks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$critPicks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick4")
                }
            } else if (albSing()==3) {
                par(lwd=2, cex=1.05, mfrow=c(1,2), mar=c(4,2,7,2))
                plot(yearStats$year, yearStats$critPicks, 
                     type="n", main="Albums",
                     xlab="", ylab="", ylim=c(0,8))
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
                plot(yearStats$year, yearStats$critPicks, 
                     type="n", main="Singles",
                     xlab="", ylab="", ylim=c(0,8))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$critPicks, col="gray70")
                    points(h1YearStats$year, h1YearStats$critPicks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$critPicks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$critPicks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick4")
                }
                title("Number of Chart-Toppers on Critic's List", outer = T, 
                      line = -2, cex = 1.5)
            }
        })
        output$numPlotT <- renderPlot({
            if(albSing()==1) {
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
            } else if (albSing()==2) {
                par(lwd=2, cex=1.05)
                if(any(chartChoice()==2)) {
                    plot(yearStats$year, yearStats$total,
                         type="n", main="Number of Chart-Topping Singles in a Given Year",
                         xlab="Year", ylab="Number of Chart-Toppers", ylim=c(0,50))
                } else {
                    plot(yearStats$year, yearStats$total,
                         type="n", main="Number of Chart-Topping Singles in a Given Year", ylim=c(0,43),
                         xlab="Year", ylab="Number of Chart-Toppers")
                }
                
                legend("topright", lty=c(2,1), col="midnightblue",
                       legend=c("Total", "on Critic's List"))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$total, col="gray70", lty=2)
                    points(h1YearStats$year, h1YearStats$total, col="midnightblue")
                    lines(h1YearStats$year, h1YearStats$critPicks, col="gray70")
                    points(h1YearStats$year, h1YearStats$critPicks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$total, col="springgreen3", lty=2)
                    points(cosYearStats$year, cosYearStats$total, col="springgreen4")
                    lines(cosYearStats$year, cosYearStats$critPicks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$critPicks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$total, col="darkorchid3", lty=2)
                    points(rbsYearStats$year, rbsYearStats$total, col="darkorchid4")
                    lines(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$total, col="firebrick3", lty=2)
                    points(mrsYearStats$year, mrsYearStats$total, col="firebrick4")
                    lines(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick4")
                }
            } else if (albSing()==3) {
                par(lwd=2, cex=1.05, mfrow=c(1,2), mar=c(2,2,7,2))
                plot(yearStats$year, yearStats$total,
                     type="n", main="Albums", ylim=c(0,43),
                     xlab="", ylab="")
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
                plot(yearStats$year, yearStats$total,
                     type="n", main="Singles", ylim=c(0,43),
                     xlab="", ylab="")
                legend("topleft", lty=c(2,1), col="midnightblue",
                       legend=c("Total", "on Critic's List"))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$total, col="gray70", lty=2)
                    points(h1YearStats$year, h1YearStats$total, col="midnightblue")
                    lines(h1YearStats$year, h1YearStats$critPicks, col="gray70")
                    points(h1YearStats$year, h1YearStats$critPicks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$total, col="springgreen3", lty=2)
                    points(cosYearStats$year, cosYearStats$total, col="springgreen4")
                    lines(cosYearStats$year, cosYearStats$critPicks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$critPicks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$total, col="darkorchid3", lty=2)
                    points(rbsYearStats$year, rbsYearStats$total, col="darkorchid4")
                    lines(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$critPicks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$total, col="firebrick3", lty=2)
                    points(mrsYearStats$year, mrsYearStats$total, col="firebrick4")
                    lines(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$critPicks, col="firebrick4")
                }
                title("Number of Chart-Toppers in a Given Year", outer = T, 
                      line = -2, cex = 1.5)
            }
        })
        output$weeksPlotAvg <- renderPlot({
            if(albSing()==1) {
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
            } else if (albSing()==2) {
                par(lwd=2, cex=1.05)
                plot(h1YearStats$year, h1YearStats$avgWeeks, 
                     type="n", main="Average Weeks At Number One",
                     xlab="Year", ylab="Weeks", ylim=c(0,12))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$avgWeeks, col="gray70")
                    points(h1YearStats$year, h1YearStats$avgWeeks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick4")
                }
            } else if (albSing()==3) {
                par(lwd=2, cex=1.05, mfrow=c(1,2), mar=c(2,2,7,2))
                if(any(chartChoice()==2)) {
                    plot(yearStats$year, yearStats$avgWeeks, 
                         type="n", main="Albums",
                         xlab="", ylab="", ylim=c(0,19))
                } else {
                    plot(yearStats$year, yearStats$avgWeeks, 
                         type="n", main="Albums",
                         xlab="", ylab="", ylim=c(0,11))
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
                if(any(chartChoice()==2)) {
                    plot(h1YearStats$year, h1YearStats$avgWeeks, 
                         type="n", main="Average Weeks At Number One",
                         xlab="Year", ylab="Weeks", ylim=c(0,19))
                } else {
                    plot(h1YearStats$year, h1YearStats$avgWeeks, 
                         type="n", main="Singles",
                         xlab="", ylab="", ylim=c(0,11))
                }
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$avgWeeks, col="gray70")
                    points(h1YearStats$year, h1YearStats$avgWeeks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick4")
                }
                title("Average Weeks At Number One", outer = T, 
                      line = -2, cex = 1.5)
            }
        })
        output$weeksPlot <- renderPlot({
            if(albSing()==1) {
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
            } else if (albSing()==2) {
                par(lwd=2, cex=1.05)
                plot(h1YearStats$year, h1YearStats$mostWeeks, ylim=c(0,30), 
                     main="Weeks At Number One", xlab="Year", ylab="Weeks")
                legend("topright", lty=c(2,1), col=c("gray70", "gray70"), 
                       legend=c("Yearly High", "Average"))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$mostWeeks, col="gray70", lty=2)
                    points(h1YearStats$year, h1YearStats$mostWeeks, col="midnightblue")
                    lines(h1YearStats$year, h1YearStats$avgWeeks, col="gray70")
                    points(h1YearStats$year, h1YearStats$avgWeeks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$mostWeeks, col="springgreen3", lty=2)
                    points(cosYearStats$year, cosYearStats$mostWeeks, col="springgreen4")
                    lines(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$mostWeeks, col="darkorchid3", lty=2)
                    points(rbsYearStats$year, rbsYearStats$mostWeeks, col="darkorchid4")
                    lines(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$mostWeeks, col="firebrick3", lty=2)
                    points(mrsYearStats$year, mrsYearStats$mostWeeks, col="firebrick4")
                    lines(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick4")
                }
            } else if (albSing()==3) {
                par(lwd=2, cex=1.05, mfrow=c(1,2), mar=c(2,2,7,2))
                if(any(chartChoice()==2)) {
                    plot(yearStats$year, yearStats$mostWeeks, ylim=c(0,50), 
                         main="Albums", xlab="", ylab="")
                } else {
                    plot(yearStats$year, yearStats$mostWeeks, ylim=c(0,30), 
                         main="Albums", xlab="", ylab="")
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
                if(any(chartChoice()==2)) {
                    plot(h1YearStats$year, h1YearStats$mostWeeks, ylim=c(0,50), 
                         main="Singles", xlab="", ylab="")
                } else {
                    plot(h1YearStats$year, h1YearStats$mostWeeks, ylim=c(0,30), 
                         main="Singles", xlab="", ylab="")
                }
                legend("topright", lty=c(2,1), col=c("gray70", "gray70"), 
                       legend=c("Yearly High", "Average"))
                if(any(chartChoice()==1)){
                    lines(h1YearStats$year, h1YearStats$mostWeeks, col="gray70", lty=2)
                    points(h1YearStats$year, h1YearStats$mostWeeks, col="midnightblue")
                    lines(h1YearStats$year, h1YearStats$avgWeeks, col="gray70")
                    points(h1YearStats$year, h1YearStats$avgWeeks, col="midnightblue")
                }
                if(any(chartChoice()==2)){
                    lines(cosYearStats$year, cosYearStats$mostWeeks, col="springgreen3", lty=2)
                    points(cosYearStats$year, cosYearStats$mostWeeks, col="springgreen4")
                    lines(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen3")
                    points(cosYearStats$year, cosYearStats$avgWeeks, col="springgreen4")
                }
                if(any(chartChoice()==3)){
                    lines(rbsYearStats$year, rbsYearStats$mostWeeks, col="darkorchid3", lty=2)
                    points(rbsYearStats$year, rbsYearStats$mostWeeks, col="darkorchid4")
                    lines(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid3")
                    points(rbsYearStats$year, rbsYearStats$avgWeeks, col="darkorchid4")
                }
                if(any(chartChoice()==4)){
                    lines(mrsYearStats$year, mrsYearStats$mostWeeks, col="firebrick3", lty=2)
                    points(mrsYearStats$year, mrsYearStats$mostWeeks, col="firebrick4")
                    lines(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick3")
                    points(mrsYearStats$year, mrsYearStats$avgWeeks, col="firebrick4")
                }
                title("Weeks At Number One", outer = T, 
                      line = -2, cex = 1.5)
            }
        })
        output$RanGarTayPlot <- renderPlot({
            if(any(chartChoice()==2) & albSing()==1) {
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
        output$topComm <- renderText({if(input$commentary==1){
            "Choose below whether you'd like to check out the Albums charts or the Singles
            charts or compare them side by side.  You can also select which charts (Country, Hip-Hop, etc.)
            you'd like to show on the graphs.  The left sidebar provides more detailed information 
            about each chart for a specific year of your choosing."
        }
        })
        output$percPlotComm <- renderText({if(input$commentary==1){
            "The original goal of this analysis was to compare what percentage of 
            chart-topping records also made it on the end-of-year critic's pick list.
            As you can see, on the Albums chart this number has gotten less variable over time, 
            but there also seems to be a lower percentage of agreement between the critics 
            and the public over time.  Interestingly, it seems to be the opposite with 
            Singles.  Hard to say exactly why that is, but my guess is that it has something
            to do with the trend towards digital sales and away from buying full albums."
            }
        })
        output$numPlotCComm <- renderText({if(input$commentary==1){
            "It's interesting to notice that it seems like, although critics in the 80's and
            early 90's selected a higher percentage of chart-toppers for their list, they
            actually selected a smaller number of albums.  The only explanation for this
            is shown in the graph below, which is where we'll discuss it further.  It's also 
            interesting that in the 80's critics picked very few chart-topping albums,
            but a ton of chart-topping singles.  For instance, in 1986 they picked only one
            #1 album (Janet Jackson's 'Control') for the list, but eight different #1 singles!
            Looking at the genre-specific charts, it seems Country gets no respect at all 
            (aside from Dwight Yoakam and Steve Earle during the 'Great Nashville Credibility 
            Crisis of 1986') until at least the late 90's.  R&B and Hip-Hop, on the other hand, is 
            surprisingly steady over the years, except of a precipitous drop in the late 90's.
            This is a bit baffling when consider that those were some of the 
            golden years for popular Hip-Hop.  However, when you look at the Singles charts
            individually, you notice that they were dominated by the likes of Usher, Deborah
            Cox and host of others who have never been critical darlings, while classic tracks
            like Wu-Tang's 'Triumph' or Outkast's 'Rosa Parks' never quite made it #1."
        }
        })
        output$numPlotTComm <- renderText({if(input$commentary==1){
            "Here is the answer to our riddle from the graph before.  How did the critics 
            give honors to 50% of 1984's chart-topping albums, while only pick two albums?
            Because only four albums hit #1 in 1984.  Similarly, only seven albums 
            reached #1 in 1987, with three of them landing on the critic's list."
        }
        })
    }
)


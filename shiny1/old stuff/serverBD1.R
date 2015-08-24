cityTemplate <- function(zip, seed, start, end=Sys.Date(), big=1){
    set.seed(seed)
    range <-seq(as.Date(start), as.Date(end), by="weeks")
    city <- data.frame(date = range,
                       zip.code = rep(zip, length(range)),
                       radio = rnorm(length(range), mean=big*runif(1, 10, 45), sd= big*runif(1, 2, 8)),
                       press = rnorm(length(range), mean=big*runif(1, -3, 0), sd= big*runif(1, 1, 3)),
                       album.sales = rnorm(length(range), mean=big*runif(1, 5, 20), sd= big*runif(1, 3, 6)),
                       merch.sales = rnorm(length(range), mean=big*runif(1, 10, 75), sd= big*runif(1, 55, 85)),
                       tickets = rep(NA, length(range)),
                       show.paid = rep(NA, length(range)),
                       show.merch = rep(NA, length(range))
    )
    # round to integers and make negatives into zeros
    city[,3:6] <- round(city[,3:6],0)
    city$radio <- ifelse(city$radio < 1, 0, city$radio)
    city$press <- ifelse(city$press < 1, 0, city$press)
    city$album.sales <- ifelse(city$album.sales < 1, 0, city$album.sales)
    city$merch.sales <- ifelse(city$merch.sales < 1, 0, city$merch.sales)
    #simulated shows
    shows <- sample(1:length(range), round(rnorm(1,length(range)/40,.5),0))
    if(length(shows)>0){
        city$tickets[shows] <- round(big*sample(100:200, length(shows)),0)
    }
    city$show.paid <- round(city$tickets*rnorm(1,9,1),0)-200
    city$show.merch <- round(city$tickets*rnorm(1,1.5,0.5),0)
    city
}

#simulate a few cities
Chicago <- cityTemplate("Chicago", 334, "2011-11-07", big=1.1)
NewYork <- cityTemplate("NewYork", 365, "2009-02-23", big=1.2)
StLouis <- cityTemplate("StLouis", 376, "2010-03-15", big=0.7)
DC <- cityTemplate("DC", 301, "2009-02-16", big=1.4)
Seattle <- cityTemplate("Seattle", 321, "2013-06-17")
big5 <- list(Chicago, NewYork, StLouis, DC, Seattle)

# THE MONEY FUNCTIONS
dfSum <- function(citylist, days) {
    zip <- as.character(unlist(lapply(citylist, function(x) x[1,2])))
    rad <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,3])))
    pre <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,4])))
    alb <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,5])))
    mer <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,6])))
    tix <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,7], na.rm=T)))
    spd <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,8], na.rm=T)))
    smr <- unlist(lapply(citylist, function(x) sum(x[x$date>Sys.Date()-days,9], na.rm=T)))
    sho <- unlist(lapply(citylist, function(x) 
        sum(complete.cases(x[x$date>Sys.Date()-days,7]))))
    dfSum <- data.frame(zip.code=zip, radio = rad, press = pre, album.sales = alb, 
                        merch.sales = mer, tickets = tix, 
                        show.paid = spd, show.merch = smr, shows = sho,
                        stringsAsFactors=F)
    for (i in 2:9){
        dfSum[,i] <- as.numeric(dfSum[,i])
    }
    dfSum
}

cityBar <- function(dfSum, stat, days) {
    statname <- character(1)
    if(stat==1){statname <- "City"
    } else if(stat==2){statname <- "Radio Spins"
    } else if(stat==3){statname <- "Press Features"
    } else if(stat==4){statname <- "Albums Sold"
    } else if(stat==5){statname <- "Merchandise Sold, in USD"
    } else if(stat==6){statname <- "Tickets Sold"
    } else if(stat==7){statname <- "Paid for Shows, in USD"
    } else if(stat==8){statname <- "Merch Sold at Shows, in USD"
    } else if(stat==9){statname <- "Shows Played"
    }
    if(days==14){timewords <- "Two Weeks"
    } else if(days==28){timewords <- "One Month"
    } else if(days==42){timewords <- "Six Weeks"
    } else if(43 <= days && days <= 70){timewords <- "Two Months"
    } else if(71 <= days && days <= 105){timewords <- "Three Months"
    } else if(106 <= days && days <= 135){timewords <- "Four Months"
    } else if(136 <= days && days <= 165){timewords <- "Five Months"
    } else if(166 <= days && days <= 239){timewords <- "Six Months"
    } else if(240 <= days && days <= 329){timewords <- "Nine Months"
    } else if(330 <= days && days <= 450){timewords <- "One Year"
    } else if(450 <= days && days <= 629){timewords <- "18 Months"
    } else if(630 <= days && days <= 910){timewords <- "Two Years"
    } else if(911 <= days && days <= 1250){timewords <- "Three Years"
    } else if(1251 <= days && days <= 1500){timewords <- "Four Years"                                     
    }
    AVG <- round(mean(dfSum[,stat]), 1)
    qplot(zip.code, dfSum[,stat], data=dfSum, geom="bar", stat="identity") +
        geom_hline(yintercept=AVG, size=1.2, colour="red") +
        geom_line(aes(color=as.character(AVG))) +
        #geom_text(aes(x= length(zip.code)+1, y = AVG,label = paste("AVG:", AVG, " "), 
        #              vjust= -1, hjust = 1, color="AVG")) +
        xlab("") +
        ylab(statname) +
        geom_text(aes(label = dfSum[,stat], y=dfSum[1,stat]*.1, vjust=0), size = 5) +
        ggtitle(paste(statname, "For The Past", timewords, sep=" ")) +
        scale_colour_manual(name="Average", values="red")
}

# THE SERVER
shinyServer(
    function(input, output) {
        #output$oid2 <- renderPrint({input$City})
        statr <- reactive({input$stat})
        output$statnum <- renderPrint({input$stat})
        output$cityBarPlot <- renderPlot({
            dfSumP <- dfSum(big5, as.numeric(input$days))
            cityBar(dfSumP[c(input$City), ], as.numeric(input$stat), as.numeric(input$days))
            })
        #output$odate <- renderPrint({input$date})
    }
)

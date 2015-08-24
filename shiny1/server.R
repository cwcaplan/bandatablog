#setwd("C:/Users/Seth/Documents/bandatablog/")

#IDEAS

#percentage of chart toppers that made the critics list
#length(wnoMnd$index[wnoMnd$index>0]) / length(wnoMnd$index)

# % of charttoppers on crit list per year
year <- "1983"
yearPerc <- data.frame()
for (i in 1:35) {
    year <- (as.character(as.numeric(year)+1))
    indices <- wnoMnd$index[substring(wnoMnd$date, 1, 4)==year]
    yearPerc[i,1] <- year
    yearPerc[i,2] <- length(indices[indices>0]) / length(indices)
}
# should add a column with the album names
#plot(yearPerc$V1, yearPerc$V2, type="l")

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
        output$percPlot <- renderPlot({
            plot(yearPerc$V1, yearPerc$V2, type="l")
        })
        #output$sentence <- renderText({paste(input$words, "...", sep = "")})
    }
)


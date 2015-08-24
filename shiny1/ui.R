shinyUI(pageWithSidebar(
    headerPanel(""),
    sidebarPanel(
        checkboxGroupInput('chartChoice', "Compare Critic's List To:", 
                           c("Billboard 200"="1", "Mainstream Rock"="2", "R&B/Hip-Hop" = "3"),
                           selected="1"),
        #verbatimTextOutput("chartChoice"),
        sliderInput('year', 'Look At A Specific Year', value = 2000, 
                    min = 1984, max = 2015, step = 1,),
        h4('albums on both lists:'),
        verbatimTextOutput("winners"),
        h4('chart-toppers that the critics snubbed:'),
        verbatimTextOutput("losers")
    ),
    mainPanel(
        h3('Sales vs. Critics, Since 1984'),
        plotOutput("percPlot"),
        plotOutput("numPlotC"),
        plotOutput("numPlotT"),
        plotOutput("weeksPlotAvg"),
        plotOutput("weeksPlot")
        #dataTableOutput("albums")
    )
))